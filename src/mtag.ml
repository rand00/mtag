open Rresult
open Bos

let sp = Printf.sprintf

let log fmt =
  let kpp k fmt =
    let k fmt = k (Format.flush_str_formatter ()) in
    Format.kfprintf k Format.str_formatter fmt
  in
  kpp (Format.printf "mtag: %s\n") fmt

let warn fmt =
  let kpp k fmt =
    let k fmt = k (Format.flush_str_formatter ()) in
    Format.kfprintf k Format.str_formatter fmt
  in
  kpp (Format.eprintf "mtag: warning: %s\n") fmt

module T = struct 

  type tag = [
    | `Tag of Fpath.t (*relative path*)
  ] 
  [@@deriving ord,eq,show]

  type t = tag
  [@@deriving ord,eq,show]

  type expr = [
    | `Not of expr
    | `Within of tag
    | tag
  ]
  (*< e.g. syntax: !type/foo,my/bar*)
  
  type query = expr list
  (*< Note: semantically this is an expr Set.t of constraints that all need to be true*)

end

include T

module Set = CCSet.Make(T)

module Member = struct 

  type t = {
    path : Fpath.t;
    symlink_path : Fpath.t;
  }[@@deriving eq,ord,show]

  module PathSet = CCSet.Make(Fpath)

end

let tags_dirname = "_mtags"

let to_absolute ~root (`Tag tag_path) =
  assert (Fpath.is_rel tag_path);
  (*< Note: needed because of semantics of the '//' operator*)
  let tag =
    Fpath.(root / tags_dirname // tag_path)
    |> Fpath.normalize
  in
  assert (Fpath.is_rooted ~root tag);
  (*< Note: In case user supplied path with '..' inside*)
  `Tag tag

let from_absolute ~root (`Tag tag_path) =
  let root = Fpath.(root / tags_dirname) in
  let tag_path =
    Fpath.(relativize ~root tag_path)
    |> CCOption.get_or ~default:tag_path
    |> Fpath.rem_empty_seg
  in
  `Tag tag_path

let relative_fpath_of_string str =
  ("./" ^ str) (*< Note: in case user passes an absolute path*)
  |> Fpath.of_string
  |> R.failwith_error_msg
  |> Fpath.normalize

let open_tag v = (v : tag :> [>tag])

(*goto should all sub-parsers be in result monad instead, to get error msgs? *)
let rec parse_not str : expr option =
  CCString.chop_prefix ~pre:"!" str
  |> CCOption.map (fun str ->
    `Not (parse_expr str)
  )

and parse_tag str : tag =
  let fpath = relative_fpath_of_string str in
  `Tag fpath

and parse_subdir str =
  CCString.chop_prefix ~pre:">" str
  |> CCOption.map (fun str ->
    `Within (parse_tag str)
  )

and parse_expr str : expr =
  parse_not str
  |> CCOption.get_lazy (fun () ->
    parse_subdir str
    |> CCOption.get_lazy (fun () ->
      let v = parse_tag str in
      (v : tag :> expr)
    )
  )

let parse_query_string tag_str : query =
  String.split_on_char ',' tag_str
  |> List.map parse_expr

let parse_string tag_str : tag list =
  String.split_on_char ',' tag_str
  |> List.map parse_tag

let resolve_tag_symlink symlink_path =
  match symlink_path |> OS.Path.symlink_target with
  | Error err -> None 
  | Ok target ->
    let target = Fpath.(parent symlink_path // target |> normalize) in
    Some target

let members ~root ~recurse tag =
  let try_resolve_symlink symlink_path =
    match resolve_tag_symlink symlink_path with
    | None -> []
    | Some normalized_target ->
      [{ Member.path = normalized_target; symlink_path }]
  in
  let rec aux tag_path =
    tag_path
    |> OS.Dir.contents ~dotfiles:true ~rel:false
    |> R.failwith_error_msg
    |> CCList.flat_map (fun content ->
      if
        recurse
        && OS.Path.symlink_target content |> Result.is_error 
        && OS.Dir.exists content |> R.failwith_error_msg
      then
        aux content
      else
        try_resolve_symlink content
    )
  in
  let (`Tag tag_path) = tag |> to_absolute ~root in
  aux tag_path

let member_paths ~root ~recurse tag =
  tag
  |> members ~root ~recurse
  |> List.map (fun {Member.path; _} -> path)
  |> Member.PathSet.of_list

(*goto make Expr module + put helpers there*)
let rec minimize_expr : expr -> expr = function
  | `Tag _ as v -> v
  | `Not (`Not v) -> minimize_expr v
  | `Not v -> `Not (minimize_expr v)
  | `Within _const as v -> v

let member_paths_of_expr ~root expr =
  let aux = function
    | `Tag _ as tag ->
      `Members (member_paths ~root ~recurse:false tag)
    | `Not (`Tag _ as tag) ->
      `Not_members (member_paths ~root ~recurse:false tag)
    | `Not (`Within tag) ->
      `Not_members (member_paths ~root ~recurse:true tag)
    | `Within tag ->
      `Members (member_paths ~root ~recurse:true tag)
    | `Not (`Not _) -> failwith "Tag: Not-expression was not minimized"
  in
  expr |> minimize_expr |> aux

let tags_to_string tags =
  tags 
  |> Set.to_list
  |> CCList.sort (fun (`Tag t) (`Tag t') ->
    CCString.compare (Fpath.to_string t) (Fpath.to_string t')
  )
  |> CCList.map (fun (`Tag tag_path) -> tag_path)
  |> CCList.to_string ~sep:"," Fpath.to_string

module Path = struct 

  (*goto 
    * for safety, work on the following types in procs;
      * type absolute = [ `Absolute of Fpath.t ]
      * type t = [ absolute | `Relative of Fpath.t ]
  *)

  let rec resolve_and_normalize path =
    OS.Path.symlink_target path
    |> CCResult.map (fun target ->
      let target = Fpath.(parent path // target) in
      resolve_and_normalize target
    )
    |> CCResult.get_or ~default:path
    |> Fpath.normalize

  let to_absolute ~cwd path = Fpath.(cwd // path |> normalize)
  
  let to_absolute_via_fs path =
    let path_str = Fpath.to_string path in
    let dir, file =
      if Sys.is_directory path_str then path, None
      else 
        let d, f = path |> Fpath.split_base in
        d, Some f
    in
    begin
      OS.Dir.with_current dir OS.Dir.current () |> R.join
      >>| fun dir ->
      let apath = match file with
        | None -> dir 
        | Some file -> Fpath.(dir // file)
      in
      apath
    end
    |> R.failwith_error_msg

  (*goto path should be `Absolute *)
  let verify ~debug ~root path =
    begin
      OS.File.exists path >>= fun p_file_exists ->
      OS.Dir.exists path >>| fun p_dir_exists ->
      let p_exists = p_file_exists || p_dir_exists in
      if debug then log "verify path: path exists = %b" p_exists;
      let p_is_inside_root =
        Fpath.is_rooted ~root path
      in
      if debug then log "verify path: inside root = %b" p_is_inside_root;
      let r = p_exists && p_is_inside_root in
      if not r then begin
        warn "path '%a' didn't verify: exists=%b, is_inside_mtag_root=%b"
          Fpath.pp path
          p_exists p_is_inside_root
      end;
      r
    end
    |> R.failwith_error_msg

  (*goto path should be `Absolute *)
  let relativize_to_root ~root path =
    match Fpath.relativize ~root path with
    | Some path -> path
    | None -> failwith (
      sp "mtag: Couldn't relativize '%s' to niseq-root"
        (Fpath.to_string path)
    )

  let hash path = 
    Digestif.(
      path
      |> Fpath.to_string
      |> SHA256.digest_string
      |> SHA256.to_hex
    )

end

module type Run = sig

  val tag : 
    dryrun:bool ->
    root:Fpath.t ->
    cwd:Fpath.t ->
    tags:(t list) ->
    paths:(Fpath.t list) ->
    unit

  val query :
    dryrun:bool ->
    root:Fpath.t ->
    query:query ->
    Member.PathSet.t 

  val rm :
    dryrun:bool ->
    root:Fpath.t ->
    cwd:Fpath.t ->
    tags:(t list) ->
    paths:(Fpath.t list) ->
    unit

  val tags_intersection :
    root:Fpath.t ->
    cwd:Fpath.t ->
    paths:(Fpath.t list) ->
    Set.t 

  val tags_union :
    root:Fpath.t ->
    cwd:Fpath.t ->
    paths:(Fpath.t list) ->
    Set.t

  val replace_paths :
    dryrun:bool ->
    root:Fpath.t ->
    Fpath.t ->
    Fpath.t ->
    unit
  
end

module Run : Run = struct

  (*goto make into common module - reusing this between scripts now as boilerplate*)
  module Aux = struct

    let run_exn cmd ~dryrun =
      if dryrun then begin
        Cmd.pp Format.std_formatter cmd;
        Format.print_newline ();
      end
      else 
        OS.Cmd.run_io ~err:OS.Cmd.err_stderr cmd OS.Cmd.in_null
        |> OS.Cmd.to_stdout
        |> R.failwith_error_msg

    let run_string_exn cmd ~dryrun =
      if dryrun then begin
        Cmd.pp Format.std_formatter cmd;
        Format.print_newline ();
        ""
      end
      else 
        OS.Cmd.run_io ~err:OS.Cmd.err_stderr cmd OS.Cmd.in_null
        |> OS.Cmd.to_string
        |> R.failwith_error_msg

  end

  let run ~dryrun = Aux.run_exn ~dryrun 

  let tag_path_once ~dryrun ~root ~path (`Tag tag_path_relative as tag) =
    let (`Tag tag_path) = to_absolute ~root tag in
    let tag_path_file =
      let path_hash =
        path
        (* |> Path.relativize_to_root ~root *)
        (*< Note: already relativized (also a thing that could be expressed in types)*)
        |> Path.hash
      in
      Fpath.(tag_path // v path_hash)
    in
    let path = 
      let p_jump_back =
        Fpath.segs tag_path_relative
        |> List.filter (fun seg -> not (seg = ""))
        |> List.map (fun _ -> "..")
        |> CCList.cons ".."
        |> String.concat "/"
        |> Fpath.v
      in
      Fpath.(p_jump_back // path)
    in
    begin
      if dryrun then begin
        log "DRYRUN: tag_path_once: create dir: %s\n" (Fpath.to_string tag_path);
        log "DRYRUN: tag_path_once: create symlink: %s -> %s\n"
          (Fpath.to_string tag_path_file)
          (Fpath.to_string path);
        Ok ()
      end else 
        OS.Dir.create ~path:true tag_path >>= fun _ ->
        tag_path_file |> OS.Path.symlink ~force:true ~target:path 
    end
  
  let tag_path ~dryrun ~root ~tags path =
    tags |> List.iter (fun tag -> 
      tag |> tag_path_once ~dryrun ~root ~path |> R.failwith_error_msg
    )

  (*exposed*)
  let tag ~dryrun ~root ~cwd ~tags ~paths =
    let normalized_paths =
      paths
      |> List.map Path.resolve_and_normalize
      |> List.map (Path.to_absolute ~cwd)
    in
    if not (normalized_paths |> List.for_all (Path.verify ~debug:dryrun ~root)) then
      failwith (sp "mtag: Normalized paths didn't verify");
    normalized_paths
    |> List.map (Path.relativize_to_root ~root)
    |> List.iter (tag_path ~dryrun ~root ~tags)

  let member_set_intersection acc_set set =
    match acc_set with
    | None -> Some set 
    | Some acc_set -> Some (Member.PathSet.inter acc_set set)

  let member_set_union acc_set set =
    match acc_set with
    | None -> Some set 
    | Some acc_set -> Some (Member.PathSet.union acc_set set)

  (*goto put in result monad*)
  let join_member_exprs exprs =
    let members, not_members = exprs |> CCList.partition_filter_map (function
      | `Members v -> `Left v
      | `Not_members v -> `Right v
    ) in
    let joined_members =
      members |> CCList.fold_left member_set_intersection None
    in
    let joined_non_members =
      not_members |> CCList.fold_left member_set_union None
    in
    match joined_members, joined_non_members with
    | None,         Some _ ->
      failwith "Tag: Query-expr: Not-expressions can't stand alone"
    | None,         None   -> None
    | Some _ as v,  None   -> v
    | Some members, Some non_members ->
      Some (Member.PathSet.diff members non_members)
  
  (*exposed*)
  let query ~dryrun ~root ~query =
    query
    |> List.map (member_paths_of_expr ~root) 
    |> join_member_exprs
    |> CCOption.get_or ~default:Member.PathSet.empty

  let rm_tag_for_path ~dryrun ~root ~tag ~path =
    tag
    |> members ~root ~recurse:false
    |> List.iter (fun member -> Member.(
      if member.path = path then begin
        if dryrun then
          log "DRYRUN: rm_tag_for_path: would delete file: %s"
            (Fpath.to_string member.symlink_path)
        else 
          member.symlink_path
          |> OS.File.delete ~must_exist:true
          |> R.failwith_error_msg
      end
    ))
    
  let rm_tag_for_paths ~dryrun ~root ~tag ~paths =
    paths |> List.iter (fun path -> rm_tag_for_path ~dryrun ~root ~tag ~path)
  
  let rm ~dryrun ~root ~cwd ~tags ~paths =
    let paths =
      paths
      |> List.map (fun path ->
        if OS.File.exists path |> R.failwith_error_msg then
          Path.resolve_and_normalize path
        else path
      )
      |> List.map (Path.to_absolute ~cwd)
    in
    tags |> List.iter (fun tag ->
      rm_tag_for_paths ~dryrun ~root ~tag ~paths)

  let dirs_containing ~f ~root =
    let rec aux acc path =
      let acc = match f path with
        | Some v -> v :: acc
        | None -> acc
      in
      OS.Dir.exists path >>= function
      | false -> Ok acc
      | true ->
        path |> OS.Dir.contents ~dotfiles:true ~rel:false
        >>= fun paths -> 
        paths |> CCResult.map_l (aux [])
        >>| fun results ->
        results
        |> List.flatten 
        |> List.append acc
    in
    aux [] root

  let tags_of_path ~root ~cwd path =
    let tags_root = Fpath.(root / tags_dirname) in
    let hashed_target =
      path
      |> Path.to_absolute ~cwd
      |> Path.relativize_to_root ~root
      |> Path.hash
    in
    let is_path path' =
      if Fpath.filename path' = hashed_target then
        let tag =
          `Tag (Fpath.parent path')
          |> from_absolute ~root in
        Some tag
      else None
    in
    dirs_containing ~root:tags_root ~f:is_path
    |> R.failwith_error_msg
    |> Set.of_list

  let tags_intersection ~root ~cwd ~paths =
    let intersection acc_set set =
      match acc_set with
      | None -> Some set 
      | Some acc_set -> Some (Set.inter acc_set set)
    in
    paths
    |> List.map (tags_of_path ~root ~cwd)
    |> List.fold_left intersection None
    |> CCOption.get_or ~default:Set.empty

  let tags_union ~root ~cwd ~paths =
    let union acc_set set =
      match acc_set with
      | None -> Some set 
      | Some acc_set -> Some (Set.union acc_set set)
    in
    paths
    |> List.map (tags_of_path ~root ~cwd)
    |> List.fold_left union None
    |> CCOption.get_or ~default:Set.empty
  
  let replace_paths_aux ~dryrun ~root ~path0 ~path1 =
    let override_symlink ~path ~target =
      let mtags_root = Fpath.(root / tags_dirname) in
      let rel_tag =
        Path.relativize_to_root ~root:mtags_root path
        |> Fpath.parent
      in
      begin if dryrun then (
        log "DRYRUN: replace_paths_aux: rel_tag = %a" Fpath.pp rel_tag;
        log "DRYRUN: replace_paths_aux: deleting %a" Fpath.pp path;
        Ok ()
      ) else OS.File.delete path
      end >>= fun () ->
      let rel_target = Path.relativize_to_root ~root target in
      tag_path_once ~dryrun ~root ~path:rel_target @@ `Tag rel_tag
    in
    let rec aux path =
      OS.Dir.exists path >>= function
      | false ->
        begin match OS.Path.symlink_target path with
          | Ok target ->
            if dryrun then begin
              log "DRYRUN: replace_paths_aux: found symlink: %a -> %a"
                Fpath.pp path
                Fpath.pp target
            end;
            let target = Fpath.(root // parent path // target |> normalize) in
            if dryrun then begin
              log "DRYRUN: replace_paths_aux: mapped target to %a" Fpath.pp target
            end;
            begin match Fpath.rem_prefix path0 target with
              | None -> (*< Note: if equal it's None too.. *)
                if not @@ Fpath.equal path0 target then Ok () else
                  override_symlink ~path ~target:path1
              | Some rel_target -> 
                let target = Fpath.(path1 // rel_target) in
                override_symlink ~path ~target
            end
          | Error _ -> Ok () (*< Note: ignoring non-symlinks*)
        end
      | true ->
        path |> OS.Dir.contents ~dotfiles:true ~rel:false
        >>= fun paths -> 
        paths |> CCResult.fold_l (fun _ e -> aux e) ()
    in
    aux root
  
  let replace_paths ~dryrun ~root path0 path1 =
    let cwd = OS.Dir.current () |> R.failwith_error_msg in
    (*> Note: '//' potentially overrides cwd prefix*)
    let path0 = Fpath.(cwd // path0 |> normalize) in
    let path1 = Fpath.(cwd // path1 |> normalize) in
    if dryrun then begin
      log "DRYRUN: replace_paths: path0 = %a" Fpath.pp path0;
      log "DRYRUN: replace_paths: path1 = %a" Fpath.pp path0;
    end;
    assert (Fpath.is_rooted ~root path0);
    assert (Fpath.is_rooted ~root path1);
    replace_paths_aux ~dryrun ~root ~path0 ~path1
    |> R.failwith_error_msg
  
end
