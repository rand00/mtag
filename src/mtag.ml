open Rresult
open Bos

let sp = Printf.sprintf

let log fmt =
  let kpp k fmt =
    let k fmt = k (Format.flush_str_formatter ()) in
    Format.kfprintf k Format.str_formatter fmt
  in
  kpp (Format.printf "mtag: %s\n") fmt

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
  `Tag Fpath.(root / tags_dirname // tag_path)

let from_absolute ~root (`Tag tag_path) =
  let root = Fpath.(root / tags_dirname) in
  let tag_path =
    Fpath.(relativize ~root tag_path)
    |> CCOption.get_or ~default:tag_path
    |> Fpath.rem_empty_seg
  in
  `Tag tag_path

let open_tag v = (v : tag :> [>tag])

(*goto cleanup this parser
  *> should all sub-parsers be in result monad instead?
*)
let rec parse_not str : expr option =
  CCString.chop_prefix ~pre:"!" str
  |> CCOption.map (fun str ->
    `Not (parse_expr str)
  )

and parse_tag str : tag =
  let fpath =
    Fpath.of_string str
    |> R.failwith_error_msg
  in
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

(*goto put in common?*)
let normalize_symlink_target symlink_path =
  match symlink_path |> OS.Path.symlink_target with
  | Error err -> None 
  | Ok target ->
    let normalized_target =
      let full_path =
        if Fpath.is_abs target then target else 
          Fpath.(parent symlink_path // target)
      in
      Fpath.normalize full_path
    in
    Some normalized_target

let members ~root ~recurse tag =
  let try_resolve_symlink symlink_path =
    match normalize_symlink_target symlink_path with
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
        && Sys.is_directory (Fpath.to_string content)
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

  let resolve_and_normalize path =
    OS.Path.symlink_target path
    |> CCResult.get_or ~default:path
    |> Fpath.normalize

  let to_absolute path =
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
      OS.File.exists path >>= fun p_exists ->
      OS.Dir.exists path >>| fun p_dir_exists ->
      let p_exists = p_exists || p_dir_exists in
      if debug then log "verify path: path exists = %b" p_exists;
      let p_is_inside_root =
        Fpath.is_rooted ~root path
      in
      if debug then log "verify path: inside root = %b" p_is_inside_root;
      p_exists && p_is_inside_root
    end
    |> R.failwith_error_msg

  (*goto path should be `Absolute *)
  let relativize_to_root ~root path =
    match Fpath.relativize ~root path with
    | Some path -> path
    | None -> failwith (
      sp "n_tag: Couldn't relativize '%s' to niseq-root"
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
    debug:bool ->
    root:Fpath.t ->
    tags:(t list) ->
    paths:(Fpath.t list) ->
    unit

  val query :
    debug:bool ->
    root:Fpath.t ->
    query:query ->
    Member.PathSet.t 

  val rm :
    debug:bool ->
    root:Fpath.t ->
    tags:(t list) ->
    paths:(Fpath.t list) ->
    unit

  val tags_intersection :
    debug:bool ->
    root:Fpath.t ->
    paths:(Fpath.t list) ->
    Set.t 

  val tags_union :
    debug:bool ->
    root:Fpath.t ->
    paths:(Fpath.t list) ->
    Set.t
  
end

module Run : Run = struct

  (*goto make into common module - reusing this between scripts now as boilerplate*)
  module Aux = struct

    let run_exn cmd ~debug =
      if debug then begin
        Cmd.pp Format.std_formatter cmd;
        Format.print_newline ();
      end
      else 
        OS.Cmd.run_io ~err:OS.Cmd.err_stderr cmd OS.Cmd.in_null
        |> OS.Cmd.to_stdout
        |> R.failwith_error_msg

    let run_string_exn cmd ~debug =
      if debug then begin
        Cmd.pp Format.std_formatter cmd;
        Format.print_newline ();
        ""
      end
      else 
        OS.Cmd.run_io ~err:OS.Cmd.err_stderr cmd OS.Cmd.in_null
        |> OS.Cmd.to_string
        |> R.failwith_error_msg

  end

  let run ~debug = Aux.run_exn ~debug 

  let tag_path_once ~debug ~root ~path (`Tag tag_path_relative as tag) =
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
      if debug then begin
        Printf.printf "DEBUG: tag_path_once: create dir: %s\n" (Fpath.to_string tag_path);
        Printf.printf "DEBUG: tag_path_once: create symlink: %s -> %s\n"
          (Fpath.to_string tag_path_file)
          (Fpath.to_string path);
        Ok ()
      end else 
        OS.Dir.create ~path:true tag_path >>= fun _ ->
        tag_path_file |> OS.Path.symlink ~force:true ~target:path 
    end
    |> R.failwith_error_msg
  
  let tag_path ~debug ~root ~tags path =
    tags |> List.iter (tag_path_once ~debug ~root ~path)

  (*exposed*)
  let tag ~debug ~root ~tags ~paths =
    let normalized_paths =
      paths
      |> List.map Path.resolve_and_normalize
      (*< Note: resolving symlinks (once), so one can tag the target of a tag*)
      (*< goto this should only be resolved if path is inside tags-root*)
      |> List.map Path.to_absolute
    in
    if not (normalized_paths |> List.for_all (Path.verify ~debug ~root)) then
      failwith (
        sp "n_tag: Normalized paths didn't verify: %s" 
          (normalized_paths |> List.map Fpath.to_string |> String.concat " "));
    normalized_paths
    |> List.map (Path.relativize_to_root ~root)
    |> List.iter (tag_path ~debug ~root ~tags)

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
  let query ~debug ~root ~query =
    query
    |> List.map (member_paths_of_expr ~root) 
    |> join_member_exprs
    |> CCOption.get_or ~default:Member.PathSet.empty

  let rm_tag_for_path ~debug ~root ~tag ~path =
    tag
    |> members ~root ~recurse:false
    |> List.iter (fun member -> Member.(
      if member.path = path then begin
        if debug then
          log "DEBUG: rm_tag_for_path: would delete file: %s"
            (Fpath.to_string member.symlink_path)
        else 
          member.symlink_path
          |> OS.File.delete ~must_exist:true
          |> R.failwith_error_msg
      end
    ))
    
  let rm_tag_for_paths ~debug ~root ~tag ~paths =
    paths |> List.iter (fun path -> rm_tag_for_path ~debug ~root ~tag ~path)
  
  let rm ~debug ~root ~tags ~paths =
    let paths =
      paths
      |> List.map Path.resolve_and_normalize
      (*< Note: enables removal of the tags of a file that a tag points to
          - do I want this feature? seems unneccesary
            * like for tagging, should only resolve to target, if path is inside
              .. tags root
      *)
      |> List.map Path.to_absolute
    in
    tags |> List.iter (fun tag ->
      rm_tag_for_paths ~debug ~root ~tag ~paths)

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

  let tags_of_path ~root path =
    let tags_root = Fpath.(root / tags_dirname) in
    let hashed_target =
      path
      |> Path.to_absolute 
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

  let tags_intersection ~debug ~root ~paths =
    let intersection acc_set set =
      match acc_set with
      | None -> Some set 
      | Some acc_set -> Some (Set.inter acc_set set)
    in
    paths
    |> List.map (tags_of_path ~root)
    |> List.fold_left intersection None
    |> CCOption.get_or ~default:Set.empty

  let tags_union ~debug ~root ~paths =
    let union acc_set set =
      match acc_set with
      | None -> Some set 
      | Some acc_set -> Some (Set.union acc_set set)
    in
    paths
    |> List.map (tags_of_path ~root)
    |> List.fold_left union None
    |> CCOption.get_or ~default:Set.empty

end
