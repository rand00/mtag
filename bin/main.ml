open Rresult
open Bos

let log = Mtag.log

let dryrun = false
(* let dryrun = true *)

let run_tags_union ~root ~cwd paths =
  let paths = paths |> List.map (fun p ->
    Fpath.of_string p |> R.failwith_error_msg
  )
  in
  Mtag.Run.tags_union ~root ~cwd ~paths
  |> Mtag.tags_to_string
  |> print_endline

let run_tags_intersection ~root ~cwd paths =
  let paths = paths |> List.map (fun p ->
    Fpath.of_string p |> R.failwith_error_msg
  )
  in
  Mtag.Run.tags_intersection ~root ~cwd ~paths
  |> Mtag.tags_to_string
  |> print_endline

let compare_name path path' =
  let n, n' = Fpath.((basename path), (basename path')) in
  CCString.compare n n'

let compare_mtime_exn path path' =
  begin
    OS.Path.stat path >>= fun stat ->
    OS.Path.stat path' >>| fun stat' ->
    CCFloat.compare stat.Unix.st_mtime stat'.Unix.st_mtime
  end
  |> R.failwith_error_msg

let find_root () =
  let is_mtags_dir file =
    Fpath.basename file = "_mtags"
    && Sys.is_directory @@ Fpath.to_string file
  in
  let rec aux dir = 
    OS.Dir.contents dir >>= fun files ->
    match files |> List.find_opt is_mtags_dir with
    | None ->
      let parent = Fpath.parent dir in
      if parent = dir then (
        log `Error "Didn't find any `_mtags` directory upwards from CWD. \
                    See --help";
        exit 1
      ) else (
        aux parent
      )
    | Some mtags_dir ->
      let root = Fpath.parent mtags_dir in
      Ok root
  in
  (OS.Dir.current () >>= aux)
  |> R.failwith_error_msg

let paths_from_stdin () =
  OS.File.(read_lines dash)
  |> R.failwith_error_msg

let print_usage () = print_endline Usage.v

let main () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  let argv = Sys.argv |> Array.to_list |> List.tl in
  begin match argv with
    | "--help" :: [] ->
      print_usage ();
      exit 0
    | _ -> ()
  end;
  let cwd = OS.Dir.current () |> R.failwith_error_msg in
  let root, argv = match argv with
    | [] -> find_root (), argv
    | arg :: argv' ->
      match CCString.chop_prefix ~pre:"--root=" arg with
      | None -> find_root (), argv
      | Some root ->
        let root = Fpath.v root |> Mtag.Path.to_absolute ~cwd in
        assert (OS.Dir.exists root |> R.failwith_error_msg);
        root, argv'
  in
  match argv with
  | "replace-paths" :: path0 :: path1 :: [] ->
    let path0 = Fpath.of_string path0 |> R.failwith_error_msg in
    let path1 = Fpath.of_string path1 |> R.failwith_error_msg in
    Mtag.Run.replace_paths ~dryrun ~root path0 path1
  | "query" :: query_str :: [] ->
    let query = query_str |> Mtag.parse_query_string in
    Mtag.Run.query ~dryrun ~root ~query
    |> Mtag.Member.PathSet.to_list
    (*> gomaybe print a more informative message:
      * currently it just says that file doesn't exist
        * should instead specify that file is linked from a broken symlink,
          * and optimally print symlink path too
    *)
    |> CCList.filter (Mtag.Path.verify ~debug:dryrun ~root)
    |> CCList.sort compare_mtime_exn
    |> CCList.to_string ~sep:"\n" Fpath.to_string
    |> print_endline
  | "query" :: _ ->
    log `Error "Too many arguments. See --help";
    exit 1
  | "mv" :: oldtags :: newtags :: [] ->
    let oldtags = oldtags |> Mtag.parse_string |> List.map Mtag.open_tag in
    let newtags = newtags |> Mtag.parse_string in
    let paths = 
      Mtag.Run.query ~dryrun ~root ~query:oldtags
      |> Mtag.Member.PathSet.to_list
    in
    Mtag.Run.tag ~dryrun ~root ~cwd ~tags:newtags ~paths;
    Mtag.Run.rm ~dryrun ~root ~cwd ~tags:oldtags ~paths
  | "rm" :: tags_str :: paths ->
    let tags = tags_str |> Mtag.parse_string in
    let paths = match paths with
      | "-" :: [] -> paths_from_stdin ()
      | v -> v
    in
    let paths = paths |> List.map (fun p ->
      Fpath.of_string p |> R.failwith_error_msg
    )
    in
    Mtag.Run.rm ~dryrun ~root ~cwd ~tags ~paths
  | "tags-intersection" :: paths ->
    let paths = match paths with
      | "-" :: [] -> paths_from_stdin ()
      | v -> v
    in
    run_tags_intersection ~root ~cwd paths 
  | "tags-union" :: paths
  | "tags" :: paths -> 
    let paths = match paths with
      | "-" :: [] -> paths_from_stdin ()
      | v -> v
    in
    run_tags_union ~root ~cwd paths
  | "export" :: query_str :: dir_name :: [] ->
    let query = query_str |> Mtag.parse_query_string in
    let dir = Fpath.of_string dir_name |> R.failwith_error_msg in
    Mtag.Run.query ~dryrun ~root ~query
    |> Mtag.Member.PathSet.to_list
    |> CCList.filter (Mtag.Path.verify ~debug:dryrun ~root)
    |> Mtag.Run.export ~dryrun ~cwd ~dir
  | tags_str :: paths -> 
    let tags = tags_str |> Mtag.parse_string in
    let paths = match paths with
      | "-" :: [] -> paths_from_stdin ()
      | v -> v
    in
    let paths = paths |> List.map (fun p ->
      Fpath.of_string p |> R.failwith_error_msg
    )
    in
    Mtag.Run.tag ~dryrun ~root ~cwd ~tags ~paths
  | [] ->
    print_usage ();
    exit 1
  
let () = main ()
