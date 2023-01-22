open Rresult
open Bos

let sp = Printf.sprintf

let debug = false
(* let debug = true *)

(*goto later; extending interface; 
  * n_tag mv tag1 foo/tag2
  * n_tag list-unique 
    * < see code:projects:art:niseq:subtodos:20200713 how to use n tag tagging
*)

let run_tags_union ~root paths =
  let paths = paths |> List.map (fun p ->
    Fpath.of_string p |> R.failwith_error_msg
  )
  in
  Mtag.Run.tags_union ~debug ~root ~paths
  |> Mtag.tags_to_string
  |> print_endline

let run_tags_intersection ~root paths =
  let paths = paths |> List.map (fun p ->
    Fpath.of_string p |> R.failwith_error_msg
  )
  in
  Mtag.Run.tags_intersection ~debug ~root ~paths
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
  let is_root file =
    Fpath.basename file = "_mtags"
    && Sys.is_directory @@ Fpath.to_string file
  in
  let rec aux dir = 
    OS.Dir.contents dir >>= fun files ->
    match files |> List.find_opt is_root with
    (*> goto fail if parent = dir*)
    | None -> aux @@ Fpath.parent dir
    | Some root -> Ok root
  in
  (OS.Dir.current () >>= aux)
  |> R.failwith_error_msg

let usage = {|
NAME
  mtag - the static filesystem tagger
  
DESCRIPTION

  Filesystems are tree-structures, which are extremely limited for querying
  for all the data you have lying around. `mtag` allows you to tag any file
  within a _static_ filesystem hierarchy (which contains an `_mtags` directory).

  Files can have many tags, which `mtag` lets you query, and which outputs
  each found file on separate lines. This allows for composing CLI applications,
  e.g. for loading all `mtag`s query-results in your favorite application. E.g.:
    $ mpv $(mtag query type/video,color/green | head -n5)

  By default `mtag` searches upwards from the current working directory to find
  an `_mtags` directory, which contains all the tags for files within that
  directory.
  All `mtag` commands optionally supports being given `--root=<root-dir>`
  explicitly as the first argument, which overrides the recursive search for
  the `_mtags` directory.

  Beware that you have the responsibility to create the `_mtags` directory
  yourself, within the root directory of the files you want to tag. The directory
  structure could e.g. look as follows:
    ~/my_static_data
    |-- _mtags/
    |-- videos/
        |-- 2001_01_01
            |--video0.mp4
            |--video1.mp4
  .. in this example, to run a `mtag` command, you would either `cd` into a
  directory within `~/my_static_data` or pass `--root=~/my_static_data` to `mtag`.
  E.g.:
    $ cd ~/my_static_data
    $ mtag score/5,animal/horse videos/2001_01_01/video0.mp4
    $ mtag query animal/horse
    > ~/my_static_data/videos/2001_01_01/video0.mp4
  
COMMANDS

mtag <tags> <paths..>

  Tag all the paths given, where 'tags' is of the format:
    <tag>[,<tag>]*
  where 'tag' is of the format:
    [<tag-dir>/]*<tag-dir>
  e.g.:
    $ mtag score/5,color/nice/black file0 file1 file3

  Tags are saved as relative symlinks in the first found '_mtags' directory
  searched for upwards in the filesystem from the current working directory.
  This directory can be explored with e.g.
    $ tree -d _mtags | less

  Beware that the files that you tag should never be moved, i.e. mtag operates
  on a static filesystem.
  
mtag query <query>
  
  Query for all files that match the query, where 'query' is of the format:
    <[!]tag0>[,<[!]tag1>]*
  where '!' means 'not'.

  All files are printed newline separated. The output can be used as argument
  to other CLI applications, e.g. in the `bash` shell:
    $ my_app $(mtag query mytag,!mytag2)
  
mtag rm <tags> <paths..>

  Remove all the given tags from the given paths. This means removing the
  relative symlinks from within the _mtags directory, but not removing their
  containing directories.
  
mtag tags_union <paths..>

  Output the tags that all the given paths have in common, i.e. the mathematical
  set union.

  The printed string has the same format as the 'tags' parameter of the other
  commands - this can be used to e.g. do (in bash):
    $ mtag query $(mtag tags_union file0 file1)
  
mtag tags_intersection <paths..>

  Works the same as `mtag tags_union` but using mathematical set intersection.
  
|}

let print_usage () = print_endline usage

(*spec; new CLI root passing:
  mtag --root:<dir> query miav
  export MTAG_ROOT=<dir>; mtag query miav # (hmm too confusing as not explicit dep)
  cd <dir-within-root>; mtag query miav
*)
(*> goto fail if no root could be found - print how user should create this root-dir
  themselves*)
let main () =
  let argv = Sys.argv |> Array.to_list |> List.tl in
  let root, argv = match argv with
    | [] -> find_root (), argv
    | arg :: argv' ->
      match CCString.chop_prefix ~pre:"--root=" arg with
      | None -> find_root (), argv
      | Some root -> Fpath.v root, argv'
  in
  match argv with
  | "query" :: query_str :: [] ->
    let query = query_str |> Mtag.parse_query_string in
    Mtag.Run.query ~debug ~root ~query
    |> Mtag.Member.PathSet.to_list
    (*> goto test if this ordering makes sense - went away from it in 'n'*)
    |> CCList.sort compare_mtime_exn
    (* |> CCList.rev *)
    |> CCList.to_string ~sep:"\n" Fpath.to_string
    |> print_endline
  | "query" :: _ -> failwith "Too many arguments"
  | "rm" :: tags_str :: paths ->
    let tags = tags_str |> Mtag.parse_string in
    let paths = paths |> List.map (fun p ->
      Fpath.of_string p |> R.failwith_error_msg
    )
    in
    Mtag.Run.rm ~debug ~root ~tags ~paths
  | "tags_intersection" :: paths ->
    run_tags_intersection ~root paths 
  | "tags_union" :: paths
  | "tags" :: paths -> 
    run_tags_union ~root paths
  | tags_str :: paths -> 
    let tags = tags_str |> Mtag.parse_string in
    let paths = paths |> List.map (fun p ->
      Fpath.of_string p |> R.failwith_error_msg
    )
    in
    Mtag.Run.tag ~debug ~root ~tags ~paths
  | [] ->
    print_usage ();
    exit 1
  
let () = main ()
