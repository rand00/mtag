open Rresult
open Bos

let sp = Printf.sprintf

let log_error fmt =
  let kpp k fmt =
    let k fmt = k (Format.flush_str_formatter ()) in
    Format.kfprintf k Format.str_formatter fmt
  in
  kpp (Format.printf "mtag: error: %s\n") fmt

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
        log_error "Didn't find any `_mtags` directory upwards from CWD. See \
                   --help";
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

let usage = {|NAME
  
  mtag - the immutable filesystem tagger

SYNOPSIS
  
  mtag <tags> <paths..>
  mtag query <query>
  mtag mv <oldtags> <newtags>
  mtag rm <tags> <paths..>
  mtag tags-union <paths..>
  mtag tags-intersection <paths..>
  mtag replace-paths <prev-path> <new-path>
  
DESCRIPTION

  Filesystems are tree-structures, which are extremely limited for structuring
  your data. Also, much data is not textbased, so you can't easily do a
  filesystem search on these files.
  Tagging allows a many-to-one relation (many tags to one file), vs the
  filesystems one-to-one relation. And tagging your files (e.g. videos) is an
  act of mapping textual data to these files, which allow them to be queried.

  `mtag` tags any file within an immutable part of the filesystem hierarchy,
  whose base directory contains an `_mtags` directory. Immutable filesystem in 
  this sense, means that files are added and modified, but not moved or deleted
  (see `mtag replace-paths` to fix the tags after moving files).

  -- USAGE / SEMANTICS
  
  When you query for files matching a set of tags, `mtag` outputs each found
  file on separate lines. This allows for composing CLI applications,
  e.g. for loading all `mtag`s query-results in your favorite application. E.g.:
    $ mpv $(mtag query type/video,color/green | head -n5)

  Note that the `bash` shell has problems with files containing spaces when
  expanding the previous `mtag` command (you can set `IFS=$'\n'`).
  The `fish` shell e.g. doesn't have this problem.

  By default `mtag` searches upwards from the current working directory to find
  an `_mtags` directory, which contains all the tags for files within that
  directory (as relative symlinks).
  You have the responsibility to create the `_mtags` directory yourself, as you
  are the one who knows what part of the filesystem will be kept immutable. 
  
  All `mtag` commands optionally supports being given `--root=<root-dir>`
  explicitly as the first argument, which overrides the recursive search for
  the immutable root dir containing the `_mtags` directory.

  For composing `mtag` commands (and other applications), it is advised to use
  the special `-` path-argument, which represents `stdin`.
  This circumvents the mentioned shell problems with whitespace, and allows
  big sets of paths to be input, as the OS have a limit on the size of the
  argument-list (see POSIX `getconf ARG_MAX`). This allows you to e.g.:
    $ mtag query mytag | mtag tags-union -
  
  -- EXAMPLES
  
  The directory structure could look as follows:
    ~/my_immutable_data
    |-- _mtags/
    |-- videos/
        |-- 2001_01_01
            |--video0.mp4
            |--video1.mp4

  .. in this example, to run an `mtag` command, you would either `cd` into a
  directory within `~/my_immutable_data` or pass `--root=~/my_immutable_data`
  to `mtag`.
  E.g.:
    # Initializing the immutable root
    $ mkdir ~/my_immutable_data/_mtags
    # Ordinary workflow
    $ cd ~/my_immutable_data/videos/2001_01_01/
    $ mtag score/5,animal/whale video0.mp4
    $ mtag query animal/whale
    > ~/my_immutable_data/videos/2001_01_01/video0.mp4

  Another useful thing is e.g. to explore the `mtags_` directory with:
    $ tree -d _mtags | less
  .. which shows all the tags you have already created.

COMMANDS

mtag <tags> <paths..>

  Tag all the paths given, where <tags> is of the format:
    <tag>[,<tag>]*
  where <tag> is of the format:
    [<tag-dir>/]*<tag-dir>
  e.g.:
    $ mtag score/5,color/nice/black file0 file1 file3

  If the first and single file is `-`, then `mtag` will read a newline separated
  list of files from stdin instead.

  `--root=<root-dir>` can be passed to override the search for the root
  directory containing an `_mtags` dir, starting from the current working
  directory.

mtag query <query>
  
  Query for all files that match the query, where <query> is of the format:
    <[!][>]tag0>[,<[!][>]tag1>]*
  where
    `tagN` is a potentially nested tag, like `color/black`.
    `!` means 'not'. A not-expression in a query won't do anything if it stands
    alone (for performance reasons); it only filters other query expressions.
    `>` means 'contained within', i.e. including within any sub-tag.

  All files are printed newline separated. The output can be used as argument
  to other CLI applications, e.g. in the `fish` shell:
    $ my_app (mtag query mytag,!mytag2,>mytag3,!>mytag4/mytag5)

  .. note the earlier mentioned problems with whitespace in filenames, and the
  limited argument-list on POSIX systems.

mtag mv <oldtags> <newtags>

  Untag all files that have all of <oldtags>, and tag them instead with <newtags>,
  where tags are a comma-separated list of tags.

  Beware; as a list of several <oldtags> is essentially a simple query, this
  means that files not matching the query will not be moved to <newtags>.
  
mtag rm <tags> <paths..>

  Remove all the given tags from the given paths. This means removing the
  relative symlinks from within the `_mtags` directory, but not removing their
  containing directories. Tags are a commaseparated list of tags.

  See `mtag <tags> <paths..>` for special arguments.
  
mtag tags-union <paths..>

  Output the tags that all the given paths have in common, i.e. the mathematical
  set union.

  The printed string has the same format as the <tags> parameter of the other
  commands - this can be used to e.g. do (in bash):
    $ mtag query $(mtag tags-union file0 file1)
  
  See `mtag <tags> <paths..>` for special arguments.
  
mtag tags-intersection <paths..>

  Works the same as `mtag tags-union` but using mathematical set intersection.

mtag replace-paths <prev-path> <new-path>

  In case you have moved tagged files around within the mtag root directory,
  you can fix all the broken tags (which are symlinks within the `_mtags`
  directory) with this command.

  Paths given can be partial (in case of moving a directory), or full (in case
  of moving a file). Paths are seen as relative to the current working
  directory. E.g. from within your mtag root directory:
  
    $ cd videos
    $ mtag mytag 20230101/vid0.mpg
    $ mkdir newdir
    $ mv 20230101 newdir/
    $ mtag replace-paths 20230101 newdir/20230101

  .. resulting in all the tags of vid0.mpg being fixed to point at the new path.

  You can use `tree _mtags` to see what symbolic links are broken.
  
|}

let print_usage () = print_endline usage

let main () =
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
    log_error "Too many arguments. See --help";
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
