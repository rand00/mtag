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

let main () =
  (* let config = *)
  (*   let home_dir_str = OS.Env.var "HOME" |> CCOption.get_exn_or "No $HOME found" in *)
  (*   N_file.Config.of_file Fpath.(v home_dir_str / ".niseqrc" |> to_string) *)
  (*   |> R.failwith_error_msg in *)
  (* let niseq_root = Fpath.v config.niseq_root *)
  let root = failwith "todo" in
  match Sys.argv |> Array.to_list |> List.tl with
  | "query" :: query_str :: [] ->
    let query = query_str |> Mtag.parse_query_string in
    Mtag.Run.query ~debug ~root ~query
    |> Mtag.Member.PathSet.to_list
    (*> goto test if this ordering makes sense - went away from it in 'n'*)
    |> CCList.sort compare_mtime_exn
    (* |> CCList.rev *)
    |> CCList.to_string ~sep:"\n" Fpath.to_string
    |> print_endline
  | "query" :: _ -> failwith "n_tag: Too many arguments"
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
    failwith "n_tag: No arguments supplied"
  
let () = main ()
