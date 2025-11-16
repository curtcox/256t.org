open Cid

let read_file path =
  let channel = open_in_bin path in
  let length = in_channel_length channel in
  let content = really_input_string channel length in
  close_in channel;
  content

let check_cids () =
  let cids_dir = cids_dir () in
  let entries = Sys.readdir cids_dir in
  Array.sort String.compare entries;
  let mismatches = ref [] in
  let count = ref 0 in
  Array.iter
    (fun name ->
      let path = Filename.concat cids_dir name in
      if Sys.is_directory path then
        ()
      else (
        incr count;
        let content = read_file path in
        let expected = compute_cid content in
        if expected <> name then mismatches := (name, expected) :: !mismatches))
    entries;
  if !mismatches = [] then (
    Printf.printf "All %d CID files match their contents.\n" !count;
    0
  ) else (
    print_endline "Found CID mismatches:";
    List.iter
      (fun (name, expected) -> Printf.printf "- %s should be %s\n" name expected)
      (List.rev !mismatches);
    1)

let () = exit (check_cids ())
