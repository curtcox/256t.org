let to_base64url data =
  Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet data

let encode_length length =
  let bytes = Bytes.make 6 '\x00' in
  for i = 0 to 5 do
    Bytes.set bytes (5 - i) (Char.chr ((length lsr (8 * i)) land 0xFF))
  done;
  bytes |> Bytes.to_string |> to_base64url

let compute_cid content =
  let length = String.length content in
  let prefix = encode_length length in
  let suffix =
    if length <= 64 then
      to_base64url content
    else
      Digestif.SHA512.digest_string content
      |> Digestif.SHA512.to_raw_string
      |> to_base64url
  in
  prefix ^ suffix

let executable_dir () =
  let exe = Sys.executable_name in
  let exe = if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe else exe in
  Filename.dirname exe

let base_dir () = executable_dir () |> Filename.dirname |> Filename.dirname
let examples_dir () = Filename.concat (base_dir ()) "examples"
let cids_dir () = Filename.concat (base_dir ()) "cids"
