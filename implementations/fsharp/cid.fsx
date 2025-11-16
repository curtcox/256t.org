module Cid

open System
open System.IO
open System.Security.Cryptography

let baseDirectory =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

let examplesDirectory = Path.Combine(baseDirectory, "examples")

let cidsDirectory = Path.Combine(baseDirectory, "cids")

let private toBase64Url (data: byte[]) =
    Convert.ToBase64String(data).TrimEnd('=').Replace('+', '-').Replace('/', '_')

let private encodeLength (length: int) =
    let buffer = Array.zeroCreate<byte> 6
    let value = uint64 length
    for i in 0 .. buffer.Length - 1 do
        buffer.[buffer.Length - 1 - i] <- byte (value >>> (8 * i))
    toBase64Url buffer

let computeCid (content: byte[]) =
    let prefix = encodeLength content.Length
    let suffix =
        if content.Length <= 64 then
            toBase64Url content
        else
            use sha = SHA512.Create()
            content |> sha.ComputeHash |> toBase64Url
    prefix + suffix
