#load "cid.fsx"

open System
open System.IO
open Cid

let main () =
    if not (Directory.Exists(cidsDirectory)) then
        eprintfn "CID directory not found at '%s'." cidsDirectory
        1
    else
        let files =
            Directory.GetFiles(cidsDirectory)
            |> Array.filter File.Exists
            |> Array.sort

        let mismatches =
            files
            |> Array.choose (fun path ->
                let name = Path.GetFileName path
                let expected = File.ReadAllBytes path |> computeCid
                if name = expected then
                    None
                else
                    Some(name, expected))
            |> Array.toList

        if mismatches.IsEmpty then
            printfn "All %d CID files match their contents." files.Length
            0
        else
            printfn "Found CID mismatches:"
            mismatches
            |> List.iter (fun (actual, expected) -> printfn "- %s should be %s" actual expected)
            1

let exitCode = main ()
exit exitCode
