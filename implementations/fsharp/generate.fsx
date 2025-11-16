#load "cid.fsx"

open System
open System.IO
open Cid

let main () =
    Directory.CreateDirectory(cidsDirectory) |> ignore
    let files =
        Directory.GetFiles(examplesDirectory)
        |> Array.filter File.Exists
        |> Array.sort

    for example in files do
        let content = File.ReadAllBytes example
        let cid = computeCid content
        let destination = Path.Combine(cidsDirectory, cid)
        File.WriteAllBytes(destination, content)
        printfn "Wrote %s from %s" (Path.GetFileName destination) (Path.GetFileName example)

    0

let exitCode = main ()
exit exitCode
