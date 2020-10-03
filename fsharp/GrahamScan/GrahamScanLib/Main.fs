namespace GrahamScan

module Main =
    open System.IO

    let scan (inputFile: string) (outputFile: string) =
        // let rawInput = File.ReadLines(inputFile) |> List.ofSeq
        // let a = GrahamScan.ToPoints rawInput
        let prettyInput = 0
        let prettyOutput = 0
        ()

    [<EntryPoint>]
    let main argv =
        match argv with
        | [| input; output |] -> 
            scan input output
            0
        | _ ->
            printfn "Invalid arguments"
            1