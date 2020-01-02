// Learn more about F# at http://fsharp.org

open System
open System.IO
open DumbLang

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let text = File.ReadAllText(argv.[0])

    let compilation = DumbLangParser().parse text

    compilation |> ignore

    0 // return an integer exit code
