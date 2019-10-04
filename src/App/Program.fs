open System
open Library
open DerekBanas

[<EntryPoint>]
let main argv =
  seq_stuff()
  
  printfn "Nice command-line arguments! Here's what JSON.NET has to say about them:"

  argv
  |> Array.map getJsonNetJson
  |> Array.iter (printfn "%s")

  let erik = Console.ReadKey()

  printfn "%c" (erik.KeyChar)

  Console.ReadLine() |> ignore

  0 // return an integer exit code