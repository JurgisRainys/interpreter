namespace interpreter

open System
namespace interpreter
 
module Entry = 
    [<EntryPoint>]
    printfn "Press any key to continue..."
    System.Console.ReadLine() |> ignore
    
