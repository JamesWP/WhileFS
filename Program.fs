module WhileFS

open Parser
open Runtime
open System

[<EntryPoint>]
let main argv =
    let stmt = Parser.stringToStatement (String.Join(" ",argv))
    let context = Runtime.Context()
    let result = context.Compute(stmt)
    printf "%s\n" (result.ToString())
    1
