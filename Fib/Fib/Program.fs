// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


(*let rec fib n =
    match n with
    | 1 | 2 -> 1
    | n -> fib(n-1) + fib(n-2)
    *)
    
let fib n =
    let lookup = Array.create (n+1) 0
    let rec f = fun x ->
        match x with
        | 1 | 2 -> 1
        | x ->
            match lookup.[x] with
            | 0 ->
                lookup.[x] <- f(x-1) + f(x-2)
                lookup.[x]
            | x -> x
    f(n)

open System
open System.Diagnostics
let test n = 
    let sw = Stopwatch()

    sw.Start()
    let x = fib(n)
    sw.Stop()

    Console.WriteLine("fibRec({0}): {1}", n, x)
    Console.WriteLine("Time: {0}", sw.ElapsedMilliseconds)

[<EntryPoint>]
let main argv = 
    test 100
    Console.ReadKey()  |> ignore
    0 // return an integer exit code
