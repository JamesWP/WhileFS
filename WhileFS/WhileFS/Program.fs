// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


type Identifier(name:string) =
    let name:string = name
    member this.Name = name
    member this.Value(context:Context) = context.ValueOf(this)

and Context = 
    val variables:Map<string,int>
    new()= {variables = Map.empty}
    new(other:Context) = {variables = other.variables}
    new(other:Context,name:Identifier,value:int) = {variables = other.variables.Add(name.Name,value)}
    member this.ValueOf(i:Identifier) = 
        if this.variables.ContainsKey(i.Name)
        then this.variables.Item(i.Name)
        else failwith ("Invalid Identifier name: " + i.Name)

type ArithExp = 
    | Var of Identifier
    | Int of int
    | Addition of ArithExp * ArithExp
    | Subtraction of ArithExp * ArithExp
    | Multiply of ArithExp * ArithExp
    member this.Value(c:Context) = match this with
        | Var(i) -> c.ValueOf(i)
        | Int(i) -> i
        | Addition(a,b)-> a.Value c + b.Value c
        | Subtraction(a,b) -> a.Value c - b.Value c
        | Multiply(a,b) -> a.Value c * b.Value c

type BooleanExp = 
    | True
    | False
    | Equals of ArithExp * ArithExp
    | LTEquals of ArithExp * ArithExp
    | Not of BooleanExp
    | And of BooleanExp * BooleanExp
    member this.Value(c:Context) = match this with
        | True -> true
        | False -> false
        | Equals(a,b) -> a.Value c = b.Value c
        | LTEquals(a,b) -> a.Value c <=b.Value c
        | Not(a) -> not(a.Value c)
        | And(a,b) -> a.Value c && b.Value c

type Statement = 
    | Assignment of Identifier * ArithExp
    | Skip
    | StatementConcat of Statement * Statement
    | If of BooleanExp * Statement * Statement
    | While of BooleanExp * Statement
    member this.Compute(c:Context) = match this with
        | Assignment(i,a) -> Context(c,i,a.Value c)
        | Skip -> c
        | StatementConcat(s1,s2) -> s1.Compute c |> s2.Compute
        | If(b,strue,sfalse) -> if b.Value(c) then strue.Compute c else sfalse.Compute c
        | While(b,s) -> 
            let mutable cur = c
            while (b.Value cur) do
                cur <- s.Compute cur
            cur
open System
open System.Text.RegularExpressions

let partition (s:string,p:Regex) =
    let parts = p.Split(s,1)
    if not(parts.Length >= 2) then failwith "partition error p:" p
    else (parts.[0],parts.[1])

let (|Equals|_|) x y = if x.Equals(y) then Some() else None
let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let parseArith (s:string) = 
    Int 1

let rec parseBool (s) = 
    match s with
    | ParseRegex "\s*(.*)\s*" [trim] -> parseBool trim
    | Equals "true" () -> True
    | Equals "false" () -> False
    | ParseRegex "(.*)=(.*)" [left;right] -> Equals(parseArith left,parseArith right)
    | ParseRegex "(.*)<=(.*)" [left;right] -> LTEquals(parseArith left,parseArith right)
    | ParseRegex "(.*)^(.*)" [left;right] -> LTEquals(parseArith left,parseArith right)


let ifRegex = Regex(@"(if\s*\((.*)\)\s*then\s*\((.*)\)\s*else\s*\((.*)\))")
let whileRegex = Regex(@"(while\s*\((.*)\)\s*do\s*\((.*)\))")

let rec parseStatement (s:string):Statement =
    let s = s.Trim()
    if s.Contains ";" then
        let (s1,s2) = partition(s,Regex ("\\;"))
        StatementConcat( parseStatement s1,parseStatement s2)
    elif s.Equals "skip"  then Skip
    elif s.Contains ":="  then
        let (var,aexp) = partition (s,Regex ":=")
        Assignment (Identifier var,parseArith aexp)
    else 
        let ifmatches = ifRegex.Match(s)
        if ifmatches.Groups.Count > 0 then
            if not(ifmatches.Groups.Count = 4) then failwith "if syntax error" s
            let bExp = parseBool (ifmatches.Groups.Item(1).Value)
            let strue = parseStatement (ifmatches.Groups.Item(2).Value)
            let sfalse = parseStatement (ifmatches.Groups.Item(3).Value)
            If(True,strue,sfalse)
        else
            let whilematches = whileRegex.Match(s)
            if whilematches.Groups.Count > 0 then
                if not(whilematches.Groups.Count = 3) then failwith "while syntax error" s
                let bExp = parseBool (whilematches.Groups.Item(1).Value)
                let s = parseStatement (whilematches.Groups.Item(2).Value)
                While(bExp,s)
            else failwith "unexpected syntax:" s


let c = Context(Context(Context(),Identifier "x",1),Identifier "y",10)
let prog = While(
    LTEquals(Int 0,Var (Identifier "y")),
    StatementConcat(
        Assignment(Identifier "x",Multiply(Int 2,Var (Identifier "x"))),
        Assignment(Identifier "y",Subtraction(Var (Identifier "y"),Int 1))
    )
)
open System.Diagnostics
[<EntryPoint>]
let main argv =
    //TODO: test ifRegex
    //let result = prog.Compute c
    //Debug.WriteLine(result)
    1 // return an integer exit code
