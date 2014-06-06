// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module WhileFS

type Identifier = 
    | String of string
    member this.Value(context:Context) = match this with
                                         | String(s) -> context.ValueOf(this)

and Context = 
    val variables:Map<Identifier,int>
    new()= {variables = Map.empty}
    new(other:Context) = {variables = other.variables}
    new(other:Context,name:Identifier,value:int) = {variables = other.variables.Add(name,value)}
    member this.ValueOf(i:Identifier) = 
        if this.variables.ContainsKey(i)
        then this.variables.Item(i)
        else failwith ("Invalid Identifier name: " + i.ToString())

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


open Parser

// stub
let parseArith t = [Parser.Token.StatementSeperator], ArithExp.Int(0)
let parseBoolCondition t = [Parser.Token.StatementSeperator], BooleanExp.True 
// stub; takes tokens returns tokens between ( and corresponding ) , rest
let removeBrackets t = t,t
// stub; tokens is (; statement) or [] else blow up; return statement
let concatOrEnd s t = Statement.Skip

let rec parseStatement = function
    |   Parser.Token.Identifier(i) :: Parser.Token.Assignment :: t ->
            let t', arith = parseArith t
            concatOrEnd (Statement.Assignment(Identifier.String(i),arith)) t'
    |   Parser.Token.While :: t ->
            let t',bool = parseBoolCondition (removeBrackets t)
            match t' with
                | Parser.Token.Do :: t -> 
                    let statetokens,rest = removeBrackets t
                    let whilestate = parseStatement statetokens
                    concatOrEnd (Statement.While(bool,whilestate)) rest
                | _ -> failwith "expecting  'do'"
    |   _ -> failwith "unexpected token"

open System
open System.Diagnostics
[<EntryPoint>]
let main argv =
    //TODO: test ifRegex
    //let result = prog.Compute c
    //Debug.WriteLine(result)
    //let test = parseArith "a+b+c*1"
    let t = Parser.tokensWhile " x:=0;while (x<=10) do (x:=x+1)"
    //let t = P
    1 // return an integer exit code
