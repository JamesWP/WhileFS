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
    | Empty
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
let parseArith (t:Token list) = t.Tail, ArithExp.Int(0)
let parseBool t = BooleanExp.True 



let rec parseStatement = function
    |   Identifier(x) :: Assignment :: t ->
            let rest,arith = parseArith t
            let newstmnt = Statement.Assignment(Identifier.String(x),arith) 
            match rest with
                | [] -> newstmnt
                | StatementSeperator :: restrest -> StatementConcat(newstmnt,parseStatement restrest)
    |   While :: Brackets(bool) :: Do :: Brackets(stmt) :: t ->
            let newstmnt =Statement.While(parseBool bool,parseStatement stmt)
            match t with
                | [] -> newstmnt
                | StatementSeperator :: restrest -> StatementConcat(newstmnt,parseStatement restrest)
    |   _ -> failwith "unexpected token"


open System
open System.Diagnostics
[<EntryPoint>]
let main argv =
    let tokens = Parser.tokensWhile "x:=0;while(x<=10) do ( x:=1)"
    let stmt = parseStatement tokens
    1 // return an integer exit code
