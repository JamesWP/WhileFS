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

    let result = prog.Compute c
    //Debug.WriteLine(result)
    1 // return an integer exit code
