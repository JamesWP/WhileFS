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

type Statement = 
    | Assignment
    | Skip
    | StatementConcat
    | If
    | While


type ArithExp = 
    | Var
    | Int
    | Addition
    | Subtraction
    | Multiply
    member this.Value(c:Context) = 1 //TODO: stub

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
        | Not(b) -> !b.Value
        | 


type Assignment = Identifier * ArithExp
type StatementConcat = Statement * Statement
type If(b:BooleanExp,strue:Statement,sfalse:Statement) = 
    let b = b
    let strue = strue
    let sfalse = sfalse

let eq = Equals (True,True)


type While(b:BooleanExp,s:Statement) = 
    let b = b
    let s = s




[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
