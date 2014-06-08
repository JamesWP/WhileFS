module Runtime

open Syntax

type Context =
    val variables:Map<Syntax.Identifier,int>

    // constructors
    new()= {variables = Map.empty}
    new(other:Context) = {variables = other.variables}
    new(other:Context,name:Syntax.Identifier,value:int) = {variables = other.variables.Add(name,value)}

    // overrides
    override this.ToString() = this.variables.ToString()

    // values
    member this.Value(i:Syntax.Identifier) =
        if this.variables.ContainsKey(i)
        then this.variables.Item(i)
        else failwith ("Invalid Identifier name: " + i.ToString())
    member this.Value(a:Syntax.ArithExp) =
        match a with
          | Var(i) -> this.Value(i)
          | Int(i) -> i
          | Addition(a,b) -> this.Value(a) + this.Value(b)
          | Subtraction(a,b) -> this.Value(a) - this.Value(b)
          | Multiply(a,b) -> this.Value(a) * this.Value(b)
    member this.Value(b:Syntax.BooleanExp) =
        match b with
          | True -> true
          | False -> false
          | Equals(a,b) -> this.Value(a)=this.Value(b)
          | LTEquals(a,b) -> this.Value(a)<=this.Value(b)
          | Not(a) -> not(this.Value(a))
          | And(a,b) -> this.Value(a) && this.Value(b)
    member this.Compute(s:Syntax.Statement) =
        match s with
          | Assignment(i,a) -> Context(this,i,this.Value(a))
          | Skip -> this
          | StatementConcat(s1,s2) -> (this.Compute s1).Compute s2
          | If(b,strue,sfalse) ->
              if this.Value(b) then this.Compute strue
              else this.Compute sfalse
          | While(b,s) ->
              let mutable cur = this
              while (cur.Value b) do cur <- cur.Compute s
              cur
