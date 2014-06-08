// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module WhileFS

type Identifier = 
    | String of string
    member this.Value(context:Context) = match this with
                                         | String(s) -> context.ValueOf(this)
    override this.ToString() = match this with | String(s) -> s

and Context = 
    val variables:Map<Identifier,int>
    new()= {variables = Map.empty}
    new(other:Context) = {variables = other.variables}
    new(other:Context,name:Identifier,value:int) = {variables = other.variables.Add(name,value)}
    member this.ValueOf(i:Identifier) = 
        if this.variables.ContainsKey(i)
        then this.variables.Item(i)
        else failwith ("Invalid Identifier name: " + i.ToString())
    override this.ToString() = this.variables.ToString()

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



// stubs


let rec parseArithComplete = function
        | Token.Number(n) :: [] -> ArithExp.Int(n)
        | Token.Identifier(i) :: []-> ArithExp.Var(Identifier.String(i))
        | Token.Brackets(t) :: [] -> parseArithComplete t
        | a :: Token.Addition :: r -> 
            ArithExp.Addition( parseArithComplete [a], parseArithComplete r )
        | a :: Token.Subtraction :: r -> 
            ArithExp.Subtraction( parseArithComplete [a], parseArithComplete r )
        | a :: Token.Multiply :: r -> 
            ArithExp.Multiply( parseArithComplete [a], parseArithComplete r )
        | _ -> failwith "unexpected token"

// this should parse all the tokens possible from the stream into an arithmetic expression  and return the rest
let parseArith (t:Token list) = 
    // returns if token is arith
    let arithToken = function
        | Token.Addition | Token.Brackets(_) | Token.Identifier(_)
        | Token.Multiply | Token.Number(_) | Token.Subtraction -> true
        | _ -> false
    let rec seperateNextArith acc = function
        | a :: r when not(arithToken a) -> (acc |> List.rev),(a::r)
        | a :: r -> seperateNextArith (a :: acc) r
        | [] -> (acc |> List.rev),[]
        | _ -> failwith "unexpected token"
    let getArithForOperand left right = function
                | Token.Addition -> ArithExp.Addition(left,right)
                | Token.Multiply -> ArithExp.Multiply(left,right)
                | Token.Subtraction -> ArithExp.Subtraction(left,right)
                | _ -> failwith "expected operand"

    let arithTokens,rest = seperateNextArith [] t
    rest,(parseArithComplete arithTokens)

// fortunatley the grammar of the language will seperate the bool expressions into complete sections
// so all the passed tokens are to be used or fail horribley...

// x=<10
let rec parseBool = function 
    |   Token.True :: [] -> BooleanExp.True
    |   Token.False :: [] -> BooleanExp.False
    |   Token.Not :: r -> BooleanExp.Not(parseBool r)
    |   Token.Brackets(a) :: [] -> parseBool a
    |   l :: Token.LTEquals :: r -> BooleanExp.LTEquals(parseArithComplete [l],parseArithComplete r)
    |   l :: Token.And :: r -> BooleanExp.And(parseBool [l],parseBool r)
    |   l :: Token.Equals :: r -> BooleanExp.Equals(parseArithComplete [l],parseArithComplete r)
    | _ -> failwith "unexpected token"



let rec parseStatement tokens = 
    let concatOrEnd newstatement = function
        | [] -> newstatement
        | StatementSeperator :: restrest -> StatementConcat(newstatement,parseStatement restrest)
    match tokens with
    // assignment
    |   Identifier(x) :: Assignment :: t ->
            let rest,arith = parseArith t
            concatOrEnd (Statement.Assignment(Identifier.String(x),arith)) rest 
    // while
    |   While :: Brackets(bool) :: Do :: Brackets(stmt) :: t ->
            concatOrEnd (Statement.While(parseBool bool,parseStatement stmt)) t
    // if
    |   If :: Brackets(bool) :: Then :: Brackets(s1) :: Else:: Brackets(s2)::t->
            concatOrEnd (Statement.If(parseBool bool,parseStatement s1,parseStatement s2)) t
    // skip
    |   Skip :: t-> concatOrEnd Statement.Skip t
    |   [] -> Statement.Skip
    |   _ -> failwith "unexpected token"


open System
open System.IO
[<EntryPoint>]
let main argv =
    let tokens = Parser.tokensWhile (String.Join(" ",argv))
    let stmt = parseStatement tokens
    let context = Context()
    let result = stmt.Compute(context)
    printf "%s" (result.ToString())
    //let arith = parseArith tokens
    1 // return an integer exit code
