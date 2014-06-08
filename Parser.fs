// x:=0;while (x<=10) do (x:=x+1)
module Parser
type Token =

    // statement
    | Assignment //
    | StatementSeperator //
    | While //
    | LBracket //
    | Rbracket //
    | Do//
    | If//
    | Else //
    | Then //
    | Skip //

    // arith
    | IdentifierToken of string //
    | Addition //
    | Subtraction //
    | Multiply //
    | Number of int//

    // bools
    | LTEquals //
    | True//
    | False//
    | Equals//
    | Not//
    | And//



    // added in after extra lex step
    | Brackets of Token list

open System

let az = set ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']

let contains x = Seq.exists ((=) x)

let rec hasNoMoreIdentifier = function
    |   a::t when Char.IsWhiteSpace(a) -> true
    |   a::t when contains a az -> false
    |   _ -> true

let rec parseInteger acc = function
            | '0' :: t  -> parseInteger (acc*10) t
            | '1' :: t  -> parseInteger (acc*10 + 1) t
            | '2' :: t  -> parseInteger (acc*10 + 2) t
            | '3' :: t  -> parseInteger (acc*10 + 3) t
            | '4' :: t  -> parseInteger (acc*10 + 4) t
            | '5' :: t  -> parseInteger (acc*10 + 5) t
            | '6' :: t  -> parseInteger (acc*10 + 6) t
            | '7' :: t  -> parseInteger (acc*10 + 7) t
            | '8' :: t  -> parseInteger (acc*10 + 8) t
            | '9' :: t  -> parseInteger (acc*10 + 9) t
            | [] -> [], acc
            | t -> t, acc

let rec tokens acc = function
    // empty codes
    | ';' :: t -> tokens ( Token.StatementSeperator :: acc)  t
    | ':' :: '=' :: t -> tokens (Token.Assignment :: acc) t
    | '(' :: t -> tokens (Token.LBracket::acc) t
    | ')' :: t -> tokens (Token.Rbracket::acc) t
    | '<' :: '=' :: t -> tokens(Token.LTEquals::acc) t
    | '*' :: t -> tokens(Token.Multiply::acc) t
    | '!' :: t -> tokens(Token.Not::acc) t
    | '&' :: '&' :: t -> tokens(Token.Not::acc) t
    | '=' :: t -> tokens(Token.Equals::acc) t
    | '+' :: t -> tokens (Token.Addition::acc) t
    | '-' :: t -> tokens (Token.Subtraction::acc) t
    | 'w'::'h'::'i'::'l'::'e'::t when hasNoMoreIdentifier t -> tokens (Token.While::acc) t
    | 'd'::'o'::t when hasNoMoreIdentifier t -> tokens (Token.Do::acc) t
    | 'i'::'f'::t when hasNoMoreIdentifier t -> tokens (Token.If::acc) t
    | 't'::'r'::'u'::'e'::t when hasNoMoreIdentifier t -> tokens (Token.True::acc) t
    | 'f'::'a'::'l'::'s'::'e'::t when hasNoMoreIdentifier t -> tokens (Token.False::acc) t
    | 't'::'h'::'e'::'n'::t when hasNoMoreIdentifier t -> tokens (Token.Then::acc) t
    | 'e'::'l'::'s'::'e'::t when hasNoMoreIdentifier t -> tokens(Token.Else::acc) t
    | 's'::'k'::'i'::'p'::t when hasNoMoreIdentifier t -> tokens(Token.Skip::acc) t

    // numbers
    | (sp::'-'::d::t)  when Char.IsWhiteSpace(sp) ->
        let t',n = parseInteger 0 (d::t)
        tokens (Token.Number(-n)::acc) t'

    // ignore extra whitespace
    | w :: t when Char.IsWhiteSpace(w) -> tokens acc t

    | ('+'::d::t)
    | d :: t when Char.IsDigit(d) ->
        let t',n = parseInteger 0 (d::t)
        tokens (Token.Number(n)::acc) t'
    // identifier
    | c :: t when contains c az->
        let rec parseIdentifier (acc:string) = function
            | c :: t when contains c az -> parseIdentifier (acc + c.ToString()) t
            | [] -> [], acc
            | t -> t, acc
        let t',i = parseIdentifier "" (c::t)
        tokens (Token.IdentifierToken(i)::acc) t'
    | [] -> List.rev(acc)
    | err -> failwith "tokenisation error"



let private removeBrackets tokens =
    let rec removeBrackets' level acc = function
        |   Token.LBracket :: t -> removeBrackets' (level+1) (Token.LBracket :: acc) t
        |   Token.Rbracket :: t when level = 1 -> (List.rev (Token.Rbracket::acc)),t
        |   Token.Rbracket :: t -> removeBrackets' (level-1) (Token.Rbracket :: acc) t
        |   a :: t -> removeBrackets' level (a :: acc) t
        |   _ -> failwith "unmatched bracket"
    let left,rest = removeBrackets' 0 [] tokens
    let leftnobrackets = (left.Tail |> List.rev).Tail |> List.rev
    leftnobrackets,rest

let private composeBrackets tokens =
    let rec cb acc = function
        |   LBracket  :: t ->
            let inside,rest = removeBrackets (LBracket::t)
            cb (Brackets(cb [] inside)::acc) rest
        |   Rbracket :: t -> failwith "unexpected rbracket"
        |   a::t -> cb (a::acc) t
        |   [] -> acc |> List.rev
        //|   _ -> failwith "error"
    cb [] tokens


// takes a while program string and returns the tokens for the string or errors
let tokensWhile (s:string) =
    let chars = List.ofArray(s.ToCharArray())
    let toks = tokens [] chars
    composeBrackets toks


let rec parseArithComplete = function
        | Token.Number(n) :: [] -> Syntax.ArithExp.Int(n)
        | Token.IdentifierToken(i) :: []-> Syntax.ArithExp.Var(i)
        | Token.Brackets(t) :: [] -> parseArithComplete t
        | a :: Token.Addition :: r ->
            Syntax.ArithExp.Addition( parseArithComplete [a], parseArithComplete r )
        | a :: Token.Subtraction :: r ->
            Syntax.ArithExp.Subtraction( parseArithComplete [a], parseArithComplete r )
        | a :: Token.Multiply :: r ->
            Syntax.ArithExp.Multiply( parseArithComplete [a], parseArithComplete r )
        | _ -> failwith "unexpected token"

// this should parse all the tokens possible from the stream into an arithmetic expression  and return the rest
let parseArith (t:Token list) =
    // returns if token is arith
    let arithToken = function
        | Token.Addition | Token.Brackets(_) | Token.IdentifierToken(_)
        | Token.Multiply | Token.Number(_) | Token.Subtraction -> true
        | _ -> false
    let rec seperateNextArith acc = function
        | a :: r when not(arithToken a) -> (acc |> List.rev),(a::r)
        | a :: r -> seperateNextArith (a :: acc) r
        | [] -> (acc |> List.rev),[]
        | _ -> failwith "unexpected token"
    let getArithForOperand left right = function
                | Token.Addition -> Syntax.ArithExp.Addition(left,right)
                | Token.Multiply -> Syntax.ArithExp.Multiply(left,right)
                | Token.Subtraction -> Syntax.ArithExp.Subtraction(left,right)
                | _ -> failwith "expected operand"

    let arithTokens,rest = seperateNextArith [] t
    rest,(parseArithComplete arithTokens)

// fortunatley the grammar of the language will seperate the bool expressions into complete sections
// so all the passed tokens are to be used or fail horribley...
let rec parseBool = function
    |   Token.True :: [] -> Syntax.BooleanExp.True
    |   Token.False :: [] -> Syntax.BooleanExp.False
    |   Token.Not :: r -> Syntax.BooleanExp.Not(parseBool r)
    |   Token.Brackets(a) :: [] -> parseBool a
    |   l :: Token.LTEquals :: r -> Syntax.BooleanExp.LTEquals(parseArithComplete [l],parseArithComplete r)
    |   l :: Token.And :: r -> Syntax.BooleanExp.And(parseBool [l],parseBool r)
    |   l :: Token.Equals :: r -> Syntax.BooleanExp.Equals(parseArithComplete [l],parseArithComplete r)
    | _ -> failwith "unexpected token"

let rec parseStatement tokens =
    let concatOrEnd newstatement = function
        | [] -> newstatement
        | StatementSeperator :: restrest -> Syntax.Statement.StatementConcat(newstatement,parseStatement restrest)
        | _ -> failwith "statement concatination expected"
    match tokens with
      // assignment
      |   Token.IdentifierToken(x) :: Assignment :: t ->
              let rest,arith = parseArith t
              concatOrEnd (Syntax.Statement.Assignment(x,arith)) rest
      // while
      |   Token.While :: Token.Brackets(bool) :: Do :: Brackets(stmt) :: t ->
              concatOrEnd (Syntax.Statement.While(parseBool bool,parseStatement stmt)) t
      // if
      |   Token.If :: Token.Brackets(bool) :: Then :: Brackets(s1) :: Else:: Brackets(s2)::t->
              concatOrEnd (Syntax.Statement.If(parseBool bool,parseStatement s1,parseStatement s2)) t
      // skip
      |   Token.Skip :: t-> concatOrEnd Syntax.Statement.Skip t
      |   [] -> Syntax.Statement.Skip
      |   _ -> failwith "unexpected token"

let public stringToStatement input = input |> tokensWhile |> parseStatement
