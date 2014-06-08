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
    | Identifier of string //
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
        tokens (Token.Identifier(i)::acc) t'
    | [] -> List.rev(acc)
    | err -> failwith "tokenisation error"



let removeBrackets tokens = 
    let rec removeBrackets' level acc = function
        |   Token.LBracket :: t -> removeBrackets' (level+1) (Token.LBracket :: acc) t
        |   Token.Rbracket :: t when level = 1 -> (List.rev (Token.Rbracket::acc)),t
        |   Token.Rbracket :: t -> removeBrackets' (level-1) (Token.Rbracket :: acc) t
        |   a :: t -> removeBrackets' level (a :: acc) t
        |   _ -> failwith "unmatched bracket"
    let left,rest = removeBrackets' 0 [] tokens
    let leftnobrackets = (left.Tail |> List.rev).Tail |> List.rev
    leftnobrackets,rest

let composeBrackets tokens = 
    let rec cb acc = function
        |   LBracket  :: t ->
            let inside,rest = removeBrackets (LBracket::t)
            cb (Brackets(cb [] inside)::acc) rest
        |   Rbracket :: t -> failwith "unexpected rbracket"
        |   a::t -> cb (a::acc) t
        |   [] -> acc |> List.rev
        //|   _ -> failwith "error"
    cb [] tokens
    

let tokensWhile (s:string) = 
    let chars = List.ofArray(s.ToCharArray()) 
    let toks = tokens [] chars
    composeBrackets toks
