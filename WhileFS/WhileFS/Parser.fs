﻿// x:=0;while (x<=10) do (x:=x+1)
module Parser
type Token = 
    | Identifier of string
    | Assignment //
    | Number of int//
    | StatementSeperator //
    | While
    | LBracket //
    | Rbracket //
    | LTEquals //
    | Do//
    | Addition //
    | Brackets of Token list

open System

let az = set ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']
let contains x = Seq.exists ((=) x)
let rec tokens acc = function
    // ignore extra whitespace
    | w :: t when Char.IsWhiteSpace(w) -> tokens acc t
    // empty codes
    | ';' :: t -> tokens ( Token.StatementSeperator :: acc)  t
    | ':' :: '=' :: t -> tokens (Token.Assignment :: acc) t
    | '(' :: t -> tokens (Token.LBracket::acc) t
    | ')' :: t -> tokens (Token.Rbracket::acc) t
    | '<' :: '=' :: t -> tokens(Token.LTEquals::acc) t
    | '+' :: t -> tokens (Token.Addition::acc) t
    | 'w'::'h'::'i'::'l'::'e'::t -> tokens (Token.While::acc) t
    | 'd'::'o'::t -> tokens (Token.Do::acc) t
    // numbers
    | d :: t when Char.IsDigit(d) -> 
        // t' is the remaining string after the number, n is the number
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
    | _ -> failwith "tokenisation error"



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
