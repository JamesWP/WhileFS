// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.


let parseInteger (str:string) = 
    let rec parseInteger' acc = function
        | '0' :: t  -> parseInteger' (acc*10) t
        | '1' :: t  -> parseInteger' (acc*10 + 1) t
        | '2' :: t  -> parseInteger' (acc*10 + 2) t
        | '3' :: t  -> parseInteger' (acc*10 + 3) t
        | '4' :: t  -> parseInteger' (acc*10 + 4) t
        | '5' :: t  -> parseInteger' (acc*10 + 5) t
        | '6' :: t  -> parseInteger' (acc*10 + 6) t
        | '7' :: t  -> parseInteger' (acc*10 + 7) t
        | '8' :: t  -> parseInteger' (acc*10 + 8) t
        | '9' :: t  -> parseInteger' (acc*10 + 9) t
        | [] -> [], acc
        | t -> t, acc
    let chars = List.ofArray(str.ToString().ToCharArray()) 
    parseInteger' 0 chars

let testParse s i r = 
    let a,b = parseInteger s
    assert  (a=r && b = i)
    
[<EntryPoint>]
let main argv = 
    testParse "111" 111 []
    testParse "123" 123 []
    testParse "1a" 1 ['a']
    testParse "0" 0 []

    
    0 // return an integer exit code
