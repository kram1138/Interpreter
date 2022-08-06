open System

type Token =
// special
| Illegal
| EOF
// keys
| Ident of string
| Int of int
| Function
| Let
| If
| Else
| Return
| True
| False
// operators
| Assign
| Plus
| Minus
| Bang
| Asterisk
| Slash
| LT
| GT
| Eq
| NotEq
// separators
| Comma
| Semicolon
| LParen
| RParen
| LBrace
| RBrace

let twoCharTokens =  [
    "==", Eq
    "!=", NotEq
]

let singleCharTokens = Map [
    '=', Assign
    '+', Plus
    '(', LParen
    ')', RParen
    '{', LBrace
    '}', RBrace
    ',', Comma
    '-', Minus
    '!', Bang
    '*', Asterisk
    '/', Slash
    '<', LT
    '>', GT
    ';', Semicolon
]

let (|SingleCharToken|_|) = singleCharTokens.TryFind

let keywords = Map [
    "let", Let
    "fn", Function
    "true", True
    "false", False
    "if", If
    "else", Else
    "return", Return
]

let lookupIden name =
    keywords.TryFind name |> Option.defaultValue (Ident name)

let (|Letter|_|) (ch: char) =
    if Char.IsLetter ch then Some ch else None
let readIdent (input: string) position =
    let mutable endPos = position + 1
    while endPos < input.Length && Char.IsLetter input[endPos] do endPos <- endPos + 1
    endPos, lookupIden input[position..endPos-1]

let (|Digit|_|) (ch: char) =
    if Char.IsDigit ch then Some ch else None
let readNumber (input: string) position =
    let mutable endPos = position + 1
    while endPos < input.Length && Char.IsDigit input[endPos] do endPos <- endPos + 1
    endPos, int input[position..endPos-1] |> Int

let lex (input: string) : Token list =

    let readSymbol position =
        let mutable startPos = position
        while startPos < input.Length && input[startPos] |> Char.IsWhiteSpace do
            startPos <- startPos + 1

        let (|TwoCharToken|_|) _ =
            twoCharTokens |> List.tryFind (fun (str, tok) ->
                input[startPos..startPos+str.Length-1] = str
            ) |> Option.map (fun (_, tok) -> tok)
        if startPos >= input.Length then
            (0, EOF)
        else
            match input[startPos] with
            | TwoCharToken token -> (startPos + 2, token)
            | SingleCharToken token -> (startPos + 1, token)
            | Letter _ -> readIdent input startPos
            | Digit _ -> readNumber input startPos
            | _ -> (startPos + 1, Illegal)
    
    let mutable (position, symbol) = readSymbol 0
    let mutable symbols: Token list = [symbol]
    while symbol <> EOF do
        let newPosition, newSymbol = readSymbol position
        position <- newPosition
        symbol <- newSymbol
        symbols <-  symbols @ [symbol]
    symbols