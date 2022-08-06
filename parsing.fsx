#load "lexing.fsx"
open Lexing
open Microsoft.FSharp.Reflection

let nameOf (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name  

type ParseResult<'T> =
| Success of list<Token> * 'T
| ParseError of string * int

type Parser<'T> = list<Token> -> ParseResult<'T>

let parseToken t (tokens: Token list) =
    if tokens.Head = t then Success (tokens[1..], tokens.Head)
    else ParseError (nameOf tokens[0], 0)

let (>>=) p1 mapping tokens =
    match p1 tokens with
    | Success (tokens, result) -> Success (tokens, mapping result)
    | ParseError (e, d) -> ParseError (e, d)
let (>==) p1 result tokens =
    match p1 tokens with
    | Success (tokens, _) -> Success (tokens, result)
    | ParseError (e, d) -> ParseError (e, d)

let (??=) p1 mapping tokens =
    match p1 tokens with
    | Success (t, r) -> Success (t, r)
    | ParseError (e, d) -> ParseError (mapping e, d)
let (?==) p1 error tokens =
    match p1 tokens with
    | Success (t, r) -> Success (t, r)
    | ParseError (_, d) -> ParseError (error, d)

let (.>>.) p1 p2 =
    let combined tokens =
        match p1 tokens with
        | Success (tokens, result1) ->
            match p2 tokens with
            | Success (tokens, result2) ->
                Success (tokens, (result1, result2))
            | ParseError (e, d) -> ParseError (e, d + 1)
        | ParseError (e, d) -> ParseError (e, d)
    combined
let (>>.) (p1: Parser<'T>) (p2: Parser<'U>) = p1 .>>. p2 >>= (fun (_, result) -> result)
let (.>>) (p1: Parser<'T>) (p2: Parser<'U>) = p1 .>>. p2 >>= (fun (result, _) -> result)

let any (parsers: Parser<'T> list) tokens =
    let rec parseP i (errors: (string * int) list) =
        match parsers |> List.tryItem i with
        | Some p ->
            match p tokens with
            | Success (tokens, result) -> (Success (tokens, result), [])
            | ParseError (e, d) -> parseP (i + 1) ((e, d)::errors)
        | None -> (ParseError ("No matching parser", 0), errors)
    let result, errors = parseP 0 []
    match result with
    | Success (t, r) -> Success (t, r)
    | _ ->
        ParseError (errors |> List.maxBy (fun (_, d) -> d))

let many (p: Parser<'T>) (tokens: Token list) =
    let rec loop tokens =
        match p tokens with
        | ParseError (e, d) ->
            if d > 0 then ParseError (e, d) else Success (tokens, [])
        | Success (tokens, result) ->
            match loop tokens with
            | Success (t, tail) -> Success(t, result::tail)
            | ParseError (e, d) -> ParseError (e, d)
    loop tokens

let until (p: Parser<'T>) (endToken: Token) (tokens: Token list) =
    let rec loop tokens =
        match p tokens with
        | ParseError (e, d) -> (tokens, [], (e, d))
        | Success (tokens, result) ->
            let (t, tail, e) = loop tokens  
            (t, result::tail, e)
    match loop tokens with
    | (head::tail, result, (e, d)) ->
        if head = endToken then Success (tail, result)
        else
            if d > 0 then ParseError (e, d)
            else ParseError ($"Expected {nameof endToken}, recieved {nameOf head}", tokens.Length - tail.Length)
    | ([], _, _) -> ParseError ("Unexpected end of input", tokens.Length)

let between startToken (p: Parser<'T>) endToken =
    (parseToken startToken) >>. p .>> (parseToken endToken)

let sepBy (sep: Parser<'a>) (p: Parser<'T>) (tokens: Token list) =
    let rec loop tokens =
        match p tokens with
        | ParseError _ -> (tokens, [])
        | Success (tokens, result) ->
            match sep tokens with
            | ParseError _ -> (tokens, [result])
            | Success (tokens, _) ->
                let (t, tail) = loop tokens  
                (t, result::tail)
    Success (loop tokens)

let (?>) p tokens =
    match p tokens with
    | Success (tokens, result) -> Success (tokens, Some result)
    | ParseError _ -> Success (tokens, None)

let (<|>) p1 p2 tokens =
    match p1 tokens with
    | Success (tokens, result) -> Success (tokens, result)
    | ParseError (e1, d1) ->
        match p2 tokens with
        | Success (tokens, result) -> Success (tokens, result)
        | ParseError (e2, d2) ->
            ParseError (if d1 > d2 then (e2, d2) else (e1, d1))

let parseNext parse  error (tokens: Token list)=
    match tokens with
    | token::tokens ->
        match parse token with
        | None -> ParseError ($"Expected a '{error}', but got a {nameOf token}", 0)
        | Some result -> Success (tokens, result)
    | _ -> ParseError ($"Expected a '{error}', but reached end of input", 0)
