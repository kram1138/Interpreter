#load "parsing.fsx"
#load "list_helper.fsx"

open Lexing
open Ast
open Parsing

let parseIdent =
    let p token =
        match token with
        | Ident name -> Some name
        | _ -> None
    parseNext p "identifier"
    
let parseInt =
    let p token =
        match token with
        | Int i -> Some (IntExpression i)
        | _ -> None
    parseNext p "int"

let parseBool =
    let p token =
        match token with
        | True -> Some (BoolExpression true)
        | False -> Some (BoolExpression false) 
        | _ -> None
    parseNext p "int"

let rec toExpr left (right: (InfixOperator * Expression) list) =
    match right with
    | [] -> left
    | (op, e)::rest ->
        toExpr (InfixExpression { left = left; operator = op; right = e})  rest

let rec parseExpression tokens =
    let parseLiteral = any [
        parseInt
        parseBool
        parseIdent >>= IdentifierExpression
    ]

    let parseInfix (ops: (Token * InfixOperator * string) list) baseParser =
        let parseOperatorTokens =
            any (ops |> List.map 
                (fun (token, op, e) ->
                    (parseToken token) >== op ?== e)
            )
        let expr =
            baseParser .>>. many (parseOperatorTokens .>>. baseParser ?== "Expected right side of infix op") 
            >>= fun (left, rest) -> toExpr left rest
        expr

    let operators = [
        [(Eq, EqInfix, "=="); (NotEq, NEqInfix, "!=")]
        [(GT, GTInfix, ">"); (LT, LTInfix, "<")]
        [(Minus, MinusInfix, "-"); (Plus, PlusInfix, "+")]
        [(Asterisk, MultInfix, "*"); (Slash, DivInfix, "/")]
    ]

    let rec parseInfixOperators ops tokens =
        match ops with
        | head::rest ->
            parseInfix head (parseInfixOperators rest) tokens
        | _ -> parsePrefixOperator tokens
    and parsePrefixOperator tokens =
        let op = any [
            any [
                (parseToken Bang) >== NotPrefix
                (parseToken Minus) >== MinusPrefix
            ]
            .>>. parsePrefixOperator
            >>= (fun (token, e) -> PrefixExpression { operator = token; value = e})
            ?== "Expected an expression after prefix"
            parseFunctionCall
        ]
        op tokens
    and parseFunctionCall tokens =
        parseBaseExpression
        .>>. ((?>) (between 
            LParen
            (sepBy (parseToken Comma) parseExpression) 
            RParen))
        >>= fun (e, a) ->
            match a with
            | Some args -> FunctionCall { fn = e; args = args}
            | None -> e
        <| tokens
    and parseIfExpression tokens =
        (parseToken If)
        >>. parseExpression ?== "Expected a condition for if statement"
        .>>. between LBrace (many parseStatement) RBrace
        .>>. (?>) (
            (parseToken Else)
            >>. between LBrace (many parseStatement) RBrace
        )
        >>= fun ((condition, consequence), alternative) ->
            IfExpression {
                condition = condition
                consequence = consequence
                alternative = alternative
            }
        <| tokens
    and parseFunctionLiteral tokens =
        (parseToken Function)
        >>. sepBy (parseToken Comma) parseIdent
        .>>. between LBrace (many parseStatement) RBrace
        >>= fun (paramList, body) ->
            FunctionLiteral {
                paramList = paramList
                body = body
            }
        <| tokens
    and parseBaseExpression tokens =
        any [
            parseLiteral
            parseIfExpression
            parseFunctionLiteral
            between LParen parseExpression RParen
        ] tokens
    and parseExpression tokens = parseInfixOperators operators tokens
    parseExpression tokens

and parseLet =
    (parseToken Let)
    >>. (parseIdent ?== "Expected an identifier after a let")
    .>> (parseToken Assign ?== "Expected an '='")
    .>>. (parseExpression ?== "No unassigned variable allowed")
    .>> ((?>) (parseToken Semicolon))
    >>= (fun (name, value) -> LetStatement { name = name; value = value })

and parseReturn =
    (parseToken Return)
    >>. parseExpression ?== "Return statements must return a value"
    .>> ((?>) (parseToken Semicolon))
    >>= (fun e -> ReturnStatement e)

and parseStatement =
    any [
        parseLet
        parseReturn
        parseExpression .>> ((?>) (parseToken Semicolon)) >>= ExpressionStatement
    ]

let parse tokens : Result<Program, string> =
    match until parseStatement EOF tokens with
    | Success (_, result) -> Ok result
    | ParseError (e,  _) ->
        Error e
