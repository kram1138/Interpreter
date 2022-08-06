#load "lexing.fsx"
#load "ast.fsx"
#load "parser.fsx"

open Lexing
open System
open Parser
open Ast

let startRepl () =
    while true do
        printf ">> "
        let text = Console.ReadLine()
        let tokens = lex text
        let result =
            match parse tokens with
            | Ok result -> printProgram result
            | Error e -> e
        printfn "%s" result