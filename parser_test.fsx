#load "ast.fsx"
#load "parser.fsx"
open Parser
open Lexing
open Ast
#r "nuget: Expecto, 9.0.4"
open Expecto

let tests =
    testList "Parser" [

        test "Let statements" {
            let input = """
            let x = 5;
            let y = true
            let foobar = y;
            """
            let tokens = lex input
            let actual = parse tokens
            let expected = Ok [
                LetStatement { name = "x"; value = IntExpression 5}
                LetStatement { name = "y"; value = BoolExpression true}
                LetStatement { name = "foobar"; value = IdentifierExpression "y"}
            ]
            
            Expect.equal actual expected "Matches simple function"
        }
        
        test "Return statements" {
            let input = """
            return 5;
            return true
            return foo;
            """
            let tokens = lex input
            let actual = parse tokens
            let expected = Ok [
                ReturnStatement (IntExpression 5)
                ReturnStatement (BoolExpression true)
                ReturnStatement (IdentifierExpression "foo")
            ]
            
            Expect.equal actual expected "Matches simple function"
        }
        
        test "Identifier expressions" {
            let input = """foobar"""
            let tokens = lex input
            let actual = parse tokens
            let expected = Ok [
                ExpressionStatement (IdentifierExpression "foobar")
            ]
            
            Expect.equal actual expected ""
        }
        
        test "Boolean expressions" {
            [
                "true", [ExpressionStatement (BoolExpression true)]
                "false", [ExpressionStatement (BoolExpression false)]
                "let a = true;", [LetStatement {name = "a"; value = BoolExpression true}]
                "let b = false;", [LetStatement {name = "b"; value = BoolExpression false}]
            ]
            |> List.iter (fun (input, expected) ->
                let tokens = lex input
                let actual = parse tokens
                
                Expect.equal actual (Ok expected) ""
            )
        }
        
        test "Prefix operators" {
            [
                "!5", [ ExpressionStatement (PrefixExpression { operator = NotPrefix; value = IntExpression 5})]
                "-foo", [ ExpressionStatement (PrefixExpression { operator = MinusPrefix; value = IdentifierExpression "foo"})]
            ] |> List.iter (fun (input, expected) ->
                let tokens = lex input
                let actual = parse tokens
                
                Expect.equal actual (Ok expected) ""
            )
        }

        test "Operator precedence" {
            [
                ("-a * b","((-a) * b)")
                ("!-a", "(!(-a))")
                ("a + b + c", "((a + b) + c)")
                ("a + b - c", "((a + b) - c)")
                ("a * b * c", "((a * b) * c)")
                ("a * b / c", "((a * b) / c)")
                ("a + b / c", "(a + (b / c))")
                ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)")
                ("3 + 4; -5 * 5", "(3 + 4)\n((-5) * 5)")
                ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")
                ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")
                ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
                ("true", "true" )
                ("false", "false" )
                ("3 > 5 == false", "((3 > 5) == false)" )
                ("3 < 5 == true", "((3 < 5) == true)" )
                ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")
                ("(5 + 5) * 2", "((5 + 5) * 2)")
                ("2 / (5 + 5)", "(2 / (5 + 5))")
                ("-(5 + 5)", "(-(5 + 5))")
                ("!(true == true)", "(!(true == true))")
                ("add(b * c) + d", "(add((b * c)) + d)")
                ("a + add(b * c) + d", "((a + add((b * c))) + d)")
                ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")
                ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))")
                ("add(s, -add(add(-3) - -add()))", "add(s, (-add((add((-3)) - (-add())))))")
            ] |> List.iter (fun (input, expected) ->
                let tokens = lex input
                let actual =
                    match parse tokens with
                    | Ok result -> printProgram result
                    | Error e -> e
                Expect.equal actual expected ""
            )
        }
        
        test "If expressions" {
            let xgty = InfixExpression { left = IdentifierExpression "x"; operator = GTInfix; right = IdentifierExpression "y"}
            [
                "if x > y { x }", [ ExpressionStatement
                    (IfExpression {
                        condition = xgty
                        consequence = [ExpressionStatement (IdentifierExpression "x")]
                        alternative = None
                    })
                ]
                "if (x > y) { x } else { y }", [ ExpressionStatement
                    (IfExpression {
                        condition = xgty
                        consequence = [ExpressionStatement (IdentifierExpression "x")]
                        alternative = Some [ExpressionStatement (IdentifierExpression "y")]
                    })
                ]
            ] |> List.iter (fun (input, expected) ->
                let tokens = lex input
                let actual = parse tokens
                
                Expect.equal actual (Ok expected) ""
            )
        }

        test "Function definitions" {
            [
                "fn x, y {x + y}", [ ExpressionStatement (FunctionLiteral {
                        paramList = [ "x"; "y"]
                        body = [
                            ExpressionStatement (InfixExpression {
                                left = IdentifierExpression "x"
                                right = IdentifierExpression "y"
                                operator = PlusInfix
                            })
                        ]
                    })
                ]
            ] |> List.iter (fun (input, expected) ->
                let tokens = lex input
                let actual = parse tokens
                
                Expect.equal actual (Ok expected) ""
            )
        }

        test "Function params" {
            [
                "fn {}", []
                "fn a {}", ["a"]
                "fn a, b {}", ["a"; "b"]
                "fn a, b, c {}", ["a"; "b"; "c"]
            ] |> List.iter (fun (input, paramList) ->
                let tokens = lex input
                let actual = parse tokens
                let expected = [ ExpressionStatement (FunctionLiteral {
                        paramList = paramList
                        body = []
                    })
                ]
                Expect.equal actual (Ok expected) ""
            )
        }

        test "Function calls" {
            [
                "add(1, 2*3)", [ ExpressionStatement (FunctionCall {
                        fn = IdentifierExpression "add"
                        args = [
                            IntExpression 1
                            InfixExpression {
                                left = IntExpression 2
                                right = IntExpression 3
                                operator = MultInfix
                            }
                        ]
                    })
                ]
            ] |> List.iter (fun (input, expected) ->
                let tokens = lex input
                let actual = parse tokens
                
                Expect.equal actual (Ok expected) ""
            )
        }
    ]
  
runTestsWithCLIArgs [] [||] tests