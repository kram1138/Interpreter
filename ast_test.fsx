#load "lexing.fsx"
#load "ast.fsx"
open Ast
#r "nuget: Expecto, 9.0.4"
open Expecto
open System

runTestsWithCLIArgs [] [||] (testList "Print program" [
    test "Print" {
        let actual = printProgram [
            LetStatement {name = "abc"; value = IntExpression 4}
            ReturnStatement (IdentifierExpression "sft")
            ExpressionStatement (IntExpression 4)
            ExpressionStatement (PrefixExpression { operator = MinusPrefix; value = IntExpression 4 })
            ExpressionStatement (PrefixExpression { operator = NotPrefix; value = IntExpression 4 })
            ExpressionStatement (InfixExpression { left = IntExpression 4; operator = PlusInfix; right = IntExpression 4 })
            ExpressionStatement (InfixExpression {
                left = IntExpression 5;
                operator = MultInfix;
                right = InfixExpression {left = IntExpression 4; operator = PlusInfix; right = IntExpression 4}
            })
            ExpressionStatement (IfExpression {
                condition = BoolExpression true
                consequence = [
                    ExpressionStatement (IdentifierExpression "abc")
                ]
                alternative = None
            })
            ExpressionStatement (IfExpression {
                condition = BoolExpression true
                consequence = [
                    ExpressionStatement (IdentifierExpression "abc")
                ]
                alternative = Some [
                    ExpressionStatement (IdentifierExpression "xyz")
                ]
            })
            ExpressionStatement (FunctionLiteral {
                paramList = [ "a"; "b"]
                body = [
                    ExpressionStatement (IdentifierExpression "b")
                ]
            })
            ExpressionStatement (FunctionCall {
                fn = IdentifierExpression "foo"
                args = [
                    IdentifierExpression "bar"
                    IdentifierExpression "fizz"
                ]
            })
        ]
        let expected = """
let abc = 4;
return sft;
4
(-4)
(!4)
(4 + 4)
(5 * (4 + 4))
if true { abc }
if true { abc } else { xyz }
fn a, b { b }
foo(bar, fizz)
"""
        Expect.equal actual (expected.Trim()) ""
    }
])
