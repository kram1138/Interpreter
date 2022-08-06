#load "lexing.fsx"
open Lexing
#r "nuget: Expecto, 9.0.4"
open Expecto

let tests =
    testList "Lexer" [
        test "A simple test" {
            let input = "=+(){},;"

            let actual = lex input
            let expected = [Assign; Plus; LParen; RParen; LBrace; RBrace; Comma; Semicolon; EOF]
            Expect.equal actual expected "Matches simple symbols"
        }
        test "Test lookahead" {
            let input = """
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
        x + y;
    };
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;
    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;
            """

            let actual = lex input
            let expected = [
                Let
                Ident "five";
                Assign
                Int 5
                Semicolon
                Let
                Ident "ten"
                Assign
                Int 10
                Semicolon
                Let
                Ident "add"
                Assign
                Function
                LParen
                Ident "x"
                Comma
                Ident "y"
                RParen
                LBrace
                Ident "x"
                Plus
                Ident "y"
                Semicolon
                RBrace
                Semicolon
                Let
                Ident "result"
                Assign
                Ident "add"
                LParen
                Ident "five"
                Comma
                Ident "ten"
                RParen
                Semicolon
                Bang
                Minus
                Slash
                Asterisk
                Int 5
                Semicolon
                Int 5
                LT
                Int 10
                GT
                Int 5
                Semicolon
                If
                LParen
                Int 5
                LT
                Int 10
                RParen
                LBrace
                Return
                True
                Semicolon
                RBrace
                Else
                LBrace
                Return
                False
                Semicolon
                RBrace
                Int 10
                Eq
                Int 10
                Semicolon
                Int 10
                NotEq
                Int 9
                Semicolon
                EOF
                ]
            Expect.equal actual expected "Matches simple function"
        }
    ]
  
runTestsWithCLIArgs [] [||] tests