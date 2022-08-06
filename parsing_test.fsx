#load "parsing.fsx"
open Parsing
open Lexing
#r "nuget: Expecto, 9.0.4"
open Expecto

let tests =
    testList "General parsing" [
        test "Errors" {
            let input = """
            let let true false
            """
            let tokens = lex input
            let actual =
                any [
                    (parseToken Let) .>>. (parseToken Let) .>> (parseToken False)
                    (parseToken Let) .>>. (parseToken False)
                ] <| tokens
            let expected = 
                Success ([EOF], (Let, Let))
            
            Expect.equal actual expected "Matches simple function"
        }
    ]
  
runTestsWithCLIArgs [] [||] tests