#load "list_helper.fsx"
#r "nuget: Expecto, 9.0.4"
open Expecto

let tests =
    testList "foldWithSkip" [

        test "Works" {
            let input = [
                1; 2; 3; 4; 5; 6; 7; 8; 9; 10;
            ]
            let folder acc n =
                if n % 4 = 0 then
                    (acc@[n], 1)
                else
                    (acc@[n], 0)
            let actual = input |> List_helper.foldWithSkip folder []
            let expected = [
                1; 2; 3; 4; 6; 7; 8; 10;
            ]
            
            Expect.equal actual expected "Matches simple function"
        }
    ]
  
runTestsWithCLIArgs [] [||] tests