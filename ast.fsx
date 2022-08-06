type Statement =
| LetStatement of LetStatement
| ReturnStatement of Expression
| ExpressionStatement of Expression
and LetStatement = { name: string; value: Expression }
and PrefixOperator =
| NotPrefix
| MinusPrefix
| PlusPrefix
and PrefixExpression = { operator: PrefixOperator; value: Expression }
and InfixOperator =
| MinusInfix
| PlusInfix
| MultInfix
| DivInfix
| LTInfix
| GTInfix
| EqInfix
| NEqInfix
and InfixExpression = { operator: InfixOperator; left: Expression; right: Expression }
and IfExpression = { condition: Expression; consequence: Statement list; alternative: Statement list option }
and FunctionLiteral = { paramList: string list; body: Statement list}
and FunctionCall = { fn: Expression; args: Expression list}
and Expression =
| IntExpression of int
| BoolExpression of bool
| FunctionLiteral of FunctionLiteral
| FunctionCall of FunctionCall
| IdentifierExpression of string
| PrefixExpression of PrefixExpression
| InfixExpression of InfixExpression
| IfExpression of IfExpression

type Program = Statement list

let prefixOpSymbols = Map [
    NotPrefix, "!"
    MinusPrefix, "-"
    PlusPrefix, "+"
]

let infixOpSymbols = Map [
    MinusInfix, "-"
    PlusInfix, "+"
    MultInfix, "*"
    DivInfix, "/"
    LTInfix, "<"
    GTInfix, ">"
    EqInfix, "=="
    NEqInfix, "!="
]

let rec printExpression e =
    match e with
    | IntExpression i -> $"{i}"
    | BoolExpression b -> $"{b}".ToLower()
    | IdentifierExpression name -> name
    | PrefixExpression p -> $"({prefixOpSymbols.Item p.operator}{printExpression p.value})"
    | InfixExpression e -> $"({printExpression e.left} {infixOpSymbols.Item e.operator} {printExpression e.right})"
    | FunctionCall f -> printFnCall f
    | IfExpression e -> printIf e
    | FunctionLiteral f -> printFnDef f

and printFnDef f =
    let paramList = f.paramList |> String.concat ", "
    let body = $"{{ {printProgram f.body} }}"
    $"fn {paramList} {body}"

and printFnCall f =
    let argList = f.args |> List.map printExpression |> String.concat ", "
    $"{printExpression f.fn}({argList})"

and printIf e =
    let elseBlock =
        match e.alternative with
        | Some a -> $" else {{ {printProgram a} }}"
        | _ -> ""
    $"if {printExpression e.condition} {{ {printProgram e.consequence} }}{ elseBlock }"

and printStatement s =
    match s with
    | LetStatement l -> $"let {l.name} = {printExpression l.value};"
    | ReturnStatement v -> $"return {printExpression v};"
    | ExpressionStatement v -> $"{printExpression v}"
    
and printProgram p =
    p
    |> List.map printStatement
    |> String.concat "\n"
