open FParsec

type Expression =
    | Plus of Expression * Expression
    | Minus of Expression * Expression
    | Multiplication of Expression * Expression
    | Division of Expression * Expression
    | Negation of Expression
    | Number of int

type E =
    | E of Term * E'
and E' =
    | Plus of Term * E'
    | Minus of Term * E'
    | Epsilon
and Term = 
    | Term of Factor * Term'
and Term' =
    | Multiplication of Factor * Term'
    | Division of Factor * Term'
    | Epsilon
and Factor =
    | Negation of E
    | Brackets of E
    | Number of int

let rec buildAST expr =
    let buildFactor = function
    | Negation(e) -> Expression.Negation(buildAST e)
    | Brackets(e) -> buildAST e
    | Number(x) -> Expression.Number(x)

    let rec buildTerm' acc = function
    | Multiplication(factor, rest) -> buildTerm' (Expression.Multiplication(acc, buildFactor factor)) rest
    | Division(factor, rest) -> buildTerm' (Expression.Division(acc, buildFactor factor)) rest
    | Epsilon -> acc

    let buildTerm (Term(factor, rest)) = buildTerm' (buildFactor factor) rest

    let rec buildE' acc = function
    | Plus(factor, rest) -> buildE' (Expression.Plus(acc, buildTerm factor)) rest
    | Minus(factor, rest) -> buildE' (Expression.Minus(acc, buildTerm factor)) rest
    | E'.Epsilon -> acc

    let buildE (E(term, rest)) = buildE' (buildTerm term) rest

    buildE expr

[<EntryPoint>]
let main argv =
    let number = 
        many1 digit 
        |>> (List.fold (fun acc x -> acc * 10 + int (x.ToString())) 0 >> Number)

    let (!) parser = parser .>> spaces

    let e, eRef = createParserForwardedToRef()

    let factor = !(pchar '-') >>. !e |>> Negation
                 <|> (!(pchar '(') >>. !e .>> !(pchar ')') |>> Brackets)
                 <|> number

    let term', term'Ref = createParserForwardedToRef()

    term'Ref := !(pchar '*') >>. !factor .>>. !term' |>> Multiplication
                <|> (!(pchar '/') >>. !factor .>>. !term' |>> Division)
                <|> preturn Epsilon

    let term = !factor .>>. !term' |>> Term

    let e', e'Ref  = createParserForwardedToRef()

    e'Ref :=
        !(pchar '+') >>. !term .>>. !e' |>> Plus
        <|> (!(pchar '-') >>. !term .>>. !e' |>> Minus)
        <|> preturn E'.Epsilon

    eRef := !term .>>. !e' |>> E

    let testInput = "1 * 2 + 3"

    let result = testInput |> run e
    printfn "%A" result

    match result with
    | Success(result, _, _) -> printfn "%A" <| buildAST result
    | _ -> printfn "%A" result

    0 
