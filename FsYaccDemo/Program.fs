open FSharp.Text.Lexing

[<EntryPoint>]
let main argv =
    let testInput = "1 * 2 + 3"

    let lexbuf = LexBuffer<char>.FromString testInput
    let ast = Parser.expression Lexer.token lexbuf
    printfn "%A" ast
    0 
