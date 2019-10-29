module AnotherFile

open System

let fromAnotherFile() = printfn "Does this run?"

module PlayingAround =
    let add x y = x + y
    let add5 = add 5
    let multiply x y = x * y
    let double = multiply 2
    let add5Double = add5 >> double
    let listApplyFunction = List.map add5Double
    let result = [ 1 .. 100 ] |> listApplyFunction

    let tuple = (1, '2')

    Math.Abs(4) |> ignore
