// For more information see https://aka.ms/fsharp-console-apps
open System

let F x =
    seq {
        let mutable A = 0
        let mutable S = 0
        for i in 1..x do
            printfn "Последовательно вводите числа: "
            S <- int(Console.ReadLine())
            A <- (S % 10)
            yield A
    }

let D1 x =
    let rec loop x min =
        if x = 0 then min
        else
            let d = x % 10
            let newm = if d < min then d else min
            loop (x / 10) newm
    loop x 9

let add (a1: float, b1: float) (a2: float, b2: float) =
    (a1 + a2, b1 + b2)

let sub (a1: float, b1: float) (a2: float, b2: float) =
    (a1 - a2, b1 - b2)

let mul (a1: float, b1: float) (a2: float, b2: float) =
    (a1 * a2 - b1 * b2, a1 * b2 + a2 * b1)

let div (a1: float, b1: float) (a2: float, b2: float) =
    let d = a2 * a2 + b2 * b2
    ((a1 * a2 + b1 * b2) / d, (b1 * a2 - a1 * b2) / d)

let rec pow (a: float, b: float) n =
    match n with
    | 0 -> (1.0, 0.0)
    | 1 -> (a, b)
    | _ when n > 0 ->
        let (a_pow, b_pow) = pow (a, b) (n - 1)
        mul (a, b) (a_pow, b_pow)
    | _ -> failwith "Отрицательные степени не поддерживаются"

[<EntryPoint>]
let main _ =
    printfn "Задание 1. Генерация списков
-> Последовательно вводя числа, сформировать список из их последних цифр: "
    printfn "Количество чисел: "
    let N = int(Console.ReadLine())
    printfn "Список из их последних цифр: %A" (F N |> Seq.toList)

    printfn "Задание 2. Рекурсия
-> Найти минимальную цифру натурального числа: "
    printfn "Введите натуральное число: "
    let N1 = int(Console.ReadLine())
    printfn "Минимальная цифра: %d" (D1 N1)

    0

