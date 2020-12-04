open System
open System.IO

let parse (l : string) =
    let s = l.Split 'x' |> Array.map Int32.Parse
    (s.[0], s.[1], s.[2])


let boxes = File.ReadAllLines "02-math-input.txt"
            |> Array.map parse

let calcPaper (a, b, c) =
    let s1 = a * b
    let s2 = a * c
    let s3 = b * c

    let mins = [s1; s2; s3] |> List.min

    s1 * 2 + s2 * 2 + s3 * 2 + mins

let result1 = boxes
              |> Array.sumBy calcPaper

let calcRibbon (a, b, c) =
    let [s1; s2] = [a; b; c] |> List.sort |> List.take 2

    s1 * 2 + s2 * 2 + a * b * c

let result2 = boxes
              |> Array.sumBy calcRibbon