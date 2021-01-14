open System
open System.IO
open System.Text.RegularExpressions

type Operation =
| TurnOn
| TurnOff
| Toggle

type Instruction = {
    TopLeft : int * int
    BottomRight : int * int
    Operation : Operation
}

let parse line =
    let m = Regex.Match(line, @"^([^\d]*)(\d+),(\d+) through (\d+),(\d+)$")
    let op = match m.Groups.[1].Value with
             | "turn on " -> TurnOn
             | "turn off " -> TurnOff
             | "toggle " -> Toggle
             | _ -> failwith "invalid input"

    { 
        TopLeft = (Int32.Parse m.Groups.[2].Value, Int32.Parse m.Groups.[3].Value)
        BottomRight = (Int32.Parse m.Groups.[4].Value, Int32.Parse m.Groups.[5].Value)
        Operation = op
    }

let exec1 instruction (lights : int[,]) =
    let xs, ys = instruction.TopLeft
    let xe, ye = instruction.BottomRight
    for x in xs..xe do
        for y in ys..ye do
            match instruction.Operation with
            | TurnOn -> lights.[x, y] <- 1
            | TurnOff -> lights.[x, y] <- 0
            | Toggle -> lights.[x, y] <- if lights.[x, y] = 0 then 1 else 0
    lights

let lights = Array2D.create 1000 1000 0

let instructions = File.ReadAllLines "06-fire-hazard-input.txt"
                   |> Array.map parse
                   |> List.ofArray

let rec processInstructions exec instructions lights =
    match instructions with
    | [] -> lights
    | h :: t -> processInstructions exec t (exec h lights)

let toSeq arr = seq {
                      for x in 0..(Array2D.length1 arr - 1) do
                          for y in 0..(Array2D.length2 arr - 1) do
                              yield arr.[x, y]
                  }

let after = processInstructions exec1 instructions lights

let result1 = after
              |> toSeq
              |> Seq.filter (fun l -> l = 1)
              |> Seq.length

let exec2 instruction (lights : int[,]) =
    let xs, ys = instruction.TopLeft
    let xe, ye = instruction.BottomRight
    for x in xs..xe do
        for y in ys..ye do
            match instruction.Operation with
            | TurnOn -> lights.[x, y] <- lights.[x, y] + 1
            | TurnOff -> lights.[x, y] <- max (lights.[x, y] - 1) 0
            | Toggle -> lights.[x, y] <- lights.[x, y] + 2
    lights

let lights2 = Array2D.create 1000 1000 0

let after2 = processInstructions exec2 instructions lights2

let result2 = after2
              |> toSeq
              |> Seq.sum
