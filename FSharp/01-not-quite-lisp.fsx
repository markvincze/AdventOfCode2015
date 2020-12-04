open System
open System.IO

let input = File.ReadAllText "01-not-quite-lisp-input.txt"

let rec calcFloor str pos floor =
    if pos >= String.length str
    then floor
    else match str.[pos] with
         | '(' -> calcFloor str (pos + 1) (floor + 1)
         | ')' -> calcFloor str (pos + 1) (floor - 1)
         | _ -> failwith "Invalid input"

let result1 = calcFloor input 0 0

let rec findBasement (str : string) pos floor =
    if floor = -1
    then pos
    else match str.[pos] with
         | '(' -> findBasement str (pos + 1) (floor + 1)
         | ')' -> findBasement str (pos + 1) (floor - 1)
         | _ -> failwith "Invalid input"

let result2 = findBasement input 0 0