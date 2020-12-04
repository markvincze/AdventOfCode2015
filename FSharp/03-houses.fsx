open System.IO

let input = File.ReadAllText "03-houses-input.txt"

let move dir (x, y) = match dir with
                      | '^' -> (x, y + 1)
                      | '>' -> (x + 1, y)
                      | 'v' -> (x, y - 1)
                      | '<' -> (x - 1, y)
                      | _ -> failwith "Invalid input"

let rec travel input index pos visited =
    let visited = Set.add pos visited
    if index >= String.length input
    then visited
    else travel input (index + 1) (move input.[index] pos) visited

let result1 = travel input 0 (0, 0) Set.empty<int * int>
              |> Set.count

let rec travelWithRobo input index santaPos roboPos visited =
    let visited = visited |> Set.add santaPos |> Set.add roboPos
    if index >= String.length input
    then visited
    else if index % 2 = 0
         then travelWithRobo input (index + 1) (move input.[index] santaPos) roboPos visited
         else travelWithRobo input (index + 1) santaPos (move input.[index] roboPos) visited

let result2 = travelWithRobo input 0 (0, 0) (0, 0) Set.empty<int * int>
              |> Set.count