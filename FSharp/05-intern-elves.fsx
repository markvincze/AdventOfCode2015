open System
open System.IO

let input = File.ReadAllLines "05-intern-elves-input.txt"

let isVowel c =
    c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

let isNice str = 
    let rec isNice vowelCount hadDoubleLetters str =
        match str with
        | a :: b :: t -> match (a, b) with
                         | 'a', 'b' | 'c', 'd' | 'p', 'q' | 'x', 'y' -> false
                         | _ -> let vowelCount = if isVowel a then vowelCount + 1 else vowelCount
                                let hadDoubleLetters = if a = b then true else hadDoubleLetters
                                isNice vowelCount hadDoubleLetters (b :: t)
        | [a] -> let vowelCount = if isVowel a then vowelCount + 1 else vowelCount
                 hadDoubleLetters && vowelCount >= 3
    
    isNice 0 false (List.ofSeq str)

let result1 = input
              |> Array.filter isNice
              |> Array.length

let isNice2 str =
    let rec isNice2 index segments cond2 str =
        match str with
        | a :: b :: rest -> let cond2 = match rest with
                                        | h :: t when a = h -> true
                                        | _ -> cond2
                            let segments = Map.change
                                               (a.ToString() + b.ToString())
                                               (fun indices -> match indices with
                                                               | None -> Some [ index ]
                                                               | Some ii -> Some (index :: ii))
                                                segments
                            isNice2 (index + 1) segments cond2 (b :: rest)
        | [a] -> let cond1 = segments
                             |> Seq.exists (fun kvp -> (List.max kvp.Value) - (List.min kvp.Value) > 1)
                 cond1 && cond2

    isNice2 0 Map.empty false (List.ofSeq str)

let result2 = input
              |> Array.filter isNice2
              |> Array.length
