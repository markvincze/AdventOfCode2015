open System
open System.Security.Cryptography
open System.Text

let input = "iwrupvqb"

let crypto = new MD5CryptoServiceProvider()

let hash (str : string) =
    let bytes = Encoding.UTF8.GetBytes(str)
    let hashBytes = crypto.ComputeHash bytes
    BitConverter.ToString(hashBytes).Replace("-","")

let rec findSol (start : string) input cnt =
    let hashStr = hash (input + (cnt |> string))
    if hashStr.StartsWith start
    then cnt
    else findSol start input (cnt + 1)

let result1 = findSol "00000" input 0

let result2 = findSol "000000" input 0
