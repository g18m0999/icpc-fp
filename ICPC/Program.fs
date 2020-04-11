module ICPC
open System

/// Given a char separator and string, it splits it up into List of parts
let stringSplit (separator:char) (input:string) = 
  input.Split separator |> List.ofArray |> List.filter (fun x-> String.length x > 0)

/// Check if last sentence is terminated with a period
let isStringTerminationValid (input:string) =
  let periodIndex = input.LastIndexOf '.'
  periodIndex = (String.length input - 1)

/// Given a sentance/paragraph as a single string, it finds length of the longest word
let findLongestWordLength (input:string) =
  let largestNumber (iLen:int) (cLen:int) = 
    match iLen < cLen with
    | true -> cLen
    | _ -> iLen

  let words = stringSplit ' ' input
  List.fold (fun maxLength word -> largestNumber maxLength (String.length word) ) 0 words

/// Only lowercase letter, comma, period and space are vaild charactors
let isValidCase (input:string) =
  let isValidChar (ch:char) = 
    match  System.Char.IsLetter ch with
    | true -> System.Char.IsLower ch
    | _ -> ",. ".Contains((string ch))
  
  let isInRangeAndfirstCharIsLetter =
    match String.length input >= 2 with 
    | true -> System.Char.IsLetter input.[0]
    | _ -> false

  let allowedChars = String.forall isValidChar input
  let isTerminationsValid = isStringTerminationValid input

  allowedChars && isInRangeAndfirstCharIsLetter && isTerminationsValid

let commaSprinkler (input:string) =
  //stringSplit '.' input
  isValidCase input

let rivers (input:string) =
  failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
