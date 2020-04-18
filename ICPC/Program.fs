module ICPC
open System

type SplitOption =
  | Default
  | RemoveEmptyOptions
  
type PunctuationIndex =
  | Nothing
  | End
  | Invalid

type TokenCase<'a> =
  | Add of 'a
  | Skip
  | Error

type Token = {
  input: string
  word: string
  preceded: bool
  succeeded: bool
}

let COMMA, SPACE, PERIOD = ',', ' ', '.'

/// Given a char separator and string, it splits it up into List of parts
let stringSplit (input:string) (separator:char) (splitOption: SplitOption) = 
  let words = input.Split separator |> List.ofArray
  match splitOption with
  | Default -> words
  | RemoveEmptyOptions -> words |> List.filter (fun x-> String.length x > 0)

/// Check if last sentence is terminated with a period
let isStringTerminationValid (input:string) =
  let periodIndex = input.LastIndexOf PERIOD
  periodIndex = (String.length input - 1)

/// Check if sentence meets the minimum length requirements
let isInRangeOfMinRequirement (input:string) (minLength:int) =
  match String.length input >= minLength with 
  | true -> System.Char.IsLetter input.[0]
  | _ -> false
    
/// Only lowercase letter, comma, period and space are vaild charactors
let isValidChar (ch:char) = 
  match  System.Char.IsLetter ch with
  | true -> System.Char.IsLower ch
  | _ -> ",. ".Contains((string ch))

/// validates a complete string to check it has valid chars
let validCharsInString (input:string) =
  String.forall isValidChar input

/// check if the string has a punctuation and if exists; return the index
let punctuationPosition (input:string) (punctuation:char) =
  let str = input.Trim()
  let lastIndex = (str.IndexOf punctuation) = ((String.length str) - 1)
  let missing = (str.IndexOf punctuation) = -1
  match missing, lastIndex with 
  | true, _ -> PunctuationIndex.Nothing
  | _, true -> PunctuationIndex.End
  | _ -> PunctuationIndex.Invalid

/// Given a string return only the word without any comma or period
let getWordOnly (input:string) =
  String.filter Char.IsLetter input

/// easy utility to create a single token instance
let createToken (input:string) (preceded:bool) (succeeded:bool) =
  let sanitizedInput = getWordOnly input
  Add { input=input; word=sanitizedInput; preceded=preceded; succeeded=succeeded }

/// create word token -- TODO
let wordTokenBuilder (prevWord: string) (current:string) = 
  let prevWordToken = punctuationPosition prevWord COMMA
  let wordToken = punctuationPosition current COMMA

  match prevWordToken, wordToken with
  | End, End -> createToken current true true
  | Nothing, End -> createToken current false true
  | End, Nothing  -> createToken current true false
  | Nothing, Nothing -> Skip
  | _ -> Error

/// validates whether the string is properly punctuated
let isPunctuationValid (words: string list) =
  let predicate (punctuation:char) (word:string) =
    let punctuationCount = 
      String.filter Char.IsPunctuation word 
      |> String.length 
      |> (fun count -> count <= 1)

    let validIndex =
      match punctuationPosition word punctuation with
      | Nothing | End -> true
      | _ -> false

    not (validIndex && punctuationCount)

  match List.tryFind (predicate COMMA) words with
  | Some _ -> false
  | _ -> true

/// Checks for empty entries or entries with punctuation only
let emptyEntriesOrPunctuationOnly (words: string list) =
  let isNullOrWhiteSpace = 
    match List.filter String.IsNullOrWhiteSpace words with
    | [] -> false
    | head::tail -> true

  let predicate (word:string) =
    Char.IsPunctuation word.[0]

  let singleCharString = List.filter (fun word -> String.length word = 1) words
  match List.tryFind predicate (singleCharString) with
  | None -> isNullOrWhiteSpace
  | Some _ -> true

//let updateWatchList (wList: Token list) (prevWord: string) (current:string) =
//  let token = wordTokenBuilder prevWord current

// /// create a watchlist
// let createWatchList (words:string list) =
//  let rec helper _in wList prevItem = 
//    match _in with
//    | [] -> wList
//    | elem::[] -> updateWatchList wList prevItem elem
//    | elem::rest -> 
//      let watchList = updateWatchList wList prevItem elem
//      helper rest watchList elem
//  helper words [] String.Empty

/// Validates the cse if its valid
let isValidCase (input:string) =
  let words = stringSplit input SPACE Default
  let isValid =
    true 
    |> (&&) (validCharsInString input)
    |> (&&) (isStringTerminationValid input)
    |> (&&) (isInRangeOfMinRequirement input 2)
    |> (&&) (isPunctuationValid words)
    |> (&&) (not (emptyEntriesOrPunctuationOnly words))

  match isValid with
  | false -> None
  | _ -> Some input

let commaSprinkler (input:string) =
  //stringSplit ' ' input
  isValidCase input

let rivers (input:string) =
  failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
