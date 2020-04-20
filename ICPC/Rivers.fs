namespace Rivers
module Utils =

  open System

  type SplitOption =
    | Default
    | RemoveEmpty
    
  let MAX_WORD_LENGTH, MIN_NUMBER_OF_WORDS = 80, 2

  /// Given a char separator and string, it splits it up into List of parts
  let stringSplit (input:string) (separator:char) (splitOption: SplitOption) = 
    let words = input.Split separator |> List.ofArray
    match splitOption with
    | Default -> words
    | RemoveEmpty -> words |> List.filter (fun x-> String.length x > 0)

  /// Given a sentance/paragraph as a single string, it finds length of the longest word
  let findLongestWordLength (words:string list) =
    let largestNumber (iLen:int) (cLen:int) = 
      match iLen < cLen with
      | true -> cLen
      | _ -> iLen

    List.fold (fun maxLength word -> largestNumber maxLength (String.length word) ) 0 words

  let emptyEntriesExist (words:string list) =
    let result = List.tryFind (fun word -> String.Empty=word) words 
    match result with
    | None -> false
    | Some _ -> true

  let processInput (input:string) =
    let words = stringSplit input ' ' Default
    let longestWord = findLongestWordLength words

    let isInputValid =
      true
      |> (&&) (String.forall Char.IsLetter input)
      |> (&&) (List.length words >= MIN_NUMBER_OF_WORDS)
      |> (&&) (longestWord <= MAX_WORD_LENGTH)
      |> (&&) (emptyEntriesExist words)

    match isInputValid with
    | false -> None
    | true -> (Some (1, 5))


    