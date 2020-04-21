namespace Rivers
module Utils =

  open System

  type RiverInfo = {
    line_width:int
    river_length:int
  }

  type SplitOption =
    | Default
    | RemoveEmpty
    
  let MAX_WORD_LENGTH, MIN_NUMBER_OF_WORDS = 80, 2
  let WHITESPACE = " "

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

  /// Given a list of strings, checks if an empty entries exists 
  let emptyEntriesExist (words:string list) =
    let result = List.tryFind (fun word -> String.Empty=word) words 
    match result with
    | None -> false
    | Some _ -> true

  /// Reshapes the words list by lin-width
  let reshape (lineWidth:int) (words:string list) =
    let rec helper _in _out state =
      match _in with
      | [] -> (_out@[state])
      | entry::rest ->
        let newState = (state+WHITESPACE+entry).Trim()
        match (String.length newState) <= lineWidth with
        | true -> helper rest _out newState
        | false -> helper _in (_out@[state]) WHITESPACE
    helper words [] String.Empty

  let processInput (input:string) =
    let words = stringSplit input ' ' Default


    //let longestWord = findLongestWordLength words

    //let isInputValid =
    //  true
    //  |> (&&) (String.forall Char.IsLetter input)
    //  |> (&&) (List.length words >= MIN_NUMBER_OF_WORDS)
    //  |> (&&) (longestWord <= MAX_WORD_LENGTH)
    //  |> (&&) (emptyEntriesExist words)

    //match isInputValid with
    //| false -> None
    //| true -> (Some (1, 5))



//reshape 15 words

//processInput "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country"
//processInput "When two or more rivers meet at a confluence other than the sea the resulting merged river takes the name of one of those rivers"
    