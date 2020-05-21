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

  let max (a:int) (b:int) =
    match a>b with
    | true -> a
    | _ -> b

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
  let emptyEntriesExist (words:string list) : bool =
    let result = List.tryFind (fun word -> String.Empty=word) words 
    match result with
    | None -> false
    | Some _ -> true

  /// Given two strings, concatenates and returns the new string
  let stringConcat (state:string) (entry:string) =
    (String.concat WHITESPACE (state::entry::[])).Trim()

  /// Checks if a given string has lowercase, uppercase letters and whitespaces (SPACE)
  let isAllowedCharacters (input:string) = 
    let isValid = (fun ch -> Char.IsLetter ch || Char.IsSeparator ch)
    (String.forall isValid input)

  /// Reshapes the words list by lin-width
  let reshape (lineWidth:int) (words:string list) =
    let rec helper _in _out state =
      match _in with
      | [] -> (_out@[state])
      | entry::rest ->
        let newState = stringConcat state entry
        match (String.length newState) <= lineWidth with
        | true -> helper rest _out newState
        | false -> helper _in (_out@[state]) WHITESPACE
    helper words [] String.Empty

  /// Gets a string, and finds all the indexes of whitespaces in that string 
  let getWhitespaceCoordinates (line:string) (row:int) = 
    let rec helper (start:int) _out =
      let index = line.IndexOf(WHITESPACE, start)
      match index = -1 with
      | true -> _out
      | _ -> 
        let coordinate = (row, index)
        helper (index+1) (coordinate::_out)
    helper 0 []

  /// Imagine a list of strings as jaggered matrix, this finds all the whitespaces and rep
  let flattenListToCoordinates (words:string list) =
    let rec helper words index _out =
      match words with 
      | [] -> _out
      | h::t -> 
        let coords = (getWhitespaceCoordinates h index)@_out
        helper t (index+1) coords
    helper words 0 []

  
  let removeEntryFromList (row, column) coords = 
    List.filter (fun coordEntry -> coordEntry <> (row, column) ) coords

  // Searchs a list with (x,y) coordinate entries, return an adjacent entry to the input
  let tryFindAdjacentFromList (r, c) coords = 
    let rec helper candidates entry =
      match candidates, entry=None with
      | [], _ | _, false -> entry
      | (row, column)::t, _ ->  
        let entry = List.tryFind (fun coordEntry -> coordEntry = (row, column) ) coords
        helper t entry

    let row = r+1
    let candidates = (row, c-1)::(row, c)::(row, c+1)::[]
    helper candidates None

  let pathfinder coords start = 
    let rec helper _in _out start =
      match coords with
      | [] -> _out
      | v -> 
        tryFindAdjacentFromList start _in

    helper coords [] start

  let searchForRivers (lineWidth:int) (words:string list) =
    let rec helper coords maxLength =
      match coords with
      | [] -> maxLength
      | h::t -> max maxLength (List.length (pathfinder t h))
      
    let resizedStrList = reshape lineWidth words
    let coords = List.rev (flattenListToCoordinates resizedStrList)
    helper coords 0 


  
  let stringConcatList (words:string list) =
    let rec helper _in _out =
      match _in with
      | [] -> _out
      | entry::rest -> 
        helper rest (stringConcat _out entry)
    helper words String.Empty

  let processInput (input:string) =
    let words = stringSplit input ' ' Default
    let longestWordLength = findLongestWordLength words

    let isInputValid =
      true
      |> (&&) (isAllowedCharacters input)
      |> (&&) (List.length words >= MIN_NUMBER_OF_WORDS)
      |> (&&) (longestWordLength <= MAX_WORD_LENGTH)
      |> (&&) (not (emptyEntriesExist words))

    match isInputValid with
    | false -> None
    | true -> 
      // TODO -- Finalize the logic
      // let rivers = searchForRivers longestWordLength words
      
      // Defaulting with random values
      Some (1, 5)

//processInput "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country"
//processInput "When two or more rivers meet at a confluence other than the sea the resulting merged river takes the name of one of those rivers"
    