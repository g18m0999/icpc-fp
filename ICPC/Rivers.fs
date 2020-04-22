namespace Rivers
module Utils =

  open System

  type RiverInfo = {
    line_width:int
    river_length:int
  }

  type River = {
    start_row:int
    start_column:int
    length:int
  }

  type State = {
    text:string
    length:int
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

  let stringConcatWords (state:string) (entry:string) =
    let newState = (state+WHITESPACE+entry).Trim()
    { text=newState; length=(String.length newState) }

  let stringConcatList (words:string list) =
    let rec helper _in _out =
      match _in with
      | [] -> _out
      | entry::rest -> 
        helper rest (stringConcatWords _out.text entry)
    helper words { text=String.Empty; length=0 }

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
        let newState = stringConcatWords state entry
        match newState.length <= lineWidth with
        | true -> helper rest _out newState.text
        | false -> helper _in (_out@[state]) WHITESPACE
    helper words [] String.Empty

  let searchhelper (words:string list) =
    []
    //match (List.length words) <= (MIN_NUMBER_OF_WORDS+1) with 
    //| true -> Some ((stringConcatList words).length, 1)
    //| false -> searchhelper startLineWidth words

    //let rec inner (_in:string list) (index:int) _out =
    //  match _in with
    //  | [] -> _out
    //  | row::matrix ->
    //    let spaceIndex = row.IndexOf WHITESPACE
    //    match spaceIndex with
    //    | -1 ->  

  let searchForRivers (startLineWidth:int) (words:string list) =
    let rec rivers (start:int) (_out: 'a list) =
      let reshaped = reshape start words
      match (List.length reshaped) = 1 with
      | true -> _out@(searchhelper reshaped)
      | false ->
        let result = _out@(searchhelper reshaped)
        rivers (start+1) result
        
    rivers startLineWidth []

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
      let rivers = searchForRivers longestWordLength words
      
      // Defaulting with random values
      Some (1, 5)


//let words = stringSplit "hi  world" ' ' Default
//emptyEntriesExist words


//reshape 15 words

//processInput "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country"
//processInput "When two or more rivers meet at a confluence other than the sea the resulting merged river takes the name of one of those rivers"
    