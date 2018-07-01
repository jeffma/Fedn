namespace Fedn

module EdnParser =
    open System.IO
    open System.Numerics
    open FParsec
    open Fedn.EdnType

    let internal isWhiteSpace c = 
        match c with 
        ' ' |  '\r' | '\t' | '\n' | ',' -> true
        | _ -> false

    let internal whiteSpace : Parser<char, unit> = 
        satisfy isWhiteSpace
    let internal skipWhiteSpace = many whiteSpace |>> ignore 
                        
    let internal delimiter : Parser<char, unit> =
        anyOf "()[]{} ,"


    let internal parseNil : Parser<EdnValue, unit> = 
        (stringReturn "nil" EdnNil)
        .>>(lookAhead eof <|> lookAhead (delimiter |>> ignore))
        .>> skipWhiteSpace
        
    let internal parseBool : Parser<EdnValue, unit> =   
        (choice [ pstring "true" >>% EdnBoolean(true) ; 
                       pstring "false" >>% EdnBoolean(false)])
        .>>(lookAhead eof <|> lookAhead (delimiter |>> ignore))               
        .>> skipWhiteSpace               

    let internal parseString : Parser<EdnValue, unit> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         |  c  -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar)) 
            |>> fun s -> EdnString(s)
        .>>(lookAhead eof <|> lookAhead (delimiter |>> ignore))    
        .>> skipWhiteSpace

    let internal parseCharacter : Parser<EdnValue, unit> = 
            pchar '\\' >>.
                (pstring "newline" <|> pstring "return" <|> pstring "space" 
                      <|> pstring "tab" <|> (manyChars (noneOf " \r\t\n")) 
                |>> fun s ->
                    match s with
                    | "newline" -> EdnCharacter('\n')
                    | "return" -> EdnCharacter('\r')
                    | "space" -> EdnCharacter(' ')
                    | "tab" -> EdnCharacter('\t')
                    | _ -> EdnCharacter(s.Chars 0))
                .>>(lookAhead eof <|> lookAhead (delimiter |>> ignore))        
                .>> skipWhiteSpace

    let internal numberFormat = NumberLiteralOptions.AllowMinusSign
                                          ||| NumberLiteralOptions.AllowPlusSign
                                          ||| NumberLiteralOptions.AllowFraction
                                          ||| NumberLiteralOptions.AllowExponent
                                          ||| NumberLiteralOptions.AllowFractionWOIntegerPart

    let internal parseNumber : Parser<EdnValue, unit> = 
        attempt(
            numberLiteral numberFormat "number"
            |>> fun nl ->
                    if nl.IsInteger then EdnInteger(BigInteger.Parse nl.String)
                    else EdnFloat(float nl.String)
            .>>(lookAhead eof <|> lookAhead (delimiter |>> ignore))       
            .>> skipWhiteSpace)
            

    let internal parseComment : Parser<EdnValue, unit> = 
        pchar ';' >>. 
        restOfLine false |>> (fun s -> EdnComment(s))

    let internal isAlphaChar c = 
       match c with
       | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'  
       | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' 
       | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'  
       | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' -> true
       | _ -> false

    let internal isValidInnerSymbolChar c = 
       match c with
       | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'  
       | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' 
       | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'  
       | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' 
       | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '>'| '<'
       | '#' | ':' | '.' | '*' | '!' | '?' | '$' | '%' | '&' | '=' | '+' | '_' | '-' -> true
       | _ -> false

    let internal isValidFirstSymbolChar c = 
       match c with
       | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'  
       | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' 
       | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'  
       | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' 
       | '>'| '<'
       | '.' | '*' | '!' | '?' | '$' | '%' | '&' | '=' | '+' | '_' | '-' -> true
       | _ -> false

    let internal isNonNumericSymbolChar c = 
       match c with
       | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'  
       | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' 
       | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'  
       | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' 
       | '>'| '<'
       | '#' | ':' | '.' | '*' | '!' | '?' | '$' | '%' | '&' | '=' | '+' | '_' | '-' -> true
       | _ -> false

    let internal parseSymbolPart : Parser<string, unit> = 
        satisfy isValidFirstSymbolChar <?> "Symbol has invalid first character" >>=
            fun firstChar ->
                match firstChar with
                | '+' | '-' | '.'  -> satisfy isNonNumericSymbolChar .>>. manySatisfy isValidInnerSymbolChar 
                                   |>> fun (nonNumeric, rest) -> System.String.Format("{0}{1}{2}", firstChar, nonNumeric, rest)
                | _ -> manySatisfy isValidInnerSymbolChar |>>  fun s -> System.String.Format("{0}{1}", firstChar, s)

    let internal parseQualifiedSymbol =
        attempt (parseSymbolPart .>> (pchar '/')) .>>. parseSymbolPart |>> 
            fun (prefix, name) -> EdnSymbol(QualifiedSymbol(prefix, name))

    let internal parseSymbol =
        parseQualifiedSymbol <|> (parseSymbolPart 
        |>> fun s-> EdnSymbol(QualifiedSymbol(null, s)))
        .>>(lookAhead eof <|> lookAhead (delimiter |>> ignore))
        .>> skipWhiteSpace   

    let internal parseKeyWord = 
        pchar ':' >>.
        parseSymbol |>> function (EdnSymbol qualifiedSymbol) -> EdnKeyword(qualifiedSymbol) 
                                 | _ -> raise (System.Exception("Invalid keyword."))                       
        .>> skipWhiteSpace

    let internal parseTag =
        attempt (pchar '#' >>. lookAhead (satisfy isAlphaChar)) >>.
        parseSymbol |>> function (EdnSymbol qs) -> qs | _ -> raise (System.Exception("Invalid tag."))
        .>> skipWhiteSpace

    let internal ednValue,internal ednValueRef = createParserForwardedToRef<EdnValue,unit>()

    let internal parseDiscard =
        pstring "#_" >>.
        ednValue |>> fun (value) -> EdnDiscard(value)
        .>> skipWhiteSpace


    let internal parseTaggedValue : Parser<EdnValue, unit> = 
        parseTag .>>.
        ednValue |>> fun (symbol, value) -> EdnTaggedValue(symbol, value)
        .>> skipWhiteSpace


    let internal parseList = 
        let left = pchar '(' .>> skipWhiteSpace
        let right = pchar ')' .>> skipWhiteSpace
        between left right (many ednValue)
        |>> EdnList

    let internal parseSet = 
        let left = pstring "#{" .>> skipWhiteSpace
        let right = pchar '}' .>> skipWhiteSpace
        between left right (many ednValue)
        |>> EdnSet

    let internal parseVector = 
        let left = pchar '[' .>> skipWhiteSpace
        let right = pchar ']' .>> skipWhiteSpace
        between left right (many ednValue)
        |>> (Array.ofList >> EdnVector)

    let internal parseMap = 
        let parseEvenList l = 
            let filteredList = List.filter isNotCommentOrDiscard l
            if filteredList.Length % 2 = 0 then
                preturn l
            else
                fail "Map must have even number of elements"
        let left = pchar '{' .>> skipWhiteSpace
        let right = pchar '}' .>> skipWhiteSpace
        between left right (many ednValue >>= parseEvenList)
        |>> EdnMap

    ednValueRef := choice 
          [
            parseNil 
            parseBool
            parseString
            parseCharacter
            parseNumber            
            parseKeyWord
            parseDiscard
            parseTaggedValue
            parseComment
            parseSymbol
            parseMap
            parseSet
            parseVector
            parseList
          ]
          

    let internal getValueFromResult result =
        match result with
          | Success (r,_,_) -> r
          | Failure (r,_,_) -> raise (System.Exception (r))

    type public EDNParserFuncs =
       static member FromString str = 
           run (skipWhiteSpace >>. many1 ednValue) str |> getValueFromResult 
    
       static member FromStream stream = 
            runParserOnStream (skipWhiteSpace >>. many1 ednValue) () "ednStream" stream System.Text.Encoding.UTF8 |> getValueFromResult

       static member FromFile fileName = 
            runParserOnFile (skipWhiteSpace >>. many1 ednValue) () fileName System.Text.Encoding.UTF8 |> getValueFromResult 

       static member FromDirectory dir  = 
           let searchPattern = @"*.edn"
           let testFiles = Directory.GetFiles(dir, searchPattern, SearchOption.AllDirectories)
           let results = [for f in testFiles do yield! EDNParserFuncs.FromFile f]
           results


