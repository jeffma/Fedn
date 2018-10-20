module Tests

open System
open Xunit
open FParsec
open Fedn.EdnType
open Fedn.EdnParser
open System.Numerics
open System.IO
open System.Reflection


[<Fact>]
let ``EdnNil From String case1`` () =
    let result = EDNParserFuncs.FromString "  nil, nil "
    let firstItem = List.item 1 result
    match firstItem with
    | EdnNil -> Assert.True(true,"first item is edn nil")
    | _ -> Assert.False(true,"first item is not nil")


[<Fact>]
let ``EdnBool true From String`` () =
    let result = EDNParserFuncs.FromString " nil, true"
    let secondItem = List.item 1 result
    match secondItem with
    | EdnBoolean(true) -> Assert.True(true,"second item is edn true")
    | _ -> Assert.False(true,"second item is not true")
      
[<Fact>]
let ``EdnBool false From String`` () =
    let result = EDNParserFuncs.FromString "  nil, true, false"
    let thirdItem = List.item 2 result
    match thirdItem with
    | EdnBoolean(false) -> Assert.True(true,"third item is edn false")
    | _ -> Assert.False(true,"third item is not false")

[<Fact>]
let ``EdnString From String`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" "
    let fourthItem = List.item 3 result
    match fourthItem with
    | EdnString("测试") -> Assert.True(true,"fourth item is edn string 测试")
    | _ -> Assert.False(true,"fourth item is not edn string 测试")

[<Fact>]
let ``EdnCharacter From String case1`` () =
    let result = EDNParserFuncs.FromString " nil, true false #_abc \\newline "
    let fifthItem = List.item 4 result
    match fifthItem with
    | EdnCharacter('\n') -> Assert.True(true,"fifth item is edn character c")
    | _ -> Assert.False(true,"fifth item is not edn character c")  

[<Fact>]
let ``EdnCharacter From String case2`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\abc "
    let fifthItem = List.item 4 result
    match fifthItem with
    | EdnCharacter('a') -> Assert.True(true,"fifth item is edn character c")
    | _ -> Assert.False(true,"fifth item is not edn character c")         

[<Fact>]
let ``EdnInteger From String case1`` () =
    let result = EDNParserFuncs.FromString " 123 "
    let firstItem = List.item 0 result
    match firstItem with
    | EdnInteger(n) when n=BigInteger(123) -> Assert.True(true,"first item is edn integer 123")
    | _ -> Assert.False(true,"fifth item is not edn integer")   

[<Fact>]
let ``EdnInteger From String case2`` () =
    let result = EDNParserFuncs.FromString " +123 "
    let firstItem = List.item 0 result
    match firstItem with
    | EdnInteger(n) when n=BigInteger(123) -> Assert.True(true,"first item is edn integer +123")
    | _ -> Assert.False(true,"fifth item is not edn integer") 

[<Fact>]
let ``EdnInteger From String case3`` () =
    let result = EDNParserFuncs.FromString " -123 "
    let firstItem = List.item 0 result
    match firstItem with
    | EdnInteger(n) when n=BigInteger(-123) -> Assert.True(true,"first item is edn integer -123")
    | _ -> Assert.False(true,"fifth item is not edn integer")     

[<Fact>]
let ``EdnFloat From String case4`` () =
    let result = EDNParserFuncs.FromString " 0.123 "
    let firstItem = List.item 0 result
    match firstItem with
    | EdnFloat(n) when n=Decimal(0.123) -> Assert.True(true,"first item is edn float 0.123")
    | _ -> Assert.False(true,"fifth item is not edn float")   

[<Fact>]
let ``EdnFloat From String case5`` () =
    let result = EDNParserFuncs.FromString " .123 "
    let firstItem = List.item 0 result
    match firstItem with
    | EdnFloat(n) when n=Decimal(0.123) -> Assert.True(true,"first item is edn float .123")
    | _ -> Assert.False(true,"fifth item is not edn float")  

[<Fact>]
let ``EdnFloat From String case6`` () =
    let result = EDNParserFuncs.FromString "-123.456e+2"
    let firstItem = List.item 0 result
    match firstItem with
    | EdnFloat(n) when n=Decimal.Parse("-123.456e+2", System.Globalization.NumberStyles.Float ||| System.Globalization.NumberStyles.AllowTrailingSign) -> Assert.True(true,"first item is edn float 1.87E+02")
    | _ -> Assert.False(true,"fifth item is not edn float")            

[<Fact>]
let ``EdnSymbol From String case1`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\newline .abc  "
    let sixthItem = List.item 5 result
    match sixthItem with
    | EdnSymbol(s) when s.name=".abc" -> Assert.True(true,"fifth item is edn character c")
    | _ -> Assert.False(true,"sixth item is not edn symbol") 

[<Fact>]
let ``EdnSymbol From String case2`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\newline -123  "
    let sixthItem = List.item 5 result
    match sixthItem with
    | EdnSymbol(s) when s.name="-123" -> Assert.False(true,"sixth item is error parsed to symbol")
    | EdnInteger(n) when n = BigInteger(-123) -> Assert.True(true,"sixth item is edn number 123")
    | _ -> Assert.False(true,"sixth item is error parsed") 

[<Fact>]
let ``EdnKeyword From String case1`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\newline .abc :abc"
    let seventhItem = List.item 6 result
    match seventhItem with
    | EdnKeyword(s) when s.name="abc" -> Assert.True(true,"seventh item is edn keyword :abc")
    | _ -> Assert.False(true,"sixth item is not edn keyword")   

[<Fact>]
let ``EdnKeyword From String case2`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\newline .abc :abc/def"
    let seventhItem = List.item 6 result
    match seventhItem with
    | EdnKeyword(s) when s.name="def" && s.prefix="abc" -> Assert.True(true,"seventh item is edn keyword :abc/def")
    | _ -> Assert.False(true,"sixth item is not edn keyword")     

[<Fact>]
let ``EdnTaggedValue From String case1`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\newline .abc :abc/def #abc \"value\""
    let eighthItem = List.item 7 result
    match eighthItem with
    | EdnTaggedValue(sym,value) when sym.name="abc" && value=EdnString("value") -> Assert.True(true,"seventh item is edn keyword :abc")
    | _ -> Assert.False(true,"sixth item is not edn keyword")  

[<Fact>]
let ``EdnDiscard From String case1`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\newline .abc :abc/def #abc \"value\" #_def"
    let ninthItem = List.item 8 result
    match ninthItem with
    | EdnDiscard(edn) when edn=EdnSymbol(QualifiedSymbol(null,"def")) -> Assert.True(true,"seventh item is edn keyword :abc")
    | _ -> Assert.False(true,"sixth item is not edn keyword")  

[<Fact>]
let ``EdnTaggedValue From String case2`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\newline .abc :abc/def #_#abc \"value\""
    let eighthItem = List.item 7 result
    match eighthItem with
    | EdnDiscard(EdnTaggedValue(sym,value)) when sym.name="abc" && value=EdnString("value") -> Assert.True(true,"seventh item is edn keyword :abc")
    | _ -> Assert.False(true,"sixth item is not edn keyword")  

[<Fact>]
let ``EdnComment From String case1`` () =
    let result = EDNParserFuncs.FromString " nil, true false \"测试\" \\newline .abc :abc/def #abc \"value\" ;#_def"
    let ninthItem = List.item 8 result
    match ninthItem with
    | EdnComment(s) when s="#_def" -> Assert.True(true,"seventh item is edn comment ")
    | _ -> Assert.False(true,"ninth item is not edn comment")  

[<Fact>]
let ``EdnMap From String case1`` () =
    let result = EDNParserFuncs.FromString " {:abc \"abc\" :def \"def\"}"
    let firstItem = List.item 0 result
    match firstItem with
    | EdnMap(edns) when edns 
        |> List.chunkBySize 2
        |> List.map(fun x -> (x.[0],x.[1]))
        |> Map.ofSeq
        |> Map.find(EdnKeyword(QualifiedSymbol(null,"abc")))
        = EdnString("abc") -> Assert.True(true,"first item is edn map")
    | _ -> Assert.False(true,"first item is not edn map")  

[<Fact>]
let ``EdnVector From String case1`` () =
    let result = EDNParserFuncs.FromString " [ 1 \"abc\" 2 \"def\"]"
    let firstItem = List.item 0 result
    match firstItem with
    | EdnVector(edns) when edns.[0]  
        = EdnInteger(BigInteger(1)) -> Assert.True(true,"first item is edn vector")
    | _ -> Assert.False(true,"first item is not edn vector")  

[<Fact>]
let ``EdnList From String case1`` () =
    let result = EDNParserFuncs.FromString " ( 1 \"abc\" ecc :def)"
    let firstItem = List.item 0 result
    match firstItem with
    | EdnList(edns) when edns.[0]  
        = EdnInteger(BigInteger(1)) -> Assert.True(true,"first item is edn list")
    | _ -> Assert.False(true,"first item is not edn vector")  

[<Fact>]
let ``EdnList From String case2`` () =
    let result = EDNParserFuncs.FromString " ( 1 \"abc\" ecc :def)"
    let firstItem = List.item 0 result
    match firstItem with
    | EdnList(edns) when edns.[1]  
        = EdnString("abc") -> Assert.True(true,"first item is edn list")
    | _ -> Assert.False(true,"first item is not edn list")  

[<Fact>]
let ``EdnList From String case3`` () =
    let result = EDNParserFuncs.FromString " ( 1 \"abc\" ecc :def)"
    let firstItem = List.item 0 result
    match firstItem with
    | EdnList(edns) when edns.[2]  
        = EdnSymbol(QualifiedSymbol(null,"ecc")) -> Assert.True(true,"first item is edn list")
    | _ -> Assert.False(true,"first item is not edn list")  

[<Fact>]
let ``EdnSet From String case1`` () =
    let result = EDNParserFuncs.FromString " #{:abc \"abc\" :def \"def\"}"
    let firstItem = List.item 0 result
    match firstItem with
    | EdnSet(edns) when edns 
        |> Set.ofList
        |> Set.contains (EdnString("abc"))
        -> Assert.True(true,"first item is edn map")
    | _ -> Assert.False(true,"first item is not edn map")  

[<Fact>]

let ``Read Edn from file case1`` () =
    let assembly = Assembly.GetExecutingAssembly()
    let inputStream = assembly.GetManifestResourceStream("FednTest.Resources.1.edn");
    let edns = EDNParserFuncs.FromStream(inputStream)
    Assert.True((edns.[0]=EdnNil),"first item is nil")
    Assert.True((edns.[1]=EdnString("123")),"2nd item is edn string 123")