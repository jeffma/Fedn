namespace Fedn

module EdnType =
    open System.Numerics
    type QualifiedSymbol =
        struct
            val prefix: string
            val name: string
            new (prefix, name) = {prefix = prefix; name = name}
            override this.ToString() = "QualifiedSymbol Prefix: " + this.prefix + " Name: " + this.name
        end
    type EdnValue = EdnNil
                    | EdnBoolean of bool
                    | EdnString of string
                    | EdnCharacter of char
                    | EdnSymbol of QualifiedSymbol
                    | EdnKeyword of QualifiedSymbol
                    | EdnInteger of BigInteger
                    | EdnFloat of decimal
                    | EdnComment of string
                    | EdnDiscard of EdnValue
                    | EdnTaggedValue of QualifiedSymbol * EdnValue
                    | EdnList of EdnValue list
                    | EdnVector of EdnValue array
                    | EdnMap of List<EdnValue>
                    | EdnSet of List<EdnValue>

    let isNotCommentOrDiscard (v : EdnValue) =
        match v with 
         | EdnComment _ | EdnDiscard _ -> false
         | _ -> true