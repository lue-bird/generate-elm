module Elm.Code exposing
    ( withImports
    , lowerNameParserUntil, upperNameParserUntil
    , locateOrigin
    )

{-| The goal is to switch from [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) to a custom syntax tree.

Why?

  - Adapt to the imports of the module it generates in (which could already have imports with a different alias etc.)

    → Store the module it was imported from

  - ~~`Node`~~
      - Make case matching & creating easier
      - Make code diffing easier (are 2 code snippets equal?)

  - ~~impossible cases~~ like
      - `Tupled [] or [ ... ] or [ ..., ..., ..., ... ]`
      - `Application [] or [ ... ]`
      - `RecordUpdateExpression ... []`
      - `LetExpression { declarations = [], ... }`
      - `LambdaExpression { args = [], ... }`
      - `CaseExpression { cases = [], ... }`
      - `Operator ...`
      - ... (many many more)

  - Easier code inspection and modification


# code generation

Going away from [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) also means creating a replacement for [the-sett's `elm-syntax-dsl`](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/).

Goal: safe, readable and intuitive code generation

@docs withImports


## validate

@docs lowerNameParserUntil, upperNameParserUntil


## [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) transform

@docs originLookup, locateOrigin

-}

import AssocSet
import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.CodeGen as CodeGen
import Elm.Docs exposing (Associativity(..))
import Elm.Pretty as CodeGen
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Import as Import
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..), value)
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Fillable exposing (Empty(..))
import Generalizable exposing (Generalizable, GeneralizableAndTransformable, generalWith, toGeneral)
import List.NonEmpty
import Misc exposing (applyIfJust, firstJust, mapAll3, maybeNonemptyToList, prettyWidth, printPretty, triple)
import Parser exposing ((|.), (|=), Parser)
import Possibly exposing (Possibly)
import Pretty exposing (pretty)
import ResultME exposing (ResultME)
import ResultME.Extra as ResultME
import Set exposing (Set)
import Stack exposing (StackFilled, topAndBelow)
import Unicode


{-| Create a [`Origin`](#Origin) qualification lookup function from the [`Imports`](#Imports) of a specific module.
-}
findOrigin : Imports -> (Origin -> List String)
findOrigin imports toFind =
    let
        ( originToFind, nameToFind ) =
            toFind

        effectiveImports : Imports
        effectiveImports =
            Dict.union imports implicitImports
    in
    case originToFind of
        DeclaredLocally ->
            []

        DeclaredIn moduleToFind ->
            case effectiveImports |> Dict.get moduleToFind of
                Just { alias, exposed } ->
                    let
                        moduleNameOrAlias : () -> List String
                        moduleNameOrAlias () =
                            case alias of
                                Just hasAlias ->
                                    [ hasAlias ]

                                Nothing ->
                                    moduleToFind |> moduleNameToSyntax
                    in
                    case exposed of
                        Just ExposingAll ->
                            []

                        Just (Exposing expose) ->
                            if
                                exposedNames expose
                                    |> Set.member nameToFind
                            then
                                []

                            else
                                moduleNameOrAlias ()

                        Nothing ->
                            moduleNameOrAlias ()

                Nothing ->
                    -- never allow this to happen!
                    -- automatically add necessary imports!
                    moduleToFind |> moduleNameToSyntax



--


type alias OriginLookup =
    RecordWithoutConstructorFunction
        { exposed : Dict String { moduleOrigin : String }
        , aliases : Dict String { moduleOrigin : String }
        }


originLookupByModules :
    Dict String (Set String)
    -> Imports
    -> OriginLookup
originLookupByModules exposedFromModules imports =
    { exposed =
        Dict.union imports implicitImports
            |> Dict.toList
            |> List.filterMap
                (\( moduleName, { exposed } ) ->
                    exposed |> Maybe.map (Tuple.pair moduleName)
                )
            |> List.map
                (\( moduleName, exposed ) ->
                    (case exposed of
                        ExposingAll ->
                            exposedFromModules
                                |> Dict.get moduleName
                                |> Maybe.withDefault Set.empty

                        Exposing expose ->
                            exposedNames expose
                    )
                        |> Set.toList
                        |> List.map
                            (\exposeName ->
                                ( exposeName, { moduleName = moduleName } )
                            )
                        |> Dict.fromList
                )
            |> List.foldl Dict.union Dict.empty
    , aliases =
        Dict.union imports implicitImports
            |> Dict.toList
            |> List.filterMap
                (\( moduleName, { alias } ) ->
                    alias
                        |> Maybe.map
                            (\alias_ ->
                                ( alias_, { moduleName = moduleName } )
                            )
                )
            |> Dict.fromList
    }


locateOrigin : OriginLookup -> List String -> String -> ( ModuleOrigin, String )
locateOrigin originLookup_ qualification name =
    ( case qualification of
        [] ->
            case originLookup_.exposed |> Dict.get name of
                Just moduleName ->
                    DeclaredIn moduleName

                Nothing ->
                    DeclaredLocally

        [ potentialAlias ] ->
            case originLookup_.aliases |> Dict.get potentialAlias of
                Just moduleName ->
                    DeclaredIn moduleName

                Nothing ->
                    -- todo: error?
                    DeclaredIn potentialAlias

        _ :: _ :: _ ->
            DeclaredIn (qualification |> moduleNameFromSyntax)
    , name
    )



--


fq : (a -> b -> c) -> (( d, b ) -> a) -> ( d, b ) -> c
fq code findOrigin_ ( importedFrom, name ) =
    let
        qualification =
            findOrigin_ ( importedFrom, name )
    in
    code qualification name



--


{-| Prefer different default imports for the module where the code is generated in. (You still have to `elm install` the desired _dependencies_)

    import Elm.Code as Code
    import Elm.Generator as Generator
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    getSetRecordWithFieldPrefix =
        RecordFieldHelper.monocle
            >> Generator.mapElm
                (\fieldName ->
                    Generator.replaceExpression
                        (let
                            { access, set } =
                                RecordFieldHelper.functionsForField fieldName
                         in
                         construct "Lens" [ access, set ]
                        )
                        >> Code.withImports
                            [ Code.import_
                                ( "Monocle", [ "Lens" ] )
                                |> Code.withExposing
                                    (addTypeAndAliasExposes [ "Lens" ])
                            ]
                )

-}
withImports :
    List Import
    -> Code code
    -> Code code
withImports imports code =
    { code
        | imports =
            imports |> joinAndSortImports
    }



--


concatJoinAndSortImports : List Imports -> Imports
concatJoinAndSortImports importsList =
    importsList
        |> List.map toImportList
        |> List.concat
        |> joinAndSortImports


toImportList : Imports -> List Import
toImportList imports =
    imports
        |> Dict.toList
        |> List.map
            (\( moduleName, { alias, exposed } ) ->
                { moduleName = moduleName, alias = alias, exposed = exposed }
            )


joinAndSortImports : List Import -> Imports
joinAndSortImports =
    List.map
        (\{ moduleName, alias, exposed } ->
            ( moduleName, { alias = alias, exposed = exposed } )
        )
        >> Dict.fromListDedupe
            (\a b ->
                { alias = firstJust [ a.alias, b.alias ]
                , exposed =
                    case ( a.exposed, b.exposed ) of
                        ( Nothing, bExposed ) ->
                            bExposed

                        ( aExposed, Nothing ) ->
                            aExposed

                        ( Just aExposed, Just bExposed ) ->
                            joinExposings aExposed bExposed
                                |> Just
                }
            )


joinExposings : Exposing -> Exposing -> Exposing
joinExposings aExposed bExposed =
    case ( aExposed, bExposed ) of
        ( ExposingAll, _ ) ->
            ExposingAll

        ( _, ExposingAll ) ->
            ExposingAll

        ( Exposing a, Exposing b ) ->
            let
                combinedOpenTypes =
                    Set.union a.openTypes b.openTypes
            in
            Exposing
                { values = Set.union a.values b.values
                , typesAndAliases =
                    Set.diff
                        (Set.union a.typesAndAliases b.typesAndAliases)
                        combinedOpenTypes
                , openTypes = combinedOpenTypes
                , infixOperators =
                    AssocSet.union a.infixOperators b.infixOperators
                }



--


printExpressionSyntax : Expression.Expression -> String
printExpressionSyntax expressionSyntax =
    expressionSyntax
        |> CodeGen.prettyExpression
        |> printPretty



--


{-| Parse valid uppercase names. [`lowerNameParserUntil`](#lowerNameParserUntil) parses lowercase names.

This is more tricky than you'd think:

    import Elm.Code exposing (upperNameParserUntil)

    "Ϡѽ"
        |> Parser.run (upperNameParserUntil Parser.end)
    --> Ok "Ϡѽ"

`Ϡѽ` for example is actually a valid uppercase name in elm!

    "TypeAliasName"
        |> Parser.run (upperNameParserUntil Parser.end)
    --> Ok "TypeAliasName"

    "Variant Name"
        |> Parser.run (upperNameParserUntil Parser.end)
    --> Err ...

    "_TypeAliasName"
        |> Parser.run (upperNameParserUntil Parser.end)
    --→ Err ...

    "3TypeName"
        |> Parser.run (upperNameParserUntil Parser.end)
    --→ Err ...

    "" |> Parser.run (upperNameParserUntil Parser.end)
    --→ Err ...

You can use this to conveniently build name parsers like

    import Elm.Generator exposing (lowerNameParserUntil)
    import Parser exposing ((|.))

    decoderNameParser : Parser String
    decoderNameParser =
        lowerNameParserUntil
            (Parser.symbol "Decoder" |. Parser.end)
            |> Parser.map Char.toUpper

In this specific use-case, you can also use [`Elm.RecordFieldHelper.Generate.fieldNameParserUntil`](Elm-Generator-RecordFieldHelper#fieldNameParserUntil).

-}
upperNameParserUntil : Parser a_ -> Parser String
upperNameParserUntil suffixParser =
    parserForName
        { casing =
            -- https://www.compart.com/en/unicode/category/Lu
            Unicode.isUpper
        , until = suffixParser
        }


{-| Parses valid lowercase names. [`upperNameParserUntil`](#upperNameParserUntil) parses uppercase names.

This is more tricky than you'd think:

    import Elm.Code exposing (lowerNameParserUntil)

    "ѽϠ"
        |> Parser.run (lowerNameParserUntil Parser.end)
    --> Ok "ѽϠ"

    "where"
        |> Parser.run (lowerNameParserUntil Parser.end)
    --> Err ...

`ѽϠ` for example is actually a valid lowercase name in elm while `where` isn't (see [reserved](#reserved))!

    "valueName"
        |> Parser.run (lowerNameParserUntil Parser.end)
    --> Ok "valueName"

    "argument name"
        |> Parser.run (lowerNameParserUntil Parser.end)
    --> Err ...

    "_portName"
        |> Parser.run (lowerNameParserUntil Parser.end)
    --→ Err ...

    "3typeParameterName"
        |> Parser.run (lowerNameParserUntil Parser.end)
    --→ Err ...

    "" |> Parser.run (lowerNameParserUntil Parser.end)
    --→ Err ...

You can use this to conveniently build name parsers like

    import Elm.Generator exposing (lowerNameParserUntil)
    import Parser exposing ((|.))
    import String.Extra

    setterNameParser : Parser String
    setterNameParser =
        Parser.succeed String.Extra.toSentenceCase
            |. Parser.symbol "set"
            |= upperNameParserUntil Parser.end

In this specific case, you can also use [`Elm.RecordFieldHelper.Generator.fieldNameParserUntil`](Elm-RecordFieldHelper-Generator#fieldNameParserUntil).

-}
lowerNameParserUntil : Parser a_ -> Parser String
lowerNameParserUntil suffixParser =
    Parser.oneOf
        ([ reserved
            |> Set.toList
            |> List.map
                (\keyword ->
                    Parser.keyword keyword
                        |> Parser.andThen
                            (\() -> Parser.problem "reserved word")
                )
         , [ parserForName
                { casing =
                    -- https://www.compart.com/en/unicode/category/Ll
                    Unicode.isLower
                , until = suffixParser
                }
           ]
         ]
            |> List.concat
        )


{-| [reserved words in elm](https://github.com/elm/compiler/blob/770071accf791e8171440709effe71e78a9ab37c/compiler/src/Parse/Variable.hs#L70)
-}
reserved : Set String
reserved =
    [ "if"
    , "then"
    , "else"
    , "case"
    , "of"
    , "let"
    , "in"
    , "type"
    , "module"
    , "where"
    , "import"
    , "exposing"
    , "as"
    , "port"
    ]
        |> Set.fromList


parserForName :
    { casing : Char -> Bool, until : Parser a_ }
    -> Parser String
parserForName { casing, until } =
    Parser.succeed (\a bc -> a ++ bc)
        |= (Parser.chompIf casing
                |> Parser.getChompedString
           )
        |= Parser.loop ""
            (\str ->
                Parser.oneOf
                    [ until |> Parser.map (\_ -> Parser.Done str)
                    , Parser.chompIf
                        (\c -> Unicode.isAlphaNum c || c == '_')
                        |> Parser.getChompedString
                        |> Parser.map (\s -> Parser.Loop (str ++ s))
                    ]
            )
