module Elm.Coder.Generator exposing
    ( Generator, Structure, Kind
    , jsonEncoder, jsonPipelineDecoder
    )

{-| Writing decoders and encoders...

  - ... is tedious
  - ... makes it harder to change the way you model your data (bad)
  - ... small mistakes sneak in easily ("compiles, but doesn't work")
  - ... can lead to out of sync decoders/encoders

`Elm.Generator.Coder` contains [`Generator`](#Generator)s for encoders, decoders and codecs that will automatically be synchronized with a `type`/`type alias` (todo!).

@docs Generator, Structure, Kind

@docs jsonEncoder, jsonPipelineDecoder


## limits

When the definition of the encoded/decoded type/type alias changes, the generated functions **won't change**. (todo)

todo: <https://github.com/miniBill/elm-codec-generator>

-}

import Dict exposing (Dict)
import Elm.Code as Code exposing (Code, Expression, ExpressionAny, FunctionLiteralExpression(..), GeneralReference, ModuleOrigin(..), ModuleScopeDeclarationAnyAny(..), Operator(..), Origin, PatternAny, Suppliable, Top, TypeCode, TypeGeneratedAny(..), access, apR, append, arrayType, boolType, call, case_, composeR, dictType, floatType, from, inferred, intType, lambda, listType, local, maybeType, namedType, of_, op, parenthesized, record, recordExtendedBy, recordType, setType, string, stringPattern, stringType, toType, tuple2, tuple2Pattern, tuple2Type, tuple3Pattern, tuple3Type, typeFromSyntax, typeToSyntax, unionPattern, unit, unitPattern, unitType, use, val, var)
import Elm.Code.Generator as Generator exposing (AllowsUnqualified, DeclarationGenerator)
import Elm.CodeGen as CodeGen exposing (binOpChain, caseExpr, composer, piper)
import Elm.Coder.Generator.Internal exposing (decoderNameParser, encoderNameParser)
import Elm.Pretty exposing (prettyTypeAnnotation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Json.Decode
import List.NonEmpty
import Misc exposing (capitalize, maybeNonemptyToList, prettyWidth)
import Pretty exposing (pretty)
import ResultME exposing (ResultME)
import ResultME.Extra as ResultME
import Stack exposing (StackFilled)
import String.Extra as String exposing (decapitalize)
import Typed


{-| Information about the declaration for which we can generate encoders, decoders and codecs:
See [`TypeKind`](#TypeKind) for the information specific to `type` or `type alias`.
-}
type alias Structure =
    { name : String
    , parameters : List String
    , kind : Kind
    }


{-| A declaration kind for which we can generate encoders, decoders and codecs:
Either `type` or `type alias`.
-}
type Kind
    = CodableTypeDeclaration { variants : StackFilled ( String, List TypeAny ) }
    | CodableTypeAliasDeclaration { aliasedType : TypeAny }


{-| A [`DeclarationGenerator`](Elm-Generator#DeclarationGenerator)s that generates encoder, decoder and codec declarations.
-}
type alias Generator requireQualified =
    DeclarationGenerator requireQualified Structure FunctionLiteralExpression


{-| [`Generator`](#Generator) for a [`elm/json`](https://package.elm-lang.org/packages/elm/json/latest/) encode function.


## encoder example

    type alias Point =
        { x : Float, y : Float }

    a =
        pointEncoder

will be "fixed" as

    import Json.Encode

    type alias Point =
        { x : Float, y : Float }

    pointEncoder : Point -> Json.Encode.Value
    pointEncoder point =
        Json.Encode.object
            [ ( "x", Json.Encode.float point.x )
            , ( "y", Json.Encode.float point.y )
            ]

    a =
        pointEncoder

using the configuration

    import Review.Generate
    import Elm.Review.Coder as Coder

    Review.Generate.inSameModule Coder.jsonEncoder
        |> Review.Generate.belowDeclarations
            (\typeOrAlias -> [ typeOrAlias.name ])
        |> Review.Generate.rule

-}
jsonEncoder : Generator AllowsUnqualified
jsonEncoder =
    Generator.forDeclaration "elm/json encoder"
        (Generator.nameChecker encoderNameParser
            |> Generator.checkNameInContext checkNameInContext
        )
        (\{ declarationName } structure ->
            let
                encoder =
                    structure |> jsonEncoderCode

                arguments =
                    Debug.todo ""

                argumentPatterns =
                    arguments
                        |> Dict.keys
                        |> List.map Code.var
                        |> Code.combine

                functionDeclaration =
                    case encoder |> root |> .code |> .general of
                        Code.SpecificExpression (Code.FunctionSpecificExpression (FunctionLiteralExpression args returnValue)) ->
                            Code.functionOrValueModuleScopeDeclaration
                                (argumentPatterns
                                    |> Code.next (args |> Stack.fromTopAndBelow |> Code.combineIs)
                                )
                                returnValue

                        _ ->
                            Code.functionOrValueModuleScopeDeclaration argumentPatterns
                                encoder
            in
            functionDeclaration
                |> Code.withType (\f -> f)
                    ((Stack.fromList arguments
                        |> Stack.appendNotEmpty
                            (Stack.only
                                (namedType (local structure.name)
                                    (structure.parameters
                                        |> List.map inferred
                                    )
                                )
                            )
                        |> Stack.toTopAndBelow
                     )
                        |> toType (jsonValueType |> use)
                    )
                |> Code.withImports
                    [ Code.import_ "Json.Encode" ]
        )


{-| [`Generator`](#Generator) for [`elm/json`](https://package.elm-lang.org/packages/elm/json/latest/) `Decoder`s using
[NoRedInk's elm-json-decode-pipeline](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest).

**Treat this one with care!**
The generated decoders might not decode what you expect (for `Dict` for example).
Evan generally recommends against implicitly defined serialization
because of limitations to that approach that he describes in his
[vision for data interchange](https://gist.github.com/evancz/1c5f2cf34939336ecb79b97bb89d9da6#gistcomment-2606737).
Read it.
Then use this generator tool to get some **help** generating `Decoder`s.


## decoder example

    type alias Point =
        { x : Float, y : Float }

    a =
        pointDecoder

will be "fixed" as

    import Json.Decode as JsonDecode
    import Json.Decode.Pipeline as JsonDecode

    type alias Point =
        { x : Float, y : Float }

    pointDecoder : JsonDecode.Decoder Point
    pointDecoder point =
        JsonDecode.succeed (\x y -> { x = x, y = y })
            |> JsonDecode.required "x" JsonDecode.float
            |> JsonDecode.required "y" JsonDecode.float

    a =
        pointDecoder

using the configuration

    import Review.Generate
    import Elm.Review.Coder as Coder

    Review.Generate.inSameModule Coder.jsonEncoder
        |> Review.Generate.belowDeclarations
            (\typeOrAlias -> [ typeOrAlias.name ])
        |> Review.Generate.rule

-}
jsonPipelineDecoder : Generator AllowsUnqualified
jsonPipelineDecoder =
    Generator.forDeclaration "elm/json pipeline decoder"
        (Generator.nameChecker decoderNameParser
            |> Generator.checkNameInContext checkNameInContext
        )
        (\{ declarationName } structure ->
            let
                decoder =
                    structure |> jsonDecoderCode

                argumentPatterns =
                    decoder.arguments
                        |> Dict.keys
                        |> List.map Code.var
                        |> Code.combine

                functionDeclaration =
                    case decoder.expression of
                        Code.SpecificExpression (Code.FunctionSpecificExpression (FunctionLiteralExpression args returnValue)) ->
                            Code.functionOrValueModuleScopeDeclaration declarationName
                                (argumentPatterns ++ (args |> List.NonEmpty.toList))
                                returnValue

                        _ ->
                            Code.functionOrValueModuleScopeDeclaration declarationName
                                argumentPatterns
                                decoder.expression
            in
            functionDeclaration
                |> Code.withType (\f -> f)
                    ((decoder.arguments |> Dict.values)
                        |> toType
                            (jsonDecoder
                                (namedType (local structure.name)
                                    (List.map inferred structure.parameters)
                                )
                                |> use
                            )
                    )
                |> Code.withImports
                    [ Code.import_ "Json.Decode"
                        |> Code.withAs "JsonDecode"
                    , Code.import_ "Json.Decode.Pipeline"
                        |> Code.withAs "JsonDecode"
                    ]
        )


checkNameInContext :
    { declarations : Dict String DeclarationSpecificAny }
    -> String
    -> ResultME String Structure
checkNameInContext { declarations } codableName =
    let
        locate =
            Debug.todo "" <| declarations
    in
    case declarations |> Dict.get codableName of
        Just codableDecl ->
            case codableDecl of
                TypeAliasDeclaration { parameters, aliasedType } ->
                    { name = codableName
                    , parameters = parameters
                    , kind =
                        CodableTypeAliasDeclaration
                            { aliasedType = aliasedType.code }
                    }
                        |> Ok

                UnionTypeDeclaration { parameters, variants } ->
                    { name = codableName
                    , parameters = parameters
                    , kind =
                        CodableTypeDeclaration
                            { variants =
                                variants
                                    |> Stack.map
                                        (\( name, arguments ) ->
                                            ( name, arguments |> List.map .code )
                                        )
                            }
                    }
                        |> Ok

                _ ->
                    ResultME.error
                        ("missing `type`/`type alias` `" ++ codableName ++ "`")

        Nothing ->
            ResultME.error
                ("missing `type`/`type alias` `" ++ codableName ++ "`")


encoderOf : String -> String
encoderOf structureName =
    structureName ++ "Encoder"


jsonEncoderCode :
    { declaration_ | kind : Kind, name : String }
    ->
        Top
            (Code
                { kind : Expression FunctionLiteralExpression
                , general : ExpressionAny
                }
            )
jsonEncoderCode codable =
    case codable.kind of
        CodableTypeDeclaration typeInfo ->
            typeInfo
                |> typeEncoder
                    { typeName = codable.name
                    , suffix = ""
                    }
                |> use

        CodableTypeAliasDeclaration typeAliasInfo ->
            typeAliasInfo.aliasedType
                |> typeAnnotationEncoder { suffix = "" }
                |> use


jsonEncode : String -> Origin
jsonEncode member =
    from "Json.Encode" member


jsonValueType :
    TypeCode
        { origin : Origin, arguments : List (Code never_) }
jsonValueType =
    namedType (jsonEncode "Value") []


encoderArguments codableType =
    let
        newArg :
            String
            -> TypeCode specific_
            -> Dict String TypeAny
        newArg name itsType =
            Dict.singleton name
                (itsType |> root |> .code |> .general)
    in
    case codableType of
        VariableType typeArgument ->
            let
                typeArgumentEncoder =
                    encoderOf typeArgument
            in
            newArg typeArgumentEncoder
                (( inferred typeArgument, [] )
                    |> toType jsonValueType
                )

        ExtendedRecordType extended fields ->
            let
                givenFieldEncoders =
                    fields
                        |> List.NonEmpty.toList
                        |> List.map (Tuple.second >> encoderArguments)
                        |> List.foldl Dict.union Dict.empty

                extraFieldEncoders =
                    extended ++ "FieldEncoders"
            in
            Dict.union
                (newArg extraFieldEncoders
                    (( inferred extended, [] )
                        |> toType
                            (listType
                                (tuple2Type
                                    ( stringType
                                    , jsonValueType
                                    )
                                )
                            )
                    )
                )
                givenFieldEncoders


typeAnnotationEncoder :
    { suffix : String }
    -> TypeAny
    ->
        Top
            (Code
                { kind : Expression FunctionLiteralExpression
                , general : ExpressionAny
                }
            )
typeAnnotationEncoder { suffix } typeAnnotation =
    let
        step additionalSuffix =
            typeAnnotationEncoder
                { suffix = suffix ++ additionalSuffix }

        argument : String -> String
        argument simpleName =
            simpleName ++ suffix

        patternVar simpleName =
            var (argument simpleName)

        -- todo: remove?
        argumentVal simpleName =
            val (local (argument simpleName))

        fieldEncoders record fields =
            let
                expressions =
                    fields
                        |> List.map
                            (\( fieldName, fieldType ) ->
                                let
                                    fieldTypeEncoder =
                                        fieldType ("In" ++ capitalize fieldName)
                                in
                                tuple2
                                    ( string fieldName
                                    , fieldTypeEncoder
                                        |> call
                                            ( record |> access.field fieldName
                                            , []
                                            )
                                    )
                            )
            in
            Code.list expressions

        tuplePartEncoder i fieldType value =
            tuple2
                ( string (a i)
                , fieldType
                    |> step ("In" ++ String.fromInt i)
                    |> call ( value, [] )
                )

        encoder1 structureEncoderName typeArgument =
            val (jsonEncode structureEncoderName)
                |> call ( typeArgument |> step "", [] )
                |> use

        encoder0 structureEncoderName =
            val (jsonEncode structureEncoderName)
                |> use
    in
    case typeAnnotation of
        UnitType () ->
            lambda (Code.only (unitPattern ()))
                (val (jsonEncode "null"))
                |> use

        NamedType ( DeclaredIn "Basics", "Bool" ) [] ->
            encoder0 "bool"

        NamedType ( DeclaredIn "Basics", "Int" ) [] ->
            encoder0 "int"

        NamedType ( DeclaredIn "Basics", "Float" ) [] ->
            encoder0 "float"

        NamedType ( DeclaredIn "String", "String" ) [] ->
            encoder0 "string"

        NamedType ( DeclaredIn "Json.Encode", "Value" ) [] ->
            val (from "Basics" "identity") |> use

        NamedType ( DeclaredIn "Json.Decode", "Value" ) [] ->
            val (from "Basics" "identity") |> use

        VariableType typeArgument ->
            val (local (encoderOf typeArgument)) |> use

        NamedType ( DeclaredIn "Maybe", "Maybe" ) [ el ] ->
            val (from "Maybe" "map")
                |> call ( el |> step "", [] )
                |> op composeR
                    (val (from "Maybe" "withDefault")
                        |> call ( val (jsonEncode "null"), [] )
                    )
                |> use

        NamedType ( DeclaredIn "List", "List" ) [ el ] ->
            encoder1 "list" el

        NamedType ( DeclaredIn "Array", "Array" ) [ el ] ->
            encoder1 "array" el

        NamedType ( DeclaredIn "Set", "Set" ) [ el ] ->
            encoder1 "set" el

        NamedType ( DeclaredIn "Result", "Result" ) [ err, ok ] ->
            typeEncoder
                { typeName = "Result", suffix = suffix }
                { variants =
                    ( ( "Err", [ err ] )
                    , [ ( "Ok", [ ok ] )
                      ]
                    )
                }
                |> use

        NamedType ( DeclaredIn "Dict", "Dict" ) [ keyType, valueType ] ->
            val (from "Dict" "toList")
                |> op composeR
                    (val (jsonEncode "list")
                        |> call
                            ( lambda
                                (Code.only
                                    (tuple2Pattern
                                        ( patternVar "key", patternVar "value" )
                                    )
                                )
                                (\key_ value_ ->
                                    val (jsonEncode "object")
                                        |> call
                                            ( tuple2
                                                ( string "key"
                                                , keyType
                                                    |> step "InKey"
                                                    |> call ( key_, [] )
                                                )
                                            , [ tuple2
                                                    ( string "value"
                                                    , valueType
                                                        |> step "InValue"
                                                        |> call ( value_, [] )
                                                    )
                                              ]
                                            )
                                )
                            , []
                            )
                    )
                |> use

        RecordType fields ->
            lambda (Code.only (patternVar "record"))
                (\record ->
                    val (jsonEncode "object")
                        |> call ( fieldEncoders record fields, [] )
                )
                |> use

        Tuple2Type ( first, second ) ->
            lambda
                (Code.only
                    (tuple2Pattern
                        ( patternVar (a 0), patternVar (a 1) )
                    )
                )
                (\a0 a1 ->
                    Code.list
                        [ tuplePartEncoder 0 first a0
                        , tuplePartEncoder 1 second a1
                        ]
                )
                |> use

        Tuple3Type ( first, second, third ) ->
            lambda
                (Code.only
                    (tuple3Pattern
                        ( patternVar (a 0), patternVar (a 1), patternVar (a 2) )
                    )
                )
                (\a0 a1 a2 ->
                    Code.list
                        [ tuplePartEncoder 0 first a0
                        , tuplePartEncoder 1 second a1
                        , tuplePartEncoder 2 third a2
                        ]
                )
                |> use

        ExtendedRecordType extended fields ->
            lambda (Code.only (patternVar "record"))
                (\record ->
                    let
                        givenFieldEncoders =
                            fields
                                |> List.NonEmpty.toList
                                |> fieldEncoders record

                        extraFieldEncoders =
                            extended ++ "FieldEncoders"
                    in
                    val (jsonEncode "object")
                        |> call
                            ( val (local extraFieldEncoders)
                                |> op append givenFieldEncoders
                            , []
                            )
                )
                |> use

        FunctionType _ _ ->
            val (from "Debug" "todo")
                |> call
                    ( Code.string
                        ([ "Encoder for `"
                         , typeAnnotation
                            |> typeToSyntax { imports = Dict.empty }
                            |> prettyTypeAnnotation
                            |> Pretty.pretty prettyWidth
                         , "`"
                         ]
                            |> String.concat
                        )
                    , []
                    )
                |> use

        NamedType reference arguments ->
            val reference |> use


a : Int -> String
a n =
    "a" ++ String.fromInt n


typeEncoder { typeName, suffix } { variants } =
    let
        opaqueEncoder ( constructorName, arguments ) =
            let
                constructorString =
                    val (jsonEncode "string")
                        |> call ( string (decapitalize constructorName), [] )
            in
            if isSimpleEnum variants then
                constructorString

            else
                let
                    argumentResults =
                        arguments
                            |> List.indexedMap
                                (\i arg ->
                                    let
                                        argument =
                                            arg
                                                |> typeAnnotationEncoder
                                                    { suffix = "In" ++ constructorName }
                                    in
                                    tuple2 ( string (a i), argument )
                                )
                in
                val (jsonEncode "object")
                    |> call
                        ( Code.list
                            (tuple2 ( string "tag", constructorString )
                                :: argumentResults
                            )
                        , []
                        )
    in
    let
        argument =
            decapitalize typeName ++ suffix
    in
    lambda (Code.only (var argument))
        (of_
            (List.NonEmpty.map
                (\( variantName, args ) ->
                    case_
                        (unionPattern (local variantName)
                            (args
                                |> List.indexedMap
                                    (\i _ -> Code.var (a i ++ suffix))
                                |> Code.combine
                            )
                        )
                        (\_ -> opaqueEncoder ( variantName, args ))
                )
                variants
            )
        )



--


jsonDecoderCode codable =
    case codable.kind of
        CodableTypeDeclaration typeInfo ->
            typeInfo
                |> typeDecoder
                    { typeName = codable.name
                    , suffix = ""
                    }

        CodableTypeAliasDeclaration typeAliasInfo ->
            typeAliasInfo.aliasedType
                |> typeAnnotationDecoder { suffix = "" }


decoderOf : String -> String
decoderOf structureName =
    structureName ++ "Decoder"


jsonDecode : String -> Origin
jsonDecode member =
    from "Json.Decode" member


jsonDecoder :
    TypeCode specific_
    -> TypeCode ( Origin, List (Code TypeAny) )
jsonDecoder value =
    namedType (jsonDecode "Decoder") [ value ]


argumentsForDecoder : TypeAny -> Dict String TypeAny
argumentsForDecoder typeAnnotation =
    let
        step =
            argumentsForDecoder

        argument : String -> TypeCode specific_ -> Dict String TypeAny
        argument name typeAnn =
            Dict.singleton name (typeAnn |> root |> .code |> .general)
    in
    case typeAnnotation of
        ExtendedRecordType extended fields ->
            let
                fieldArguments =
                    fields
                        |> List.NonEmpty.toList
                        |> List.map (Tuple.second >> step)
                        |> List.foldl Dict.union Dict.empty

                extraFieldDecoders =
                    "toExtended" ++ capitalize extended ++ "Decoder"
            in
            Dict.union
                (argument extraFieldDecoders
                    (( recordType
                        (fields
                            |> List.NonEmpty.toList
                            |> List.map
                                (\( fieldName, fieldType ) ->
                                    Code.field fieldName
                                        -- wrapped in a dummy CodeReference
                                        ({ this =
                                            { code = { general = fieldType }
                                            , imports = Dict.empty
                                            }
                                         , rootWith =
                                            identity
                                         }
                                            |> Reference.fromRecord
                                        )
                                )
                        )
                     , []
                     )
                        |> toType
                            (listType
                                (tuple2Type
                                    ( stringType, jsonValueType )
                                )
                            )
                    )
                )
                fieldArguments

        VariableType typeArgument ->
            let
                typeArgumentDecoder =
                    decoderOf typeArgument
            in
            argument typeArgumentDecoder
                (jsonDecoder (inferred typeArgument))


typeAnnotationDecoder { suffix } typeAnnotation =
    let
        step additionalSuffix =
            typeAnnotationDecoder
                { suffix = suffix ++ additionalSuffix }

        argument : String -> String
        argument simpleName =
            simpleName ++ suffix

        patternVar simpleName =
            var (argument simpleName)

        argumentVal simpleName =
            val (local (argument simpleName))

        recordDecoder fields =
            case fields of
                firstField :: followingFields ->
                    let
                        fieldNames =
                            topAndBelow firstField followingFields
                                |> Stack.map (\( name, _ ) -> name)
                    in
                    fieldDecoders
                        (lambda
                            (fieldNames
                                |> Stack.map patternVar
                                |> Code.combineIs
                            )
                            (\fieldArgs ->
                                record
                                    (Stack.map2
                                        (\name fieldArg ->
                                            Code.field name fieldArg
                                        )
                                        fieldNames
                                        fieldArgs
                                        |> List.NonEmpty.toList
                                    )
                            )
                        )
                        fields

        tupleDecoder parts =
            fieldDecoders
                (val (from "Tuple" "pair"))
                (parts
                    |> List.indexedMap (\i part -> ( a i, part ))
                )

        decoder1 structureDecoderName typeArgument =
            let
                element =
                    typeArgument |> step ""
            in
            val (jsonDecode structureDecoderName)
                |> call ( element.expression, [] )
                |> use

        decoderWithoutArguments structureDecoderName =
            val (jsonDecode structureDecoderName)
                |> use
    in
    case typeAnnotation of
        UnitType () ->
            val (jsonDecode "null")
                |> call ( unit (), [] )
                |> use

        NamedType ( DeclaredIn "Basics", "Bool" ) [] ->
            decoderWithoutArguments "bool"

        NamedType ( DeclaredIn "Basics", "Int" ) [] ->
            decoderWithoutArguments "int"

        NamedType ( DeclaredIn "Basics", "Float" ) [] ->
            decoderWithoutArguments "float"

        NamedType ( DeclaredIn "String", "String" ) [] ->
            decoderWithoutArguments "string"

        NamedType ( DeclaredIn "Json.Encode", "Value" ) [] ->
            decoderWithoutArguments "value"

        NamedType ( DeclaredIn "Json.Decode", "Value" ) [] ->
            decoderWithoutArguments "value"

        VariableType typeArgument ->
            val (local (decoderOf typeArgument))
                |> use

        NamedType ( DeclaredIn "Maybe", "Maybe" ) [ el ] ->
            decoder1 "nullable" el

        NamedType ( DeclaredIn "List", "List" ) [ el ] ->
            decoder1 "list" el

        NamedType ( DeclaredIn "Array", "Array" ) [ el ] ->
            decoder1 "array" el

        NamedType ( DeclaredIn "Set", "Set" ) [ el ] ->
            decoder1 "set" el

        NamedType ( DeclaredIn "Result", "Result" ) [ err, ok ] ->
            typeDecoder
                { typeName = "Result", suffix = suffix }
                { variants =
                    ( ( "Err", [ err ] )
                    , [ ( "Ok", [ ok ] )
                      ]
                    )
                }
                |> use

        NamedType ( DeclaredIn "Dict", "Dict" ) [ key, value ] ->
            val (from "Dict" "toList")
                |> op composeR
                    (val (jsonDecode "list")
                        |> call
                            ( recordDecoder
                                [ ( "key", key )
                                , ( "value", value )
                                ]
                            , []
                            )
                    )
                |> use

        RecordType fields ->
            recordDecoder fields

        Tuple2Type ( first, second ) ->
            tupleDecoder [ first, second ]

        Tuple3Type ( first, second, third ) ->
            tupleDecoder [ first, second, third ]

        ExtendedRecordType extended fields ->
            let
                recordWithoutExtendedFieldsDecoder =
                    recordDecoder (fields |> List.NonEmpty.toList)

                extraFieldDecoders =
                    "toExtended" ++ capitalize extended ++ "Decoder"
            in
            recordWithoutExtendedFieldsDecoder.expression
                |> op apR
                    (val (jsonDecode "andThen")
                        |> call ( val (local extraFieldDecoders), [] )
                    )

        FunctionType _ _ ->
            val (from "Debug" "todo")
                |> call
                    ( Code.string
                        ([ "Decoder for `"
                         , typeAnnotation
                            |> typeToSyntax { imports = Dict.empty }
                            |> prettyTypeAnnotation
                            |> Pretty.pretty prettyWidth
                         , "`"
                         ]
                            |> String.concat
                        )
                    , []
                    )
                |> use

        NamedType reference arguments ->
            let
                ( qualificationInThisModule, name ) =
                    reference
            in
            val
                ( qualificationInThisModule
                , decoderOf name
                )


fieldDecoders constructor fields =
    List.foldr
        (\f value -> value |> f)
        (val (jsonDecode "succeed")
            |> call ( constructor, [] )
            |> use
        )
        (fields
            |> List.map
                (\( fieldName, fieldType ) ->
                    let
                        fieldTypeDecoder =
                            fieldType
                                |> typeAnnotationDecoder
                                    { suffix = "In" ++ capitalize fieldName }
                    in
                    op apR
                        (val ("Json.Decode.Pipeline" "required")
                            |> call
                                ( string fieldName
                                , [ fieldTypeDecoder.expression ]
                                )
                            |> use
                        )
                )
        )


isSimpleEnum : StackFilled ( String, List TypeAny ) -> Bool
isSimpleEnum variants =
    List.NonEmpty.all (\( _, args ) -> args == [])
        variants


typeDecoder :
    { typeName : String, suffix : String }
    -> { namedType | variants : StackFilled ( String, List TypeAny ) }
    ->
        GeneralReference
            ( Code { kind : Expression leftKind_, general : ExpressionAny }
            , Operator
            , Code
                { function : Code { origin : Origin }
                , arguments : StackFilled (Code ExpressionAny)
                }
            )
            kind_
typeDecoder { typeName, suffix } { variants } =
    let
        opaqueDecoder ( constructorName, arguments ) =
            fieldDecoders
                (val (local constructorName))
                (arguments
                    |> List.indexedMap
                        (\i argumentType ->
                            ( a i, argumentType )
                        )
                )
    in
    let
        argument =
            decapitalize typeName ++ suffix

        tagConstructor =
            if isSimpleEnum variants then
                val (jsonDecode "string")
                    |> use

            else
                val (jsonDecode "field")
                    |> call ( string "tag", [ val (jsonDecode "string") ] )
                    |> use
    in
    tagConstructor
        |> op apR
            (val (jsonDecode "andThen")
                |> call
                    ( lambda (Code.only (var argument))
                        (of_
                            (List.NonEmpty.append
                                (variants
                                    |> List.NonEmpty.map
                                        (\( variantName, args ) ->
                                            case_
                                                (stringPattern
                                                    (variantName |> decapitalize)
                                                )
                                                (opaqueDecoder ( variantName, args ))
                                        )
                                )
                                (List.NonEmpty.singleton
                                    (let
                                        unknownTag =
                                            "unknownTag" ++ suffix
                                     in
                                     case_ (var unknownTag)
                                        (\unknownTagVar ->
                                            val (jsonDecode "problem")
                                                |> call
                                                    ( string "Unknown tag \""
                                                        |> op append unknownTagVar
                                                        |> op append
                                                            (string
                                                                ("\" doesn't match any "
                                                                    ++ typeName
                                                                    ++ " variant name"
                                                                )
                                                            )
                                                    , []
                                                    )
                                        )
                                    )
                                )
                            )
                        )
                    , []
                    )
            )



-- todo: yoink from [miniBill/elm-codec-generator](https://github.com/miniBill/elm-codec-generator)
-- todo: yoink from [json-to-elm](https://github.com/alexkorban/json-to-elm/blob/1.0.0/src/ElmCodeGenerator.elm)


type JsonValue
    = JsonString
    | JsonFloat
    | JsonInt
    | JsonBool
    | JsonList (List JsonValue)
    | JsonObject (List ( String, JsonValue ))
    | JsonNull


jsonValueDecoder : Json.Decode.Decoder JsonValue
jsonValueDecoder =
    Json.Decode.oneOf
        [ Json.Decode.string |> Json.Decode.map (\_ -> JsonString)
        , Json.Decode.int |> Json.Decode.map (\_ -> JsonInt)
        , Json.Decode.float |> Json.Decode.map (\_ -> JsonFloat)
        , Json.Decode.bool |> Json.Decode.map (\_ -> JsonBool)
        , Json.Decode.list
            (Json.Decode.lazy (\() -> jsonValueDecoder))
            |> Json.Decode.map JsonList
        , Json.Decode.keyValuePairs
            (Json.Decode.lazy (\() -> jsonValueDecoder))
            |> Json.Decode.map JsonObject
        , Json.Decode.null JsonNull
        ]
