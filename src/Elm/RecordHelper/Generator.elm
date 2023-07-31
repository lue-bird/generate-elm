module Elm.RecordHelper.Generator exposing
    ( Generator
    , accessors, optics, monocle, focus, fields, zipper, update, set
    , fieldNameParserUntil, functionsForField, getSetRecordForField
    )

{-| Often, I find myself writing code like this:

    InputAdded path newInput ->
        { model
            | projects =
                model.projects
                    |> ZipList.updateSelected
                        (\project ->
                            { project
                                | calls =
                                    project.calls
                                        |> List.map
                                            (Tree.updateAt path
                                                (Tree.prependChild newInput)
                                            )
                            }
                        )
        }

which is ... suboptimal.


#### Using some `update` helpers:

    import GenerateMissingDeclarations
    import Record exposing (..)

    InputAdded path newInput ->
        model
            |> updateProjects
                (ZipList.updateSelected
                    (updateCalls
                        (List.map
                            (Tree.updateAt path
                                (Tree.prependChild newInput)
                            )
                        )
                    )
                )


#### Using lenses:

    import Field
    import Accessors exposing (over)
    import Accessors.Library exposing (onEach)

    InputAdded path newInput ->
        model
            |> over
                ((Field.projects << ZipList.selected)
                    << Field.calls
                    << onEach
                )
                (Tree.updateAt path
                    (Tree.prependChild newInput)
                )

Using methods like these helps making your code **readable**.

The biggest pain-point with these helpers: you need to manually create one for every used field.

→ Automatically generate record field helpers that don't exist yet.

  - In example 0: `updateProjects` & `updateCalls` will be generated in `Record`
  - In example 1: `Field.projects` & `Field.calls` will be generated in `Field`


#### helpers that work out of the box:

  - [`updateField`](#update), [`setField`](#set)
  - [`elm-accessors` lens](#accessors),
  - [`elm-optics` lens](#optics)
  - [`elm-monocle` lens](#monocle),
  - [`focus` lens](#focus),
  - [`elm-fields` lens](#fields),
  - [`zipper` `intoField` function](#zipper) or

Note: Generate custom helpers using [`Elm.Generator.forDeclaration`](Elm-Generator#forDeclaration).


## configuration

    module ReviewConfig exposing (config)

    import Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Review.Generate
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ Review.Generate.inModule
            [ "Accessors", "Library", "Fields" ]
            RecordFieldHelper.accessors
            |> Review.Generate.rule
        ]

See [`Review.Generate.rule`](https://package.elm-lang.org/packages/lue-bird/generate-elm/latest/Review-Generate#rule)


## Example

    module Player exposing (scoreAPoint)

    import Accessors.Library.Fields as Field

    scoreAPoint =
        Accessors.over Field.score ((+) 1)


### Fail

    module Accessors.Library.Fields exposing (name)
    ...


### Success

    module Accessors.Library.Fields exposing (score)
    ...

@docs Generator


## available out of the box

@docs accessors, optics, monocle, focus, fields, zipper, update, set


### custom

@docs fieldNameParserUntil, functionsForField, getSetRecordForField

-}

import Elm.Code as Code exposing (Code, Expression, ExpressionAny, Field, FunctionLiteralExpression, GeneralReference, Origin, PatternAny, RecordLiteralExpression, access, addTypeAndAliasExposes, addValueExposes, call, empty, field, from, import_, inferred, local, namedType, next, only, record, recordExtendedBy, toType, typeParameter, updateFields, use, val, var, withExposing)
import Elm.Code.Generator as Generator exposing (AllowsUnqualified, RequiresQualified)
import Elm.CodeGen as CodeGen
import List.NonEmpty exposing (StackFilled)
import ListIs exposing (ListIs, NotEmpty)
import Parser exposing ((|.), (|=), Parser)


{-| How to generate a record field helper declaration plus the necessary imports.

Out of the box there is support for

  - [`updateField`](#update), [`setField`](#set)
  - [`elm-accessors`](#accessors),
  - [`elm-optics`](#optics)
  - [`elm-monocle`](#monocle),
  - [`focus`](#focus),
  - [`elm-fields`](#fields),
  - [`zipper`](#zipper)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    import Elm.CodeGen exposing (typeVar)
    import Elm.Generator as Generator exposing (RequiresQualified, call, type_, val)
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    customLens : RecordFieldHelper.Generator RequiresQualified
    customLens =
        Generator.forDeclaration "CustomLens"
            (Generator.acceptEveryQualifiedName
                (\name -> { fieldName = name })
            )
            (\{ declarationName, from } { fieldName } ->
                Generator.valueDeclaration declarationName
                    (let
                        { access, set } =
                            functionsForField fieldName
                     in
                     call (val (from [ "CustomLens" ] "create"))
                        [ access, set ]
                    )
                    |> Code.withAnnotation
                        (type_ (from [ "CustomLens" ] "CustomLens")
                            [ extRecordAnn "record"
                                [ ( fieldName, typeVar fieldName ) ]
                            , typeVar fieldName
                            ]
                        )
                    |> Code.withDocumentation
                        (emptyDocComment
                            |> markdown
                                ("`CustomLens` for the field `." ++ fieldName ++ "`.")
                        )
                    |> Generator.withImports
                        [ Generator.import_ [ "CustomLens" ]
                            |> Generator.withExposing
                                [ typeOrAliasExpose "CustomLens" ]
                        ]
            )

`customLens` will generate lenses in the form

    {-| `CustomLens` for the field `.score`.
    -}
    score : CustomLens { record | score : score } score
    score =
        CustomLens.create .score (\score_ r -> { r | score = score_ })

-}
type alias Generator requireQualified =
    Generator.DeclarationGenerator
        requireQualified
        { fieldName : String }
        Code.FunctionOrValueModuleScopeDeclaration


{-| Generate lenses for [bChiquet's elm-accessors](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest) in the form

    import Accessors exposing (Relation, makeOneToOne)

    score : Relation score sub wrap -> Relation { record | score : score } sub wrap
    score =
        makeOneToOne .score (\f r -> { r | score = f r.score })

Check out [`Elm.Generator.replaceChecker`](Elm-Generator#replaceChecker) if you prefer to use a prefix/suffix/..., for example:

    import Accessors

    scoreAPoint =
        Accessors.over scoreAccessor ((+) 1)

-}
accessors : Generator RequiresQualified
accessors =
    Generator.forDeclaration "elm-accessors lens"
        (Generator.acceptEveryQualifiedName
            (\name -> { fieldName = name })
        )
        (\{ declarationName } { fieldName } ->
            Code.valueDeclaration declarationName
                (let
                    function =
                        functionsForField fieldName
                 in
                 val (fromAccessors "makeOneToOne")
                    |> call ( .access function, [ .update function ] )
                )
                |> Code.withType
                    (\f -> f (inferred fieldName) (inferred "record") (inferred "sub") (inferred "wrap"))
                    (\fieldType record sub wrap ->
                        let
                            relation super =
                                namedType (fromAccessors "Relation")
                                    [ use super, use sub, use wrap ]
                        in
                        ( relation fieldType, [] )
                            |> toType
                                (relation
                                    (record
                                        |> Code.recordExtendedBy
                                            (field fieldName fieldType)
                                            []
                                    )
                                )
                    )
                |> Code.withImports
                    [ import_ "Accessors"
                        |> Code.withExposing
                            (addValueExposes [ "makeOneToOne" ]
                                >> addTypeAndAliasExposes [ "Relation" ]
                            )
                    ]
        )


fromAccessors : String -> Origin
fromAccessors member =
    from "Accessors" member


{-| Generate lenses for [Heimdell's elm-optics](https://package.elm-lang.org/packages/Heimdell/elm-optics/latest/) in the form

    score : SimpleLens ls { record | score : score } score
    score =
        lens .score (\r score_ -> { r | score = score_ })

Check out [`Elm.Generator.replaceChecker`](Elm-Generator#replaceChecker) if you prefer to use a prefix/suffix/..., for example:

    import Accessors

    scoreAPoint =
        Optics.Core.over (o score_) ((+) 1)

TODO check if correct

-}
optics : Generator RequiresQualified
optics =
    Debug.todo ""


{-| Generate lenses for [arturopala's elm-monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest) in the form

    import Monocle.Lens exposing (Lens)

    score : Lens { record | score : score } score
    score =
        { get = .score, set = \score_ r -> { r | score = score_ } }

Check out [`Elm.Generator.replaceChecker`](Elm-Generator#replaceChecker) if you prefer to use a prefix/suffix/..., for example:

    import Monocle.Lens

    scoreAPoint =
        Monocle.Lens.modify scoreLens ((+) 1)

-}
monocle : Generator RequiresQualified
monocle =
    Generator.forDeclaration "elm-monocle lens"
        (Generator.acceptEveryQualifiedName
            (\name -> { fieldName = name })
        )
        (\{ declarationName } { fieldName } ->
            Code.valueDeclaration declarationName
                (getSetRecordForField fieldName)
                |> Code.withType
                    (\f -> f (inferred "record") (inferred fieldName))
                    (\record field ->
                        namedType (from "Monocle.Lens" "Lens")
                            [ record
                                |> recordExtendedBy
                                    (Code.field fieldName field)
                                    []
                                |> use
                            , use field
                            ]
                    )
                |> Code.withImports
                    [ import_ "Monocle.Lens"
                        |> withExposing
                            (addTypeAndAliasExposes [ "Lens" ])
                    ]
        )


{-| Generate lenses for [ccapndave's focus](https://package.elm-lang.org/packages/ccapndave/focus/latest) in the form

    import Focus exposing (Focus)

    score : Focus { record | score : score } score
    score =
        Focus.create .score (\f r -> { r | score = f r.score })

Check out [`Elm.Generator.replaceChecker`](Elm-Generator#replaceChecker) if you prefer to use a prefix/suffix/..., for example:

    import Focus

    scoreAPoint =
        Focus.update scoreFocus ((+) 1)

-}
focus : Generator RequiresQualified
focus =
    Generator.forDeclaration "focus lens"
        (Generator.acceptEveryQualifiedName
            (\name -> { fieldName = name })
        )
        (\{ declarationName } { fieldName } ->
            Code.valueDeclaration declarationName
                (let
                    function =
                        functionsForField fieldName
                 in
                 val (fromFocus "create")
                    |> call ( .access function, [ .update function ] )
                )
                |> Code.withType
                    (\f -> f (inferred fieldName) (inferred "record"))
                    (\field record ->
                        namedType (fromFocus "Focus")
                            [ record
                                |> recordExtendedBy
                                    (Code.field fieldName field)
                                    []
                                |> use
                            , use field
                            ]
                    )
                |> Code.withImports
                    [ import_ "Focus"
                        |> withExposing
                            (addTypeAndAliasExposes [ "Focus" ])
                    ]
        )


fromFocus : String -> Origin
fromFocus member =
    from "Focus" member


{-| Generate lenses for [z5h's zipper](https://package.elm-lang.org/packages/z5h/zipper/latest/) in the form

    import Zipper exposing (Zipper, into)

    intoScore : Zipper { record | score : score } root -> Zipper score root
    intoScore =
        into .score (\score_ r -> { r | score = score_ })

-}
zipper : Generator AllowsUnqualified
zipper =
    Generator.forDeclaration "zipper lens"
        (Generator.nameChecker
            (Parser.succeed identity
                |. Parser.symbol "into"
                |= fieldNameParserUntil Parser.end
            )
        )
        (\{ declarationName } { fieldName } ->
            Code.valueDeclaration declarationName
                (let
                    function =
                        functionsForField fieldName
                 in
                 val (fromZipper "into")
                    |> call ( .access function, [ .set function ] )
                )
                |> Code.withType
                    (\f -> f (inferred fieldName) (inferred "root") (inferred "record"))
                    (\fieldType root record ->
                        let
                            zipperType focus_ =
                                namedType (fromZipper "Zipper")
                                    [ use focus_, use root ]
                        in
                        ( zipperType
                            (record
                                |> recordExtendedBy
                                    (field fieldName fieldType)
                                    []
                            )
                        , []
                        )
                            |> toType (zipperType fieldType)
                    )
                |> Code.withImports
                    [ import_ "Zipper"
                        |> withExposing
                            (addTypeAndAliasExposes [ "Zipper" ]
                                >> addValueExposes [ "into" ]
                            )
                    ]
        )


fromZipper : String -> Origin
fromZipper member =
    from "Zipper" member


{-| Generate lenses for [sjorn3's elm-fields](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/) in the form

    score :
        { get : { a | score : score } -> score
        , set : score -> { b | score : score } -> { b | score : score }
        }
    score =
        { get = .score, set = \score_ r -> { r | score = score_ } }

Check out [`Elm.Generator.replaceChecker`](Elm-Generator#replaceChecker) if you prefer to use a prefix/suffix/..., for example:

    import FieldLens

    scoreAPoint =
        FieldLens.modify scoreField ((+) 1)

-}
fields : Generator RequiresQualified
fields =
    Generator.forDeclaration "elm-fields lens"
        (Generator.acceptEveryQualifiedName
            (\name -> { fieldName = name })
        )
        (\{ declarationName } { fieldName } ->
            Code.valueDeclaration declarationName
                (getSetRecordForField fieldName)
                |> Code.withType
                    (\f -> f (inferred "a") (inferred "b") (inferred fieldName))
                    (\a b fieldType ->
                        let
                            extRecordType =
                                recordExtendedBy (field fieldName fieldType) []
                        in
                        Code.recordType
                            [ Code.field "get"
                                (( extRecordType a, [] ) |> toType fieldType)
                            , Code.field "set"
                                (( fieldType, [ extRecordType b ] )
                                    |> toType (extRecordType b)
                                )
                            ]
                    )
        )


{-| Generate updaters in the form

    updateScore : (score -> score) -> { record | score : score } -> { record | score : score }
    updateScore f record =
        { record | score = f record.score }

Check out [`Elm.Generator.replaceChecker`](Elm-Generator#replaceChecker) if you prefer a different prefix, for example:

    mapScore ((+) 1)
    changeScore ((+) 1)

    import UpdateField
    UpdateField.score ((+) 1)

-}
update : Generator AllowsUnqualified
update =
    Generator.forDeclaration "field update helper"
        (Generator.nameChecker
            (Parser.succeed identity
                |. Parser.symbol "update"
                |= fieldNameParserUntil Parser.end
            )
        )
        (\{ declarationName } { fieldName } ->
            Code.functionOrValueModuleScopeDeclaration declarationName
                (only (var "f") |> next (var "record"))
                (\f record ->
                    record
                        |> updateFields
                            (field fieldName
                                (f |> call ( record |> access.field fieldName, [] ))
                            )
                            []
                )
                |> Code.withType
                    (\f -> f (inferred fieldName) (inferred "record"))
                    (\fieldType record ->
                        let
                            recordType =
                                recordExtendedBy (field fieldName fieldType) []
                        in
                        ( ( fieldType, [] ) |> toType fieldType
                        , [ recordType record ]
                        )
                            |> toType (recordType record)
                    )
        )


{-| Generate setters in the form

    setScore : score -> { record | score : score } -> { record | score : score }
    setScore score_ record =
        { record | score = score_ } }

Check out [`Elm.Generator.replaceChecker`](Elm-Generator#replaceChecker) if you prefer a different prefix:

    import Field

    resetScore =
        Field.replaceScore 0

or

    import SetField

    resetScore =
        SetField.score 0

etc.

-}
set : Generator AllowsUnqualified
set =
    Generator.forDeclaration "field setter"
        (Generator.nameChecker
            (Parser.succeed identity
                |. Parser.symbol "set"
                |= fieldNameParserUntil Parser.end
            )
        )
        (\{ declarationName } { fieldName } ->
            Code.functionOrValueModuleScopeDeclaration declarationName
                (only (var (fieldName ++ "_"))
                    |> next (var "record")
                )
                (\newValue ->
                    updateFields (field fieldName newValue) []
                )
                |> Code.withType
                    (\f -> f (inferred "record") (inferred fieldName))
                    (\record fieldType ->
                        let
                            recordType =
                                record
                                    |> recordExtendedBy
                                        (field fieldName fieldType)
                                        []
                        in
                        ( fieldType, [ recordType ] ) |> toType recordType
                    )
        )



--


{-| [`Parser`](https://package.elm-lang.org/packages/elm/parser/latest) that returns a valid record field name:

    import Elm.Generator.RecordFieldHelper exposing (fieldNameParserUntil)

    "fieldName"
        |> Parser.run (fieldNameParserUntil Parser.end)
    --> Ok { fieldName = "fieldName" }

    "FieldName"
        |> Parser.run (fieldNameParserUntil Parser.end)
    --> Ok { fieldName = "fieldName" }

    "_fieldName"
        |> Parser.run (fieldNameParserUntil Parser.end)
    --→ Err _

    "3fieldName"
        |> Parser.run (fieldNameParserUntil Parser.end)
    --→ Err _

You can use this to conveniently build record field helper declaration name parsers.

    import Elm.Generator.RecordFieldHelper as RecordFieldHelper exposing (fieldNameParserUntil)
    import Parser exposing ((|.))

    setterNameParser : Parser { fieldName : String }
    setterNameParser =
        Parser.succeed identity
            |. Parser.symbol "set"
            |= fieldNameParserUntil Parser.end

which can be used to replace existing name parsers

    import Elm.Generator as Generator exposing (AllowUnqualified)
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper exposing (fieldNameParserUntil)
    import Parser exposing ((|.))

    focusField : RecordFieldHelper.Generator AllowUnqualified
    focusField =
        RecordFieldHelper.focus
            |> Generator.replaceChecker
                (Generator.nameChecker
                    (fieldNameParserUntil
                        (Parser.symbol "Focus" |. Parser.end)
                    )
                )

Note: `fieldNameParserUntil` is equivalent to

    import Elm.Generator exposing (lowerNameParserUntil)
    import Parser exposing ((|=))

    Parser.succeed (\name -> { fieldName = name })
        |= lowerNameParserUntil <suffixParser>

-}
fieldNameParserUntil : Parser a_ -> Parser { fieldName : String }
fieldNameParserUntil suffixParser =
    Code.lowerNameParserUntil suffixParser
        |> Parser.map (\name -> { fieldName = name })



--


{-| Generate a field lens implementation in the form

    { get = .score, set = \score_ r -> { r | score = score_ } }

This is equivalent to

    import Elm.Generator.RecordFieldHelper exposing (functionsForField)
    import Elm.Code exposing (record)

    let
        functions =
            functionsForField fieldName
    in
    record
        [ ( "get", .access functions )
        , ( "set", .set functions )
        ]

-}
getSetRecordForField : String -> RecordExpression
getSetRecordForField fieldName =
    let
        functions =
            functionsForField fieldName
    in
    record
        [ field "get" (.access functions)
        , field "set" (.set functions)
        ]


{-| The access, set and update functions for a record field in the form

    -- access
    .score

    -- set
    \score_ r -> { r | score = score_ }

    -- update
    \f r -> { r | score = f r.score }

-}
functionsForField :
    String
    ->
        { access : GeneralReference { field : String } FunctionLiteralExpression
        , set :
            GeneralReference
                { arguments : ListIs NotEmpty (Code PatternAny)
                , result : Code ExpressionAny
                }
                FunctionLiteralExpression
        , update :
            GeneralReference
                { arguments : ListIs NotEmpty (Code PatternAny)
                , result : Code ExpressionAny
                }
                FunctionLiteralExpression
        }
functionsForField fieldName =
    { access = Code.accessFun .field fieldName
    , set =
        Code.lambda
            (only (Code.var (fieldName ++ "_"))
                |> next (Code.var "r")
            )
            (\newFieldValue ->
                updateFields (field fieldName newFieldValue) []
            )
    , update =
        Code.lambda
            (only (Code.var "f") |> next (Code.var "r"))
            (\f r ->
                r
                    |> updateFields
                        (field fieldName
                            (f |> call ( r |> access.field fieldName, [] ))
                        )
                        []
            )
    }
