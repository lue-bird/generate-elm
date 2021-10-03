module Elm.Generator.RecordFieldHelper exposing
    ( Generator
    , accessors, monocle, focus, fields, zipper, update, set
    , functionsForField, getSetRecordForField
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

See [`Review.Generate.rule`](https://package.elm-lang.org/packages/lue-bird/elm-review-generate/latest/Review-Generate#rule)


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

@docs accessors, monocle, focus, fields, zipper, update, set


### custom

@docs functionsForField, getSetRecordForField

-}

import Elm.CodeGen as CodeGen
import Elm.Generator as Generator exposing (AllowsUnqualified, RequiresQualified)
import Parser exposing ((|.), (|=))
import Util exposing (updateFirstChar)


{-| How to generate a record field helper declaration plus the necessary imports.

Out of the box there is support for

  - [`updateField`](#update), [`setField`](#set)
  - [`elm-accessors`](#accessors),
  - [`elm-monocle`](#monocle),
  - [`focus`](#focus),
  - [`elm-fields`](#fields),
  - [`zipper`](#zipper)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    import Elm.Generator as Generator exposing (RequiresQualified)
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    customLens : RecordFieldHelper.Generator RequiresQualified
    customLens =
        Generator.forDeclaration "CustomLens"
            (Generator.acceptEveryQualifiedName
                (\name -> { fieldName = name })
            )
            (\{ declarationName, fq } { fieldName } ->
                Generator.valueDeclaration declarationName
                    (let
                        { access, set } =
                            functionsForField fieldName
                     in
                     fq.construct ( "CustomLens", [] ) "create" [ access, set ]
                    )
                    |> Generator.withAnnotation
                        (fq.typed ( "CustomLens", [] )
                            "CustomLens"
                            [ extRecordAnn "record"
                                [ ( fieldName, typeVar fieldName ) ]
                            , typeVar fieldName
                            ]
                        )
                    |> Generator.withDocumentation
                        (emptyDocComment
                            |> markdown
                                ("`CustomLens` for the field `." ++ fieldName ++ "`.")
                        )
                    |> Generator.withImports
                        [ Generator.importModule [ "CustomLens" ]
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
        Generator.FunctionDeclaration


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
        (\{ declarationName, fq } { fieldName } ->
            let
                function =
                    functionsForField fieldName
            in
            Generator.valueDeclaration declarationName
                (fq.construct ( "Accessors", [] )
                    "makeOneToOne"
                    [ .access function, .update function ]
                )
                |> Generator.withAnnotation
                    (let
                        relation super =
                            fq.typed ( "Accessors", [] )
                                "Relation"
                                [ super
                                , CodeGen.typeVar "sub"
                                , CodeGen.typeVar "wrap"
                                ]
                     in
                     CodeGen.funAnn
                        (relation (CodeGen.typeVar fieldName))
                        (relation
                            (CodeGen.extRecordAnn "record"
                                [ ( fieldName, CodeGen.typeVar fieldName ) ]
                            )
                        )
                    )
                |> Generator.withImports
                    [ Generator.importModule ( "Accessors", [] )
                        |> Generator.withExposing
                            [ CodeGen.funExpose "makeOneToOne"
                            , CodeGen.typeOrAliasExpose "Relation"
                            ]
                    ]
        )


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
        (\{ declarationName, fq } { fieldName } ->
            Generator.valueDeclaration declarationName
                (getSetRecordForField fieldName)
                |> Generator.withAnnotation
                    (fq.typed ( "Monocle", [ "Lens" ] )
                        "Lens"
                        [ CodeGen.extRecordAnn "record"
                            [ ( fieldName, CodeGen.typeVar fieldName ) ]
                        , CodeGen.typeVar fieldName
                        ]
                    )
                |> Generator.withImports
                    [ Generator.importModule ( "Monocle", [ "Lens" ] )
                        |> Generator.withExposing
                            [ CodeGen.typeOrAliasExpose "Lens" ]
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
        (\{ declarationName, fq } { fieldName } ->
            Generator.valueDeclaration declarationName
                (let
                    function =
                        functionsForField fieldName
                 in
                 fq.construct ( "Focus", [] )
                    "create"
                    [ .access function, .update function ]
                )
                |> Generator.withAnnotation
                    (fq.typed ( "Focus", [] )
                        "Focus"
                        [ CodeGen.extRecordAnn "record"
                            [ ( fieldName, CodeGen.typeVar fieldName ) ]
                        , CodeGen.typeVar fieldName
                        ]
                    )
                |> Generator.withImports
                    [ Generator.importModule ( "Focus", [] )
                        |> Generator.withExposing
                            [ CodeGen.typeOrAliasExpose "Focus" ]
                    ]
        )


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
            (Parser.succeed (\name -> { fieldName = name })
                |. Parser.symbol "into"
                |= (Parser.chompWhile (\_ -> True)
                        |> Parser.getChompedString
                        |> Parser.map (updateFirstChar Char.toLower)
                   )
                |. Parser.end
            )
        )
        (\{ declarationName, fq } { fieldName } ->
            Generator.valueDeclaration declarationName
                (let
                    function =
                        functionsForField fieldName
                 in
                 fq.construct ( "Zipper", [] )
                    "into"
                    [ .access function, .set function ]
                )
                |> Generator.withAnnotation
                    (let
                        zipperType focus_ root =
                            fq.typed ( "Zipper", [] ) "Zipper" [ focus_, root ]

                        fieldType =
                            CodeGen.typeVar fieldName

                        rootType =
                            CodeGen.typeVar "root"
                     in
                     CodeGen.funAnn
                        (zipperType
                            (CodeGen.extRecordAnn "record"
                                [ ( fieldName, fieldType ) ]
                            )
                            rootType
                        )
                        (zipperType fieldType rootType)
                    )
                |> Generator.withImports
                    [ Generator.importModule ( "Zipper", [] )
                        |> Generator.withExposing
                            [ CodeGen.typeOrAliasExpose "Zipper"
                            , CodeGen.funExpose "into"
                            ]
                    ]
        )


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
            Generator.valueDeclaration declarationName
                (getSetRecordForField fieldName)
                |> Generator.withAnnotation
                    (let
                        fieldType =
                            CodeGen.typeVar fieldName

                        recordType name =
                            CodeGen.extRecordAnn name
                                [ ( fieldName, fieldType ) ]
                     in
                     CodeGen.recordAnn
                        [ ( "get"
                          , CodeGen.funAnn (recordType "a") fieldType
                          )
                        , ( "set"
                          , CodeGen.funAnn
                                fieldType
                                (CodeGen.funAnn (recordType "b") (recordType "b"))
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
            (Parser.succeed (\name -> { fieldName = name })
                |. Parser.symbol "update"
                |= (Parser.chompWhile (\_ -> True)
                        |> Parser.getChompedString
                        |> Parser.map (updateFirstChar Char.toLower)
                   )
                |. Parser.end
            )
        )
        (\{ declarationName } { fieldName } ->
            Generator.functionDeclaration declarationName
                [ CodeGen.varPattern "f"
                , CodeGen.varPattern "record"
                ]
                (CodeGen.update "record"
                    [ ( fieldName
                      , CodeGen.construct "f"
                            [ CodeGen.access (CodeGen.val "record") fieldName ]
                      )
                    ]
                )
                |> Generator.withAnnotation
                    (let
                        fieldType =
                            CodeGen.typeVar fieldName

                        recordType name =
                            CodeGen.extRecordAnn name
                                [ ( fieldName, fieldType ) ]
                     in
                     CodeGen.funAnn
                        (CodeGen.funAnn fieldType fieldType)
                        (CodeGen.funAnn (recordType "record")
                            (recordType "record")
                        )
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
            (Parser.succeed (\name -> { fieldName = name })
                |. Parser.symbol "set"
                |= (Parser.chompWhile (\_ -> True)
                        |> Parser.getChompedString
                        |> Parser.map (updateFirstChar Char.toLower)
                   )
                |. Parser.end
            )
        )
        (\{ declarationName } { fieldName } ->
            Generator.functionDeclaration declarationName
                [ CodeGen.varPattern (fieldName ++ "_")
                , CodeGen.varPattern "record"
                ]
                (CodeGen.update "record"
                    [ ( fieldName
                      , CodeGen.val (fieldName ++ "_")
                      )
                    ]
                )
                |> Generator.withAnnotation
                    (let
                        fieldType =
                            CodeGen.typeVar fieldName

                        recordType =
                            CodeGen.extRecordAnn "record"
                                [ ( fieldName, fieldType ) ]
                     in
                     CodeGen.funAnn
                        fieldType
                        (CodeGen.funAnn recordType recordType)
                    )
        )



--


{-| Generate a field lens implementation in the form

    { get = .score, set = \score_ r -> { r | score = score_ } }

This is equivalent to

    let
        { access, set } =
            functionsForField fieldName
    in
    record [ ( "get", access ), ( "set", set ) ]

-}
getSetRecordForField : String -> CodeGen.Expression
getSetRecordForField fieldName =
    let
        functions =
            functionsForField fieldName
    in
    CodeGen.record
        [ ( "get", .access functions ), ( "set", .set functions ) ]


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
        { access : CodeGen.Expression
        , set : CodeGen.Expression
        , update : CodeGen.Expression
        }
functionsForField fieldName =
    { access = CodeGen.accessFun ("." ++ fieldName)
    , set =
        CodeGen.lambda
            [ CodeGen.varPattern (fieldName ++ "_")
            , CodeGen.varPattern "r"
            ]
            (CodeGen.update "r"
                [ ( fieldName
                  , CodeGen.val (fieldName ++ "_")
                  )
                ]
            )
    , update =
        CodeGen.lambda
            [ CodeGen.varPattern "f"
            , CodeGen.varPattern "r"
            ]
            (CodeGen.update "r"
                [ ( fieldName
                  , CodeGen.construct "f"
                        [ CodeGen.access (CodeGen.val "r") fieldName ]
                  )
                ]
            )
    }
