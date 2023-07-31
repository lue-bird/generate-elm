module Elm.Generator.RecordFieldHelper.Test exposing (all)

import Elm.Code as Code
import Elm.Code.Generator as Generator
import Elm.RecordHelper.Generator exposing (accessors, fieldNameParserUntil, fields, focus, monocle, set, update, zipper)
import Expect
import Parser exposing ((|.))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Elm.Generator.RecordFieldHelper"
        [ declarations
        , describe "fieldNameParserUntil"
            [ test "uppercase succeeds"
                (\() ->
                    "FieldName"
                        |> Parser.run (fieldNameParserUntil Parser.end)
                        |> Expect.equal (Ok { fieldName = "fieldName" })
                )
            , test "lowercase succeeds"
                (\() ->
                    "fieldName"
                        |> Parser.run (fieldNameParserUntil Parser.end)
                        |> Expect.equal (Ok { fieldName = "fieldName" })
                )
            , test "starting with _ fails"
                (\() ->
                    "_fieldName"
                        |> Parser.run (fieldNameParserUntil Parser.end)
                        |> Expect.err
                )
            , test "starting with digit fails"
                (\() ->
                    "3fieldName"
                        |> Parser.run (fieldNameParserUntil Parser.end)
                        |> Expect.err
                )
            , test "non-alpha-numeric fails"
                (\() ->
                    "field-name"
                        |> Parser.run
                            (fieldNameParserUntil Parser.end)
                        |> Expect.err
                )
            , test "with suffix"
                (\() ->
                    "fieldNameFocus"
                        |> Parser.run
                            (fieldNameParserUntil
                                (Parser.symbol "Focus")
                                |. Parser.end
                            )
                        |> Expect.equal (Ok { fieldName = "fieldName" })
                )
            ]
        ]


declarations : Test
declarations =
    describe "kinds of declarations"
        [ test accessors.description
            (\() ->
                accessors.what
                    |> Code.printUsingSpecifiedImports "score"
                        { fieldName = "score" }
                    |> Expect.equal
                        """score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })"""
            )
        , test monocle.description
            (\() ->
                monocle.what
                    |> Code.printUsingSpecifiedImports "score"
                        { fieldName = "score" }
                    |> Expect.equal
                        """score : Lens { record | score : score } score
score =
    { get = .score, set = \\score_ r -> { r | score = score_ } }"""
            )
        , test focus.description
            (\() ->
                focus.what
                    |> Code.printUsingSpecifiedImports "score"
                        { fieldName = "score" }
                    |> Expect.equal
                        """score : Focus { record | score : score } score
score =
    Focus.create .score (\\f r -> { r | score = f r.score })"""
            )
        , test fields.description
            (\() ->
                fields.what
                    |> Code.printUsingSpecifiedImports "score"
                        { fieldName = "score" }
                    |> Expect.equal
                        """score :
    { get : { a | score : score } -> score
    , set : score -> { b | score : score } -> { b | score : score }
    }
score =
    { get = .score, set = \\score_ r -> { r | score = score_ } }"""
            )
        , test zipper.description
            (\() ->
                zipper.what
                    |> Code.printUsingSpecifiedImports "intoScore"
                        { fieldName = "score" }
                    |> Expect.equal
                        """intoScore : Zipper { record | score : score } root -> Zipper score root
intoScore =
    into .score (\\score_ r -> { r | score = score_ })"""
            )
        , test set.description
            (\() ->
                set.what
                    |> Code.printUsingSpecifiedImports "setScore"
                        { fieldName = "score" }
                    |> Expect.equal
                        """setScore : score -> { record | score : score } -> { record | score : score }
setScore score_ record =
    { record | score = score_ }"""
            )
        , test update.description
            (\() ->
                update.what
                    |> Code.printUsingSpecifiedImports "updateScore"
                        { fieldName = "score" }
                    |> Expect.equal
                        """updateScore : (score -> score) -> { record | score : score } -> { record | score : score }
updateScore f record =
    { record | score = f record.score }"""
            )
        ]
