module Elm.Generator.RecordFieldHelper.Test exposing (all)

import Elm.Generator as Generator
import Elm.Generator.RecordFieldHelper exposing (accessors, fields, focus, monocle, set, update, zipper)
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Elm.Generator.RecordFieldHelper"
        [ declarations ]


declarations : Test
declarations =
    describe "kinds of declarations"
        [ test accessors.description
            (\() ->
                accessors.elm
                    |> Generator.printUsingSpecifiedImports "score"
                        { fieldName = "score" }
                    |> Expect.equal
                        """score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })"""
            )
        , test monocle.description
            (\() ->
                monocle.elm
                    |> Generator.printUsingSpecifiedImports "score"
                        { fieldName = "score" }
                    |> Expect.equal
                        """score : Lens { record | score : score } score
score =
    { get = .score, set = \\score_ r -> { r | score = score_ } }"""
            )
        , test focus.description
            (\() ->
                focus.elm
                    |> Generator.printUsingSpecifiedImports "score"
                        { fieldName = "score" }
                    |> Expect.equal
                        """score : Focus { record | score : score } score
score =
    Focus.create .score (\\f r -> { r | score = f r.score })"""
            )
        , test fields.description
            (\() ->
                fields.elm
                    |> Generator.printUsingSpecifiedImports "score"
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
                zipper.elm
                    |> Generator.printUsingSpecifiedImports "intoScore"
                        { fieldName = "score" }
                    |> Expect.equal
                        """intoScore : Zipper { record | score : score } root -> Zipper score root
intoScore =
    into .score (\\score_ r -> { r | score = score_ })"""
            )
        , test set.description
            (\() ->
                set.elm
                    |> Generator.printUsingSpecifiedImports "setScore"
                        { fieldName = "score" }
                    |> Expect.equal
                        """setScore : score -> { record | score : score } -> { record | score : score }
setScore score_ record =
    { record | score = score_ }"""
            )
        , test update.description
            (\() ->
                update.elm
                    |> Generator.printUsingSpecifiedImports "updateScore"
                        { fieldName = "score" }
                    |> Expect.equal
                        """updateScore : (score -> score) -> { record | score : score } -> { record | score : score }
updateScore f record =
    { record | score = f record.score }"""
            )
        ]
