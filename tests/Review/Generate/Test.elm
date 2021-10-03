module Review.Generate.Test exposing (all, error)

import Elm.Generator as Generator
import Elm.Generator.RecordFieldHelper exposing (accessors, update)
import Expect
import Review.Generate exposing (belowAllDeclarations, belowMarker, inModule, inSameModule, rule)
import Review.Generate.Internal exposing (duplicateMarkerError, missingDeclarationError, missingImportFromGeneratingModule, missingMarkerError, missingModuleError)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Generate"
        [ generateInDifferentModule
        , generateInSameModule
        ]


generateInSameModule : Test
generateInSameModule =
    describe "generate in same module"
        [ generateInSameModuleFail ]


generateInSameModuleFail : Test
generateInSameModuleFail =
    describe "report"
        [ test "missing declaration; add after all declarations"
            (\() ->
                """module Player exposing (scoreAPoint)

scoreAPoint =
    updateScore ((+) 1)


z =
    z
"""
                    |> Review.Test.run
                        (inSameModule update
                            |> belowAllDeclarations
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "updateScore"
                                { description = update.description }
                            )
                            { under = "updateScore" }
                            |> Review.Test.whenFixed
                                """module Player exposing (scoreAPoint)

scoreAPoint =
    updateScore ((+) 1)


z =
    z


updateScore : (score -> score) -> { record | score : score } -> { record | score : score }
updateScore f record =
    { record | score = f record.score }
"""
                        ]
            )
        , test "missing declaration; add after declaration it was referenced in"
            (\() ->
                """module Player exposing (scoreAPoint)

scoreAPoint =
    updateScore ((+) 1)


z =
    z
"""
                    |> Review.Test.run
                        (inSameModule update
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "updateScore"
                                { description = update.description }
                            )
                            { under = "updateScore" }
                            |> Review.Test.whenFixed
                                """module Player exposing (scoreAPoint)

scoreAPoint =
    updateScore ((+) 1)


updateScore : (score -> score) -> { record | score : score } -> { record | score : score }
updateScore f record =
    { record | score = f record.score }


z =
    z
"""
                        ]
            )
        ]


generateInDifferentModule : Test
generateInDifferentModule =
    describe "generate in different module"
        [ generateInDifferentModuleFail
        , generateInDifferentModuleSuccess
        , declarations
        ]


generateInDifferentModuleFail : Test
generateInDifferentModuleFail =
    describe "report"
        [ generateInDifferentModuleMarker
        , test "missing module to generate in"
            (\() ->
                [ """module Fields exposing (setName)

setName =
    setName
"""
                , """module Player exposing (scoreAPoint)

import Accessors.Library.Fields as Field
import Fields

scoreAPoint =
    Accessors.over Field.score ((+) 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (inModule
                            ( "Accessors", [ "Library", "Fields" ] )
                            accessors
                            |> rule
                        )
                    |> Review.Test.expectGlobalErrors
                        [ missingModuleError
                            ( "Accessors", [ "Library", "Fields" ] )
                            { oneDeclarationKind = accessors.description }
                        ]
            )
        , describe "missing declaration"
            [ test "used qualified; add after all declarations"
                (\() ->
                    [ """module Accessors.Library.Fields exposing (a, z)

a =
    a


z =
    z
"""
                    , """module Player exposing (scoreAPoint)

import Accessors.Library.Fields as Field

scoreAPoint =
    Accessors.over Field.score ((+) 1)
"""
                    ]
                        |> Review.Test.runOnModules
                            (inModule
                                ( "Accessors", [ "Library", "Fields" ] )
                                accessors
                                |> rule
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Accessors.Library.Fields"
                              , [ error
                                    (missingDeclarationError "score"
                                        { description = accessors.description }
                                    )
                                    { under = "Accessors.Library.Fields" }
                                    |> Review.Test.whenFixed
                                        """module Accessors.Library.Fields exposing (a, z, score)

import Accessors exposing (Relation, makeOneToOne)
a =
    a


z =
    z


score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })
"""
                                ]
                              )
                            ]
                )
            ]
        , test "used unqualified; add after all declarations"
            (\() ->
                [ """module Fields exposing (a, z)

a =
    a


z =
    z
"""
                , """module Player exposing (scoreAPoint)

scoreAPoint =
    updateScore ((+) 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (inModule
                            ( "Fields", [] )
                            update
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Fields"
                          , [ error
                                (missingDeclarationError "updateScore"
                                    { description = update.description }
                                )
                                { under = "Fields" }
                                |> Review.Test.whenFixed
                                    """module Fields exposing (a, z, updateScore)

a =
    a


z =
    z


updateScore : (score -> score) -> { record | score : score } -> { record | score : score }
updateScore f record =
    { record | score = f record.score }
"""
                            ]
                          )
                        , ( "Player"
                          , [ error
                                (missingImportFromGeneratingModule "updateScore"
                                    { description = update.description }
                                )
                                { under = "updateScore" }
                                |> Review.Test.whenFixed
                                    """module Player exposing (scoreAPoint)

import Fields exposing (updateScore)
scoreAPoint =
    updateScore ((+) 1)
"""
                            ]
                          )
                        ]
            )
        ]


generateInDifferentModuleMarker : Test
generateInDifferentModuleMarker =
    describe "marker"
        [ test "missing"
            (\() ->
                [ """module Fields exposing (setName)

setName =
    setName
"""
                , """module Player exposing (scoreAPoint)

import Fields

scoreAPoint =
    Accessors.over Fields.score ((+) 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (inModule
                            ( "Fields", [] )
                            accessors
                            |> belowMarker "accessors"
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Fields"
                          , [ error
                                (missingMarkerError "accessors"
                                    { for = accessors.description }
                                )
                                { under = "Fields" }
                                |> Review.Test.whenFixed
                                    """module Fields exposing (setName)

setName =
    setName


-- accessors

"""
                            ]
                          )
                        ]
            )
        , test "duplicate"
            (\() ->
                [ """module Fields exposing (setName)

-- accessors

setName =
    setName

-- accessors

"""
                , """module Player exposing (scoreAPoint)

import Fields

scoreAPoint =
    Accessors.over Fields.score ((+) 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (inModule
                            ( "Fields", [] )
                            accessors
                            |> belowMarker "accessors"
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Fields"
                          , [ error
                                (duplicateMarkerError "accessors"
                                    { for = accessors.description }
                                )
                                { under = "-- accessors" }
                                |> Review.Test.atExactly
                                    { start = { row = 3, column = 1 }, end = { row = 3, column = 13 } }
                            , error
                                (duplicateMarkerError "accessors"
                                    { for = accessors.description }
                                )
                                { under = "-- accessors" }
                                |> Review.Test.atExactly
                                    { start = { row = 8, column = 1 }, end = { row = 8, column = 13 } }
                            ]
                          )
                        ]
            )
        , test "existing"
            (\() ->
                [ """module Accessors.Library.Fields exposing (name)

-- accessors


name =
    name
"""
                , """module Player exposing (scoreAPoint)

import Accessors.Library.Fields as Field

scoreAPoint =
    Accessors.over Field.score ((+) 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (inModule
                            ( "Accessors", [ "Library", "Fields" ] )
                            accessors
                            |> belowMarker "accessors"
                            |> rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Accessors.Library.Fields"
                          , [ error
                                (missingDeclarationError "score"
                                    { description = accessors.description }
                                )
                                { under = "Accessors.Library.Fields" }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (name, score)

import Accessors exposing (Relation, makeOneToOne)
-- accessors


score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })


name =
    name
"""
                            ]
                          )
                        ]
            )
        ]


generateInDifferentModuleSuccess : Test
generateInDifferentModuleSuccess =
    test "doesn't report existing field lens"
        (\() ->
            [ """module Accessors.Library.Fields exposing (score)

score =
    score
"""
            , """module Player exposing (scoreAPoint)

import Accessors.Library.Fields as Field

scoreAPoint =
    Accessors.over Field.score ((+) 1)
       """
            ]
                |> Review.Test.runOnModules
                    (inModule
                        ( "Accessors", [ "Library", "Fields" ] )
                        accessors
                        |> rule
                    )
                |> Review.Test.expectNoErrors
        )


declarations : Test
declarations =
    describe "kinds of declarations"
        [ test ("function declaration ( " ++ accessors.description ++ " as an example")
            (\() ->
                accessors.elm
                    |> Generator.printUsingSpecifiedImports "score"
                        { fieldName = "score" }
                    |> Expect.equal
                        """score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })"""
            )
        ]



--


error :
    { message : String, details : List String }
    -> { under : String }
    -> Review.Test.ExpectedError
error errorInfo { under } =
    Review.Test.error
        { message = errorInfo.message
        , details = errorInfo.details
        , under = under
        }
