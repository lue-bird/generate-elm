module Elm.Generator.Html.Test exposing (tests)

import Elm.Html.Generator as Html
import Review.Generate exposing (rule)
import Review.Generate.Internal exposing (errorInArguments, expressionGenerationRequestedError)
import Review.Generate.Test exposing (error)
import Review.Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Elm.Generator.Html"
        [ test "report"
            (\() ->
                """module A exposing (..)


helloWord =
    todoHtml \"\"\"<div><p>Hello, world!</p></div>\"\"\"

"""
                    |> Review.Test.run
                        (Review.Generate.replaceStub "todoHtml"
                            Html.generator
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (expressionGenerationRequestedError
                                { description = Html.generator.description }
                            )
                            { under = """todoHtml \"\"\"<div><p>Hello, world!</p></div>\"\"\"""" }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Html
import Html.Attributes

helloWord =
    Html.div [] [ Html.p [] [ Html.text "Hello, world!" ] ]

"""
                        ]
            )
        , test "error while trying to generate"
            (\() ->
                """module A exposing (..)

invalid =
    todoHtml 3

                """
                    |> Review.Test.run
                        (Review.Generate.replaceStub "todoHtml"
                            Html.generator
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (errorInArguments
                                { description = Html.generator.description
                                , errors = [ "expected 1 string argument" ]
                                }
                            )
                            { under = """todoHtml 3""" }
                        ]
            )
        ]
