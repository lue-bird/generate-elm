module Elm.Generator.ElmSvg.Test exposing (tests)

import Elm.Generator.ElmSvg as Svg
import Review.Generate exposing (rule)
import Review.Generate.Internal exposing (errorInArguments, expressionGenerationRequestedError)
import Review.Generate.Test exposing (error)
import Review.Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Elm.Generator.ElmSvg"
        [ test "report"
            (\() ->
                """module A exposing (..)


someIcon =
    todoElmSvg \"\"\"<svg height="150" width="500">
  <ellipse cx="240" cy="100" rx="220" ry="30" style="fill:purple" />
  <ellipse cx="220" cy="70" rx="190" ry="20" style="fill:lime" />
  <ellipse cx="210" cy="45" rx="170" ry="15" style="fill:yellow" />
  Sorry, your browser does not support inline SVG.</svg>\"\"\"

                """
                    |> Review.Test.run
                        (Review.Generate.replaceStub "todoElmSvg"
                            Svg.generator
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (expressionGenerationRequestedError
                                { description = Svg.generator.description }
                            )
                            { under = """todoElmSvg \"\"\"<svg height="150" width="500">
  <ellipse cx="240" cy="100" rx="220" ry="30" style="fill:purple" />
  <ellipse cx="220" cy="70" rx="190" ry="20" style="fill:lime" />
  <ellipse cx="210" cy="45" rx="170" ry="15" style="fill:yellow" />
  Sorry, your browser does not support inline SVG.</svg>\"\"\"""" }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Svg
import Svg.Attribute

someIcon =
    Svg.svg
        [ Svg.Attribute.height "150", Svg.Attribute.width "500" ]
        [ Svg.ellipse
            [ Svg.Attribute.cx "240"
            , Svg.Attribute.cy "100"
            , Svg.Attribute.rx "220"
            , Svg.Attribute.ry "30"
            , Svg.Attribute.style "fill:purple"
            ]
            []
        , Svg.ellipse
            [ Svg.Attribute.cx "220"
            , Svg.Attribute.cy "70"
            , Svg.Attribute.rx "190"
            , Svg.Attribute.ry "20"
            , Svg.Attribute.style "fill:lime"
            ]
            []
        , Svg.ellipse
            [ Svg.Attribute.cx "210"
            , Svg.Attribute.cy "45"
            , Svg.Attribute.rx "170"
            , Svg.Attribute.ry "15"
            , Svg.Attribute.style "fill:yellow"
            ]
            []
        , Svg.text "Sorry, your browser does not support inline SVG."
        ]

                """
                        ]
            )
        , test "error while trying to generate"
            (\() ->
                """module A exposing (..)

someIcon =
    todoElmSvg 3

                """
                    |> Review.Test.run
                        (Review.Generate.replaceStub "todoElmSvg"
                            Svg.generator
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (errorInArguments
                                { description = Svg.generator.description
                                , errorMessage = "expected 1 string argument"
                                }
                            )
                            { under = """todoElmSvg 3""" }
                        ]
            )
        ]
