module Elm.Generator.Svg.Test exposing (tests)

import Elm.Svg.Generator as Svg
import Review.Generate exposing (rule)
import Review.Generate.Internal exposing (errorInArguments, expressionGenerationRequestedError)
import Review.Generate.Test exposing (error)
import Review.Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Elm.Generator.Svg"
        [ test "report"
            (\() ->
                """module A exposing (..)


someIcon =
    elmSvgToGenerate \"\"\"<svg height="150" width="500">
  <ellipse cx="240" cy="100" rx="220" ry="30" style="fill:purple" />
  <ellipse cx="220" cy="70" rx="190" ry="20" style="fill:lime" />
  <ellipse cx="210" cy="45" rx="170" ry="15" style="fill:yellow" />
  Sorry, your browser does not support inline SVG.</svg>\"\"\"

"""
                    |> Review.Test.run
                        (Review.Generate.replaceStub "elmSvgToGenerate"
                            Svg.generator
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (expressionGenerationRequestedError
                                { description = Svg.generator.description }
                            )
                            { under = """elmSvgToGenerate \"\"\"<svg height="150" width="500">
  <ellipse cx="240" cy="100" rx="220" ry="30" style="fill:purple" />
  <ellipse cx="220" cy="70" rx="190" ry="20" style="fill:lime" />
  <ellipse cx="210" cy="45" rx="170" ry="15" style="fill:yellow" />
  Sorry, your browser does not support inline SVG.</svg>\"\"\"""" }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Svg
import Svg.Attributes

someIcon =
    Svg.svg
        [ Svg.Attributes.height "150", Svg.Attributes.width "500" ]
        [ Svg.ellipse
            [ Svg.Attributes.cx "240"
            , Svg.Attributes.cy "100"
            , Svg.Attributes.rx "220"
            , Svg.Attributes.ry "30"
            , Svg.Attributes.style "fill:purple"
            ]
            []
        , Svg.ellipse
            [ Svg.Attributes.cx "220"
            , Svg.Attributes.cy "70"
            , Svg.Attributes.rx "190"
            , Svg.Attributes.ry "20"
            , Svg.Attributes.style "fill:lime"
            ]
            []
        , Svg.ellipse
            [ Svg.Attributes.cx "210"
            , Svg.Attributes.cy "45"
            , Svg.Attributes.rx "170"
            , Svg.Attributes.ry "15"
            , Svg.Attributes.style "fill:yellow"
            ]
            []
        , Svg.text "Sorry, your browser does not support inline SVG."
        ]

"""
                        ]
            )
        , test "another example to generate"
            (\() ->
                """module A exposing (..)


polygon =
    elmSvgToGenerate
        \"\"\"<svg height="210" width="500">
            <polygon points="200,10 250,190 160,210" style="fill:lime;stroke:purple;stroke-width:1" />
            Sorry, your browser does not support inline SVG.</svg>\"\"\"
"""
                    |> Review.Test.run
                        (Review.Generate.replaceStub "elmSvgToGenerate"
                            Svg.generator
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (expressionGenerationRequestedError
                                { description = Svg.generator.description }
                            )
                            { under = """elmSvgToGenerate
        \"\"\"<svg height="210" width="500">
            <polygon points="200,10 250,190 160,210" style="fill:lime;stroke:purple;stroke-width:1" />
            Sorry, your browser does not support inline SVG.</svg>\"\"\"""" }
                            |> Review.Test.whenFixed
                                """module A exposing (..)

import Svg
import Svg.Attributes

polygon =
    Svg.svg
        [ Svg.Attributes.height "210", Svg.Attributes.width "500" ]
        [ Svg.polygon
            [ Svg.Attributes.points "200,10 250,190 160,210"
            , Svg.Attributes.style "fill:lime;stroke:purple;stroke-width:1"
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
    todoSvg 3

"""
                    |> Review.Test.run
                        (Review.Generate.replaceStub "todoSvg"
                            Svg.generator
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (errorInArguments
                                { description = Svg.generator.description
                                , errors = [ "expected 1 string argument" ]
                                }
                            )
                            { under = """todoSvg 3""" }
                        ]
            )
        ]
