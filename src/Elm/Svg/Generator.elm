module Elm.Svg.Generator exposing (Generator, generator)

{-| Generate [`elm/svg`](https://package.elm-lang.org/packages/elm/svg/latest/)
making use of [Garados007's elm-svg-parser](https://package.elm-lang.org/packages/Garados007/elm-svg-parser/latest).

@docs Generator, generator

-}

import Elm.Code as Code exposing (Code, Expression, ExpressionAny, GeneralReference, Origin, Top, call, from, import_, local, string, use, val)
import Elm.Code.Generator as Generator
import ResultME
import SvgParser exposing (SvgNode(..))


{-| A [`Generator`](Elm-Generator#Generator) that turns a svg string argument into [`elm/svg`](https://package.elm-lang.org/packages/elm/svg/latest/).
See [`generator`](#generator).
-}
type alias Generator =
    Generator.ExpressionGenerator SvgNode


{-| Generate [`elm/svg`](https://package.elm-lang.org/packages/elm/svg/latest/) from a string argument.


## configuration

    import Elm.Generator.Svg as Svg
    import Review.Generate
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ Review.Generate.replaceStub "elmSvgToGenerate"
            Svg.generator
            |> Review.Generate.rule
        ]


## example

    module Svgs exposing (polygon)

    polygon =
        elmSvgToGenerate
            """<svg height="210" width="500">
                <polygon points="200,10 250,190 160,210" style="fill:lime;stroke:purple;stroke-width:1" />
                Sorry, your browser does not support inline SVG.</svg>"""

will be "fixed" as

    module Svgs exposing (polygon)

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

-}
generator : Generator
generator =
    Generator.forExpression "elm/svg"
        (Generator.argumentsChecker
            (\arguments ->
                case arguments of
                    [ Code.Specific (Code.AppendableSpecificExpression (Code.Specific (Code.Stringy svgString) _)) _ ] ->
                        svgString
                            |> SvgParser.parseToNode
                            |> ResultME.fromResult
                            |> ResultME.mapError
                                (\deadEnds ->
                                    "parsing error: " ++ deadEnds
                                )

                    _ ->
                        ResultME.error "expected 1 string argument"
            )
        )
        (\svgNode ->
            svgNodeToElm svgNode
                |> Code.withImports
                    [ import_ svg
                    , import_ svgAttributes
                    ]
        )


{-| Generate [`elm/svg`](https://package.elm-lang.org/packages/elm/svg/latest/) code from a string.
-}
svgNodeToElm svgNode =
    let
        svgAttributeToElm ( name, value ) =
            val
                (from svgAttributes
                    (case name of
                        "in" ->
                            "in_"

                        "type" ->
                            "type_"

                        _ ->
                            name
                    )
                )
                |> call ( Code.string value, [] )
    in
    case svgNode of
        SvgElement { name, attributes, children } ->
            val
                (from svg
                    (case name of
                        "text" ->
                            "text_"

                        _ ->
                            name
                    )
                )
                |> call
                    ( attributes
                        |> List.map (use << svgAttributeToElm)
                        |> Code.list
                    , [ children
                            |> List.map svgNodeToElm
                            |> Code.list
                      ]
                    )
                |> use

        SvgText text ->
            svgText |> call ( Code.string text, [] ) |> use

        SvgComment comment ->
            svgText
                |> call ( string "", [] )
                -- ↓ sorry :(
                |> call ( val (local ("{- " ++ comment ++ " -}")), [] )
                |> use


svgText : GeneralReference { origin : Origin } kind_
svgText =
    val (from svg "text")


svg =
    "Svg"


svgAttributes =
    "Svg.Attributes"
