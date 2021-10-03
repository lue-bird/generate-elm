module Elm.Generator.ElmSvg exposing (Generator, generator)

{-| Generate [`elm/svg`](https://package.elm-lang.org/packages/elm/svg/latest/).

@docs Generator, generator

-}

import Elm.CodeGen as CodeGen
import Elm.Generator as Generator exposing (ReferenceLookup)
import Elm.Syntax.Expression as Expression
import SvgParser exposing (SvgNode(..))


{-| A [`Generator`](Elm-Generator#Generator) that turns a svg string argument into [`elm/svg`](https://package.elm-lang.org/packages/elm/svg/latest/).
See [`generator`](#generator).
-}
type alias Generator =
    Generator.ExpressionGenerator
        SvgNode
        Generator.Expression


{-| Generate [`elm/svg`](https://package.elm-lang.org/packages/elm/svg/latest/) from a string argument.


## configuration

    import Elm.Generator.ElmSvg as Svg
    import Review.Generate
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ Review.Generate.replaceStub "elmSvgToGenerate"
            Svg.generator
            |> Review.Generate.rule
        ]


## situation where svg is generated

    icon =
        elmSvgToGenerate
            """<svg height="210" width="500">
                <polygon points="200,10 250,190 160,210" style="fill:lime;stroke:purple;stroke-width:1" />
                Sorry, your browser does not support inline SVG.
            </svg>"""

-}
generator : Generator
generator =
    Generator.forExpression "elm/svg"
        (Generator.argumentsChecker
            (\arguments ->
                case arguments of
                    [ Expression.Literal htmlString ] ->
                        htmlString |> SvgParser.parseToNode

                    _ ->
                        Err "expected 1 string argument"
            )
        )
        (\{ fq } svgNode ->
            Generator.expression
                (svgToElm { fq = fq } svgNode)
                |> Generator.withImports
                    [ Generator.importModule ( "Svg", [] )
                    , Generator.importModule ( "Svg", [ "Attribute" ] )
                    ]
        )


{-| Generate [`elm/svg`](https://package.elm-lang.org/packages/elm/svg/latest/) code from a string.
-}
svgToElm : { fq : ReferenceLookup } -> SvgNode -> CodeGen.Expression
svgToElm { fq } svg =
    let
        svgAttributeToElm : ( String, String ) -> CodeGen.Expression
        svgAttributeToElm ( name, value ) =
            fq.construct
                ( "Svg", [ "Attribute" ] )
                (case name of
                    "in" ->
                        "in_"

                    "type" ->
                        "type_"

                    _ ->
                        name
                )
                [ CodeGen.string value ]
    in
    case svg of
        SvgElement { name, attributes, children } ->
            fq.construct ( "Svg", [] )
                (case name of
                    "text" ->
                        "text_"

                    _ ->
                        name
                )
                [ attributes
                    |> List.map svgAttributeToElm
                    |> CodeGen.list
                , children
                    |> List.map (svgToElm { fq = fq })
                    |> CodeGen.list
                ]

        SvgText text ->
            fq.construct ( "Svg", [] )
                "text"
                [ CodeGen.string text ]

        SvgComment comment ->
            fq.construct ( "Svg", [] )
                "text"
                [ CodeGen.string ""

                -- ↓ sorry :(
                , CodeGen.val ("-- " ++ comment)
                ]
