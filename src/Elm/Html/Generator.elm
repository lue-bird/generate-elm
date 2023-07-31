module Elm.Html.Generator exposing (Generator, generator)

{-| Generate [`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/).
making use of [hecrj's html-parser](https://package.elm-lang.org/packages/hecrj/html-parser/latest/).

This has some limitations:

  - `<script>` tags are not fully supported.
  - svg is not supported.

@docs Generator, generator

-}

import Elm.Code as Code exposing (Code, Expression, ExpressionAny, Origin, Top, call, from, import_, local, use, val)
import Elm.Code.Generator as Generator
import Html.Parser
import Parser
import ResultME


{-| A [`Generator`](Elm-Generator#Generator) that turns a html string argument into [`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/).
See [`generator`](#generator).
-}
type alias Generator =
    Generator.ExpressionGenerator (List Html.Parser.Node)


{-| Generate [`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/) from a string argument.


## configuration

    import Elm.Generator.Html as Html
    import Review.Generate
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ Review.Generate.replaceStub "elmHtmlToGenerate"
            Html.generator
            |> Review.Generate.rule
        ]


## example

    module HelloWord exposing (helloWorld)

    helloWorld =
        elmHtmlToGenerate
            """<div><p>Hello, world!</p></div>"""

would be "fixed" as

    module HelloWord exposing (helloWorld)

    import Html
    import Html.Attributes

    helloWord =
        Html.div [] [ Html.p [] [ Html.text "Hello, world!" ] ]

-}
generator : Generator
generator =
    Generator.forExpression "elm/html"
        (Generator.argumentsChecker
            (\arguments ->
                case arguments of
                    [ Code.Specific (Code.AppendableSpecificExpression (Code.Specific (Code.Stringy htmlString) _)) _ ] ->
                        case htmlString |> Html.Parser.run of
                            Ok ok ->
                                Ok ok

                            Err deadEnds ->
                                ResultME.error
                                    ("parsing error: " ++ (deadEnds |> Parser.deadEndsToString))

                    _ ->
                        ResultME.error
                            "expected 1 string argument"
            )
        )
        (\htmlNodeList ->
            (case htmlNodeList of
                [] ->
                    htmlText
                        |> call ( Code.string "", [] )
                        |> use

                [ htmlNode ] ->
                    htmlToElm htmlNode

                _ ->
                    Code.list
                        (htmlNodeList |> List.map (use << htmlToElm))
                        |> use
            )
                |> Code.withImports
                    [ import_ "Html"
                    , import_ "Html.Attributes"
                    ]
        )


{-| Generate [`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/) code from a string.
-}
htmlToElm htmlNode =
    let
        htmlAttributeToElm ( name, value ) =
            val
                (from "Html.Attributes"
                    (case name of
                        "type" ->
                            "type_"

                        _ ->
                            name
                    )
                )
                |> call ( Code.string value, [] )
    in
    case htmlNode of
        Html.Parser.Element name attributes children ->
            val
                (from "Html"
                    (case name of
                        "main" ->
                            "main_"

                        _ ->
                            name
                    )
                )
                |> call
                    ( Code.list
                        (attributes |> List.map (use << htmlAttributeToElm))
                    , [ Code.list
                            (children |> List.map htmlToElm)
                      ]
                    )
                |> use

        Html.Parser.Text text ->
            htmlText
                |> call ( Code.string text, [] )
                |> use

        Html.Parser.Comment comment ->
            htmlText
                |> call ( Code.string "", [] )
                -- ↓ sorry :(
                |> call ( val (local ("{- " ++ comment ++ " -}")), [] )
                |> use


htmlText : GeneralReference { origin : Origin } kind_
htmlText =
    val (from "Html" "text")
