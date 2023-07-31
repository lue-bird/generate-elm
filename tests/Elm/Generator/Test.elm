module Elm.Generator.Test exposing (tests)

import AssocSet
import Dict
import Elm.Code as Code exposing (Operator(..), ParserOperator(..), ignore, import_, withExposing)
import Elm.CodeGen exposing (exposeExplicit, typeVar)
import Elm.Pretty exposing (prettyImports)
import Expect
import Misc exposing (prettyWidth)
import Pretty
import SyntaxExtra exposing (printImports)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "exposing"
        [ test "infix operators"
            (\() ->
                import_ "Parser"
                    |> withExposing
                        (\expose ->
                            { expose
                                | infixOperators =
                                    [ Ignore, Keep ]
                                        |> List.map ParserOperator
                                        |> AssocSet.fromList
                            }
                        )
                    |> List.singleton
                    |> Code.importsFromList
                    |> printImports
                    |> Expect.equal """import Parser exposing ((|.), (|=))"""
            )
        ]
