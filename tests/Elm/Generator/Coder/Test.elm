module Elm.Generator.Coder.Test exposing (tests)

import Elm.Coder.Generator exposing (jsonEncoder, jsonPipelineDecoder)
import Elm.Coder.Generator.Internal exposing (decoderNameParser, encoderNameParser)
import Expect
import Parser
import Review.Generate as Generate
import Review.Generate.Internal exposing (missingDeclarationError)
import Review.Generate.Test exposing (error)
import Review.Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Elm.Generator.Coder"
        [ encoder, decoder ]


encoder : Test
encoder =
    describe "encoder"
        [ test "nameParser works"
            (\() ->
                "exampleEncoder"
                    |> Parser.run encoderNameParser
                    |> Expect.equal (Ok "Example")
            )
        , test "value encoder"
            (\() ->
                """module A exposing (..)
import Json.Encode

type alias Example =
    Json.Encode.Value

encode =
    Json.Encode.encode exampleEncoder
"""
                    |> Review.Test.run
                        (Generate.inSameModule
                            jsonEncoder
                            |> Generate.rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "exampleEncoder"
                                { description = jsonEncoder.description }
                            )
                            { under = "exampleEncoder" }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode

type alias Example =
    Json.Encode.Value

encode =
    Json.Encode.encode exampleEncoder


exampleEncoder : Example -> Json.Encode.Value
exampleEncoder =
    identity
"""
                        ]
            )
        , test "record encoder"
            (\() ->
                """module A exposing (..)

type alias Point =
    { x : Float, y : Float }

a =
    pointEncoder
"""
                    |> Review.Test.run
                        (Generate.inSameModule jsonEncoder
                            |> Generate.belowDeclarations
                                (\typeOrAlias -> [ typeOrAlias.name ])
                            |> Generate.rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "pointEncoder"
                                { description = jsonEncoder.description }
                            )
                            { under = "pointEncoder" }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Encode
type alias Point =
    { x : Float, y : Float }


pointEncoder : Point -> Json.Encode.Value
pointEncoder record =
    Json.Encode.object
        [ ( "x", Json.Encode.float record.x ), ( "y", Json.Encode.float record.y ) ]

a =
    pointEncoder
"""
                        ]
            )
        , test "simple type enum encoder"
            (\() ->
                """module A exposing (..)

type Direction phantom =
    Left | Right

a =
    directionEncoder
"""
                    |> Review.Test.run
                        (Generate.inSameModule jsonEncoder
                            |> Generate.belowDeclarations
                                (\typeOrAlias -> [ typeOrAlias.name ])
                            |> Generate.rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "directionEncoder"
                                { description = jsonEncoder.description }
                            )
                            { under = "directionEncoder" }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Encode
type Direction phantom =
    Left | Right


directionEncoder : Direction phantom -> Json.Encode.Value
directionEncoder direction =
    case direction of
        Left ->
            Json.Encode.string "left"

        Right ->
            Json.Encode.string "right"

a =
    directionEncoder
"""
                        ]
            )
        , test "type encoder"
            (\() ->
                """module A exposing (..)


type Result err ok
    = Err err
    | Ok ok


a =
    resultEncoder
"""
                    |> Review.Test.run
                        (Generate.inSameModule jsonEncoder
                            |> Generate.belowDeclarations
                                (\typeOrAlias -> [ typeOrAlias.name ])
                            |> Generate.rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "resultEncoder"
                                { description = jsonEncoder.description }
                            )
                            { under = "resultEncoder" }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Encode

type Result err ok
    = Err err
    | Ok ok


resultEncoder :
    (err -> Json.Encode.Value)
    -> (ok -> Json.Encode.Value)
    -> Result err ok
    -> Json.Encode.Value
resultEncoder errEncoder okEncoder result =
    case result of
        Err a0 ->
            Json.Encode.object [ ( "tag", Json.Encode.string "err" ), ( "a0", errEncoder ) ]

        Ok a0 ->
            Json.Encode.object [ ( "tag", Json.Encode.string "ok" ), ( "a0", okEncoder ) ]


a =
    resultEncoder
"""
                        ]
            )
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "nameParser works"
            (\() ->
                "exampleDecoder"
                    |> Parser.run decoderNameParser
                    |> Expect.equal (Ok "Example")
            )
        , test "record encoder"
            (\() ->
                """module A exposing (..)

type alias Point =
    { x : Float, y : Float }

a =
    pointDecoder
"""
                    |> Review.Test.run
                        (Generate.inSameModule jsonPipelineDecoder
                            |> Generate.belowDeclarations
                                (\typeOrAlias -> [ typeOrAlias.name ])
                            |> Generate.rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "pointDecoder"
                                { description = jsonPipelineDecoder.description }
                            )
                            { under = "pointDecoder" }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Decode as JsonDecode
import Json.Decode.Pipeline as JsonDecode
type alias Point =
    { x : Float, y : Float }


pointDecoder : JsonDecode.Decoder Point
pointDecoder =
    JsonDecode.succeed (\\x y -> { x = x, y = y })
        |> JsonDecode.required "x" JsonDecode.float
        |> JsonDecode.required "y" JsonDecode.float

a =
    pointDecoder
"""
                        ]
            )
        , test "simple type enum decoder"
            (\() ->
                """module A exposing (..)

type Direction phantom =
    Left | Right

a =
    directionDecoder
"""
                    |> Review.Test.run
                        (Generate.inSameModule jsonPipelineDecoder
                            |> Generate.belowDeclarations
                                (\typeOrAlias -> [ typeOrAlias.name ])
                            |> Generate.rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "directionDecoder"
                                { description = jsonPipelineDecoder.description }
                            )
                            { under = "directionDecoder" }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Decode as JsonDecode
import Json.Decode.Pipeline as JsonDecode
type Direction phantom =
    Left | Right


directionDecoder : JsonDecode.Decoder (Direction phantom)
directionDecoder =
    JsonDecode.string
        |> JsonDecode.andThen
            (\\direction ->
                case direction of
                    "left" ->
                        JsonDecode.succeed Left

                    "right" ->
                        JsonDecode.succeed Right

                    unknownTag ->
                        JsonDecode.problem
                            ("Unknown tag \\""
                                ++ unknownTag
                                ++ "\\" doesn't match any Direction variant name"
                            )
            )

a =
    directionDecoder
"""
                        ]
            )
        , test "type decoder"
            (\() ->
                """module A exposing (..)


type Result err ok
    = Err err
    | Ok ok


a =
    resultDecoder
"""
                    |> Review.Test.run
                        (Generate.inSameModule jsonPipelineDecoder
                            |> Generate.belowDeclarations
                                (\typeOrAlias -> [ typeOrAlias.name ])
                            |> Generate.rule
                        )
                    |> Review.Test.expectErrors
                        [ error
                            (missingDeclarationError "resultDecoder"
                                { description = jsonPipelineDecoder.description }
                            )
                            { under = "resultDecoder" }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Decode as JsonDecode
import Json.Decode.Pipeline as JsonDecode

type Result err ok
    = Err err
    | Ok ok


resultDecoder :
    JsonDecode.Decoder err -> JsonDecode.Decoder ok -> JsonDecode.Decoder (Result err ok)
resultDecoder errDecoder okDecoder =
    JsonDecode.field "tag" JsonDecode.string
        |> JsonDecode.andThen
            (\\result ->
                case result of
                    "err" ->
                        JsonDecode.succeed Err |> JsonDecode.required "a0" errDecoder

                    "ok" ->
                        JsonDecode.succeed Ok |> JsonDecode.required "a0" okDecoder

                    unknownTag ->
                        JsonDecode.problem
                            ("Unknown tag \\""
                                ++ unknownTag
                                ++ "\\" doesn't match any Result variant name"
                            )
            )


a =
    resultDecoder
"""
                        ]
            )
        ]
