module Elm.Coder.Generator.Internal exposing (decoderNameParser, encoderNameParser)

import Elm.Code exposing (lowerNameParserUntil, upperNameParserUntil)
import Parser exposing ((|.), Parser)


encoderNameParser : Parser String
encoderNameParser =
    lowerNameParserUntil
        (Parser.symbol "Encoder" |. Parser.end)
        |> Parser.map String.toUpper


decoderNameParser : Parser String
decoderNameParser =
    lowerNameParserUntil
        (Parser.symbol "Decoder" |. Parser.end)
        |> Parser.map String.toUpper
