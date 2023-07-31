module ResultME.Extra exposing (combineStack, combineStackFilled, fromMaybe, orElse)

import Fillable exposing (Empty(..))
import List.Nonempty
import ResultME exposing (ResultME)
import Stack exposing (StackFilled, topAndBelow)


orElse : ResultME err ok -> ResultME err ok -> ResultME err ok
orElse nextTry =
    \result ->
        case result of
            Ok ok ->
                Ok ok

            Err errs ->
                case nextTry of
                    Ok ok ->
                        Ok ok

                    Err nextErrors ->
                        List.Nonempty.append errs nextErrors
                            |> Err


combineStack :
    Empty emptyOrNot (StackFilled (ResultME err ok))
    -> ResultME err (Empty emptyOrNot (StackFilled ok))
combineStack results =
    case results of
        Filled nonEmpty ->
            (let
                ( head, tail ) =
                    nonEmpty
             in
             List.Nonempty.Nonempty head tail
            )
                |> ResultME.combineNonempty
                |> ResultME.map
                    (\(List.Nonempty.Nonempty head tail) ->
                        topAndBelow head tail
                    )

        Empty canBe ->
            Ok (Empty canBe)


combineStackFilled :
    StackFilled (ResultME err ok)
    -> ResultME err (StackFilled ok)
combineStackFilled results =
    (let
        ( head, tail ) =
            results
     in
     List.Nonempty.Nonempty head tail
    )
        |> ResultME.combineNonempty
        |> ResultME.map
            (\(List.Nonempty.Nonempty head tail) ->
                ( head, tail )
            )


{-| Turns a `Maybe` into a `ResultME`, returning error(s) when it's `Nothing`.

    parseInt : String -> Result String Int
    parseInt string =
        ResultME.fromMaybe
            (ResultME.error ("error parsing string: " ++ string))
            (String.toInt string)

-}
fromMaybe : ResultME err Never -> Maybe ok -> ResultME err ok
fromMaybe errorsWhenNothing maybe =
    case maybe of
        Nothing ->
            errorsWhenNothing |> ResultME.map never

        Just val ->
            Ok val
