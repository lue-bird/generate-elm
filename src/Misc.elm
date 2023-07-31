module Misc exposing (applyIfJust, capitalize, dictKeys, firstJust, mapAccumMultipleL, mapAll3, maybeNonemptyToList, prettyWidth, printPretty, toNonempty, triple, updateFirstChar)

{-| Some helpers that can be used from all modules.
-}

import Dict exposing (Dict)
import List.NonEmpty
import Pretty exposing (pretty)
import Set exposing (Set)


{-| Change the first character in the string.

    decapitalize =
        updateFirstChar Char.toLower

-}
updateFirstChar : (Char -> Char) -> String -> String
updateFirstChar f string =
    case String.uncons string of
        Just ( firstChar, after ) ->
            String.cons (f firstChar) after

        Nothing ->
            ""


{-| Convert the first character in the string to lowercase.

    "hello" |> capitalize
    --> "Hello"

-}
capitalize : String -> String
capitalize string =
    string
        |> updateFirstChar Char.toUpper



--


firstJust : List (Maybe a) -> Maybe a
firstJust maybes =
    case maybes of
        (Just value) :: _ ->
            Just value

        [] ->
            Nothing

        Nothing :: rest ->
            rest |> firstJust


maybeNonemptyToList : Maybe ( a, List a ) -> List a
maybeNonemptyToList maybeNonemptyList =
    case maybeNonemptyList of
        Just nonEmptyList ->
            nonEmptyList |> List.NonEmpty.toList

        Nothing ->
            []


toNonempty : List a -> Maybe ( a, List a )
toNonempty list =
    case list of
        head :: tail ->
            Just ( head, tail )

        [] ->
            Nothing


{-| A variant of map-accumulate that can return multiple elements alongside the new accumulated result per step.
-}
mapAccumMultipleL : (a -> acc -> ( List b, acc )) -> acc -> List a -> ( List b, acc )
mapAccumMultipleL accumulateStep initialAccumulator =
    List.foldl
        (\typeAnn ( list, acc ) ->
            let
                ( toAdd, newAcc ) =
                    accumulateStep typeAnn acc
            in
            ( list ++ toAdd, newAcc )
        )
        ( [], initialAccumulator )


{-| Apply the passed function only onto a value that's Just.

    [ 2, 3 ]
        |> applyIfJust (Just 1) (::)
    --> [ 1, 2, 3 ]

    [ 2, 3 ]
        |> applyIfJust Nothing (::)
    --> [ 2, 3 ]

-}
applyIfJust : Maybe a -> (a -> value -> value) -> value -> value
applyIfJust maybe ifExists =
    case maybe of
        Just existing ->
            ifExists existing

        Nothing ->
            identity



--


dictKeys : Dict comparableKey value_ -> Set comparableKey
dictKeys dict =
    dict |> Dict.keys |> Set.fromList



--


triple : first -> second -> third -> ( first, second, third )
triple first second third =
    ( first, second, third )


mapAll3 :
    (first -> mappedFirst)
    -> (second -> mappedSecond)
    -> (third -> mappedThird)
    -> ( first, second, third )
    -> ( mappedFirst, mappedSecond, mappedThird )
mapAll3 mapFirst mapSecond mapThird =
    \( a0, a1, a2 ) -> ( mapFirst a0, mapSecond a1, mapThird a2 )



--


prettyWidth : number_
prettyWidth =
    92


printPretty : Pretty.Doc -> String
printPretty =
    pretty prettyWidth
