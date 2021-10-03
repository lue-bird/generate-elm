module Util exposing (applyIfJust, firstJust, fromNonempty, mapAccumMultipleL, toNonempty, updateFirstChar)

{-| Some utilities that can be used from all modules.
-}


{-| Change the first character in the string.

    capitalize =
        updateFirstChar Char.toUpper

-}
updateFirstChar : (Char -> Char) -> String -> String
updateFirstChar f string =
    case String.uncons string of
        Just ( firstChar, after ) ->
            String.cons (f firstChar) after

        Nothing ->
            ""


firstJust : List (Maybe a) -> Maybe a
firstJust maybes =
    case maybes of
        (Just value) :: _ ->
            Just value

        [] ->
            Nothing

        Nothing :: rest ->
            rest |> firstJust


fromNonempty : ( a, List a ) -> List a
fromNonempty nonemptyList =
    let
        ( head, tail ) =
            nonemptyList
    in
    head :: tail


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
