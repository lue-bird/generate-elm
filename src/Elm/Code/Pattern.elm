module Elm.Code.Pattern exposing
    ( Pattern, SpecificPattern, ListSpecificPattern
    , SpecificPatternAny, PatternAny
    , all_, var, as_
    , ListPattern
    , unCons, listPattern
    , intPattern
    , unitPattern, stringPattern, tuple2Pattern, tuple3Pattern, unionPattern
    , recordPattern, fieldVar
    )

{-| Pattern

@docs Pattern, SpecificPattern, ListSpecificPattern
@docs SpecificPatternAny, PatternAny


## general pattern

@docs all_, var, as_


## list specific pattern

@docs ListPattern
@docs unCons, listPattern


## other specific patterns

@docs intPattern
@docs unitPattern, stringPattern, tuple2Pattern, tuple3Pattern, unionPattern
@docs recordPattern, fieldVar

-}

import Elm.Code.Common exposing (Appendably, Base, Origin, Tuply, local)
import Elm.Code.Expression.Common exposing (named)
import Elm.Code.Pattern.Common exposing (GeneralPattern(..), Pattern)
import Elm.Code.Util exposing (SpecificOrGeneral(..), mapSpecific)
import Elm.Code.Util.Suppliable exposing (Suppliable, supplyWith, supplying)
import Generalizable exposing (Generalizable, generalWith, toGeneral)
import Misc exposing (mapAll3)



--


{-| A wildcard pattern.

    import Elm.Code exposing (all_, var, typePattern, only, next)

    typePattern (from "Elm.Syntax.Node" "Node")
        (only all_ |> next (var "expression"))

would generate

    -- case node of
        Node _ expression --->

See [`Origin`](#Origin).

-}
all_ : Suppliable (Pattern specific_) (a -> a)
all_ =
    All_
        |> General
        |> supplying identity


{-| `()` pattern.

    import Elm.Code exposing (unitPattern, lambda, val, local, only)

    \nextStep ->
        lambda (only (unitPattern ()))
            nextStep

would generate

    \() -> nextStep

See [`CombinedList`](#CombinedList).

-}
unitPattern : () -> Suppliable UnitPattern (a -> a)
unitPattern unit_ =
    unit_
        |> Specific
        |> generalWith Unity
        |> Specific
        |> supplying identity


{-| A pattern variable.

    import Elm.Code exposing (all_, var, typePattern, only, next)

    typePattern (from "Elm.Syntax.Node" "Node")
        (only all_ |> next (var "expression"))

would generate

    -- case node of
        Node _ expression --->

See [`CombinedList`](#CombinedList) and [`Origin`](#Origin).

-}
var :
    String
    ->
        Suppliable
            (Pattern specificPattern_)
            ((Expression specificExpression_ -> to) -> to)
var patternVariableName =
    patternVariableName
        |> Variable
        |> General
        |> supplying
            ((|>) (named (local patternVariableName)))


{-| An int pattern.

    import Elm.Code exposing (intPattern)

    intPattern 2

would generate

    -- case movesLeft of
        2 --->

[`inBase`](#inBase) to use a different [`Base`](#Base).

-}
intPattern :
    Int
    -> Base
    -> Suppliable IntPattern (a -> a)
intPattern intValue base =
    ( intValue, base )
        |> generalWith Inty
        |> Specific
        |> Specific
        |> supplying identity


{-| A string pattern.

    import Elm.Code exposing (stringPattern)

    stringPattern "ok"

would generate

    -- case tag of
        "ok" --->

-}
stringPattern :
    String
    -> Suppliable StringPattern (a -> a)
stringPattern string_ =
    string_
        |> generalWith
            (StringPattern >> Appendably)
        |> Specific
        |> Specific
        |> supplying identity


{-| An `as` pattern.

    import Elm.Code exposing (recordPattern, as_, var)

    recordPattern (only (var "name")) |> as_ (var "person")

would generate

    { name } as person

See [`Origin`](#Origin).

-}
as_ :
    String
    ->
        Suppliable
            specificPattern
            (patternFunction -> (Expression specificExpression_ -> to))
    ->
        Suppliable
            specificPattern
            (patternFunction -> to)
as_ variableName destructuringPattern =
    AsPattern destructuringPattern variableName
        |> Specific
        |> General
        |> supplying ((|>) (var variableName))


toPattern :
    Pattern specificPattern
    -> PatternAny
toPattern generalPattern =
    generalPattern
        |> mapSpecific top
            (\general ->
                case general of
                    All_ ->
                        All_

                    Variable varName ->
                        Variable varName

                    AsPattern pattern varName ->
                        AsPattern (pattern |> toPattern) varName
            )


{-| A `::` pattern.

    import Elm.Code exposing (unCons, var)

    var "head" |> unCons (var "rest")

would generate

    -- case list of
        head :: rest --->

-}
unCons :
    Suppliable tail (ListLiteralPattern elementSpecific) (b -> to)
    -> Suppliable head elementSpecific_ (a -> b)
    ->
        Suppliable
            ( Code head, Code tail )
            (ListLiteralPattern elementSpecific)
            (a -> to)
unCons tailPattern headPattern =
    ( headPattern, tailPattern )
        |> Specific
        << generalWith
            (\( h, t ) -> UnConsPattern h t)
        |> supplying
            (headRoot.code.supply
                >> tailRoot.code.supply
            )


{-| A `[ ... ]` pattern.

    import Elm.Code exposing (listPattern, var, only, next)

    listPattern
        (only (var "first") |> next (var "second"))

would generate

    -- case list of
        [ first, second ] --->

See [`CombinedList`](#CombinedList).

-}
listPattern :
    Suppliable
        supplyFunction
        (Empty possiblyOrNever_ (StackFilled (Pattern elementSpecific)))
    -> Suppliable supplyFunction (ListPattern elementSpecific)
listPattern elementPatterns =
    elementPatterns
        |> value
        |> Stack.toList
        |> ListElementsPattern
        |> generalWith ListPatterny
        |> Specific
        |> supplying elementPatterns.supply


{-| A `type` variant pattern.

    import Elm.Code exposing (all_, var, typePattern, only, next)

    typePattern (from "Elm.Syntax.Node" "Node")
        (only all_ |> next (var "expression"))

would generate

    -- case node of
        Node _ expression --->

See [`CombinedList`](#CombinedList) and [`Origin`](#Origin).

-}
unionPattern :
    Origin
    ->
        Supplying
            supply
            (Empty possiblyOrNever_ (StackFilled PatternAny))
    -> Suppliable supply UnionPattern
unionPattern variantOrigin argumentPatterns =
    { variantOrigin = variantOrigin
    , argumentPatterns = argumentPatterns |> value |> toList
    }
        |> generalWith
            (\specific ->
                Uniony specific.variantOrigin specific.argumentPatterns
            )
        |> Specific
        |> supplying argumentPatterns.supply


{-| A record fields pattern.

    import Elm.Code exposing (recordPattern, fieldVar)

    recordPattern
        (only (fieldVar "name")
            |> next (fieldVar "status")
        )

would generate

    { name, status }

See [`CombinedList`](#CombinedList).

-}
recordPattern :
    Suppliable supply (Empty possiblyOrNever_ (StackFilled String))
    -> Suppliable supply RecordPattern
recordPattern fieldNames =
    fieldNames
        |> value
        |> Stack.toList
        |> generalWith Recordy
        |> Specific
        |> supplying fieldNames.supply


{-| A record field pattern.

    import Elm.Code exposing (recordPattern, fieldVar)

    recordPattern
        (only (fieldVar "name")
            |> next (fieldVar "status")
        )

would generate

    { name, status }

See [`CombinedList`](#CombinedList).

-}
fieldVar :
    String
    ->
        Suppliable
            (Pattern specific)
            ((Expression specific -> to) -> to)
fieldVar fieldName =
    fieldName
        |> supplying ((|>) (named (local fieldName)))


{-| A 2-tuple pattern.

    import Elm.Code exposing (tuple2Pattern, var, typePattern)

    typePattern (from "Maybe" "Just")
        (only (tuple2Pattern ( var "head", var "tail" )))

would generate

    -- case nonEmpty of
        Just ( head, tail ) --->

See [`Origin`](#Origin) and [`CombinedList`](#CombinedList).

-}
tuple2Pattern :
    ( Suppliable (Pattern firstSpecific) (a -> b)
    , Suppliable (Pattern secondSpecific) (b -> to)
    )
    -> Suppliable Tuple2Pattern (a -> to)
tuple2Pattern ( firstPattern, secondPattern ) =
    ( firstPattern, secondPattern )
        |> Tuple.mapBoth value value
        |> Tuple.mapBoth any any
        |> generalWith Tuply
        |> Specific
        |> supplying
            (firstPattern.supply >> secondPattern.supply)


{-| A 3-tuple pattern.

    import Elm.Code exposing (tuple3Pattern, var)

    tuple3Pattern
        ( var "left"
        , var "selected"
        , var "right"
        )

would generate

    -- case zipList of
        ( left, selected, right ) --->

-}
tuple3Pattern :
    ( Suppliable (Pattern firstSpecific_) (a -> b)
    , Suppliable (Pattern secondSpecific_) (b -> c)
    , Suppliable (Pattern thirdSpecific_) (c -> to)
    )
    -> Suppliable Tuple3Pattern (a -> to)
tuple3Pattern ( firstPattern, secondPattern, thirdPattern ) =
    ( firstPattern, secondPattern, thirdPattern )
        |> mapAll3 Elm.Code.Util.Suppliable.value Elm.Code.Util.Suppliable.value Elm.Code.Util.Suppliable.value
        |> mapAll3 any any any
        |> supplying
            (supplyWith firstPattern
                >> supplyWith secondPattern
                >> supplyWith thirdPattern
            )



--


any : Pattern specific_ -> PatternAny
any pattern =
    pattern |> mapSpecific toGeneral
