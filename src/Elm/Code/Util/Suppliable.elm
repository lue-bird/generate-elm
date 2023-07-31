module Elm.Code.Util.Suppliable exposing (..)

{-| Attach a function to a value that can supply a different form.


## create

@docs supplying


## scan

@docs value


## transform

@docs mapValue

It's also used to build a

    Suppliable
        (Empty possiblyOrNever (StackFilled element))
        supplyFunction

Collect arguments.


## create

@docs empty, only, combine


## alter

@docs next

or a combination.

-}

import Elm.Code.Util exposing (SpecificOrGeneral, mapSpecific)
import Fillable exposing (Empty)
import Generalizable exposing (Generalizable, toGeneral)
import Possibly exposing (Possibly)
import Stack exposing (StackFilled)


type alias Suppliable value supplyFunction =
    ( value, { supply : supplyFunction } )


value : Suppliable value supplyFunction -> value
value =
    \( value_, _ ) -> value_


supplyWith : Suppliable value supplyFunction -> supplyFunction
supplyWith =
    \( _, function ) -> function.supply


supplying :
    supplyFunction
    -> value
    -> Suppliable value supplyFunction
supplying supply value_ =
    ( value_, { supply = supply } )


mapValue :
    (value -> valueMapped)
    -> Suppliable value supplyFunction
    -> Suppliable valueMapped supplyFunction
mapValue changeValue =
    \( value_, supply ) ->
        ( value_ |> changeValue, supply )



--


empty : Suppliable (Empty Possibly (StackFilled element_)) (a -> a)
empty =
    Fillable.empty
        |> supplying identity


next :
    Suppliable
        (SpecificOrGeneral (Generalizable specific_ specificAny) general)
        (b -> to)
    ->
        Suppliable
            (Empty
                possiblyOrNever_
                (StackFilled (SpecificOrGeneral specificAny general))
            )
            (a -> b)
    ->
        Suppliable
            (Empty
                never_
                (StackFilled (SpecificOrGeneral specificAny general))
            )
            (a -> to)
next toAdd listSupplying =
    listSupplying
        |> value
        |> Stack.stackTypedOnTop
            (Stack.only (toAdd |> value |> mapSpecific toGeneral))
        |> supplying
            (supplyWith listSupplying
                >> supplyWith toAdd
            )


{-| Short for

    import Elm.Code exposing (empty, next)

    \x -> empty |> next x

-}
only :
    Suppliable
        (SpecificOrGeneral (Generalizable specific_ specificAny) general)
        (from -> to)
    ->
        Suppliable
            (Empty
                Never
                (StackFilled (SpecificOrGeneral specificAny general))
            )
            (from -> to)
only firstElement =
    empty |> next firstElement


list :
    List
        (Suppliable
            (SpecificOrGeneral (Generalizable specific_ specificAny) general)
            supply
        )
    ->
        Suppliable
            (Empty Possibly (StackFilled (SpecificOrGeneral specificAny general)))
            ((List supply -> to) -> to)
list elements =
    elements
        |> List.map (value >> mapSpecific toGeneral)
        |> Stack.fromList
        |> supplying
            ((|>) (elements |> List.map supplyWith))


stack :
    Empty
        possiblyOrNever
        (StackFilled
            (Suppliable
                (SpecificOrGeneral (Generalizable specific_ specificAny) general)
                supply
            )
        )
    ->
        Suppliable
            (Empty
                possiblyOrNever
                (StackFilled (SpecificOrGeneral specificAny general))
            )
            ((Empty possiblyOrNever (StackFilled supply)
              -> to
             )
             -> to
            )
stack elements =
    elements
        |> Stack.map (value >> mapSpecific toGeneral)
        |> supplying
            ((|>) (elements |> Stack.map supplyWith))
