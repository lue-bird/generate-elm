module Elm.Code.Util exposing
    ( Both2
    , SpecificOrGeneral(..)
    , mapGeneral, mapSpecific
    )

{-| Types and helpers that haven't received enough love
to be published in their own package.


## both

@docs Both2, Both3


## specific or general

@docs SpecificOrGeneral
@docs mapGeneral, mapSpecific

-}


{-| Alias for a tuple with the same type for left and right values

    type alias Point2 =
        Both2 Float

    ( 0.1, 2.3 ) --: Point2

In the context of `Elm.Code`, it's often used for two equal operation argument types:

    type ParserLiteralExpression
        = ParserOperationExpression ParserInfixOperator (Both2 ParserExpression)

-}
type alias Both2 leftAndRightValue =
    ( leftAndRightValue, leftAndRightValue )


{-| Alias for a tuple with the same type for left and right values

    type alias Point3 =
        Both3 Float

    ( 0.1, 2.3, 987.1 ) --: Point3

One example in the context of `Elm.Code`:

    type alias TypeTuple3Literal =
        Both3 TypeAny

-}
type alias Both3 leftMiddleAndRightValue =
    ( leftMiddleAndRightValue, leftMiddleAndRightValue, leftMiddleAndRightValue )



--


type SpecificOrGeneral specific general
    = Specific specific
    | General general


mapSpecific :
    (specific -> mappedSpecific)
    -> SpecificOrGeneral specific general
    -> SpecificOrGeneral mappedSpecific general
mapSpecific changeSpecific specificOrGeneral =
    case specificOrGeneral of
        Specific specific ->
            specific |> changeSpecific |> Specific

        General general ->
            general |> General


mapGeneral :
    (general -> mappedGeneral)
    -> SpecificOrGeneral specific general
    -> SpecificOrGeneral specific mappedGeneral
mapGeneral changeGeneral specificOrGeneral =
    case specificOrGeneral of
        Specific specific ->
            specific |> Specific

        General general ->
            general |> changeGeneral |> General
