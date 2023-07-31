module ELm.Code.Util.Generalizable exposing
    ( Generalizable
    , generalWith
    , value
    , alter, mapGeneral
    , transform, toGeneral
    )

{-| `(`data`,`a function which can turn it into a more general form`)`.

This can make builders nicer

    import Generalizable exposing (generalWith, toGeneral)

    type ModuleScopeDeclaration
        = ExpressionModuleScopeDeclaration ExpressionModuleScopeDeclaration
        | TypeAliasDeclaration TypeAliasDeclaration
        | UnionTypeDeclaration UnionTypeDeclaration

    add :
        Generalizable value_ ModuleScopeDeclaration
        -> ModuleDeclaration
        -> ModuleDeclaration
    add moduleScopeDeclaration moduleDeclaration =
        { moduleDeclaration
            | declarations =
                moduleDeclaration.declarations
                    ++ (moduleScopeDeclaration |> toGeneral)
        }

    type alias Documentable code =
        { code | documentation : Maybe (List String) }

    withDocumentation :
        List String
        -> Generalizable (Documentable code) general
        -> Generalizable (Documentable code) general
    withDocumentation newDocumentation documentable =
        documentable
            |> Generalizable.alter
                (\documentableValue ->
                    { documentableValue
                        | documentation =
                            newDocumentation |> Just
                    }
                )

    type alias TypeAliasDeclaration =
        Documentable
            { name : String
            , parameters : List String
            , aliasedType : Type
            }

    ...
        |> add
            (typeAliasDeclaration "Documentable"
                (\f -> f (inferred "code"))
                (recordExtendedBy
                    [ ( "documentation", ... ) ]
                )
                |> withDocumentation
                    ...
            )

[arowM's `elm-reference`](https://dark.elm.dmy.fr/packages/arowM/elm-reference/latest/) provides the same functionality
but doesn't allow destructuring which is a pain in nested case-matches.

@docs Generalizable


## create

@docs generalWith


## scan

@docs value


## change

@docs alter, mapGeneral


## transform

@docs transform, toGeneral

-}


{-| `(`data`,`a function which can turn it into a more general form`)`
-}
type alias Generalizable value general =
    ( value
    , { toGeneral : value -> general }
    )


{-| Attach a function which can turn the value into a more general form.

    import Generalizable exposing (generalWith, toGeneral)

    3
        |> generalWith toFloat
        |> Generalizable.alter negate
        |> toGeneral
    --> -3.0

-}
generalWith :
    (value -> general)
    -> value
    -> Generalizable value general
generalWith specificValueToGeneral specificValue =
    ( specificValue, { toGeneral = specificValueToGeneral } )


{-| The current (not general) value.

    import Generalizable exposing (generalWith)

    3
        |> generalWith toFloat
        |> Generalizable.alter negate
        |> Generalizable.value
    --> -3

-}
value :
    ( value
    , { transformations_ | toGeneral : value -> general_ }
    )
    -> value
value =
    \( value_, _ ) -> value_


{-| Replace the [`value`](#value) based on its current value.

    module Number exposing (negate)

    import Generalizable exposing (generalWith, toGeneral)

    negate =
        Generalizable.alter Basics.negate

    3
        |> generalWith toFloat
        |> Number.negate
        |> toGeneral
    --> -3.0

-}
alter :
    (value -> value)
    ->
        ( value
        , { transformations | toGeneral : value -> general }
        )
    ->
        ( value
        , { transformations | toGeneral : value -> general }
        )
alter alterThis =
    \( value_, function ) ->
        ( value_ |> alterThis, function )


{-| As the new [`toGeneral`](#toGeneral) function:
Take the result of the current [`toGeneral`](#toGeneral) and change it.

    import Generalizable exposing (generalWith, mapGeneral)

    generalWith toFloat
        |> mapGeneral String.fromFloat
    --> generalWith (toFloat >> String.fromFloat)

-}
mapGeneral :
    (general -> moreGeneral)
    -> Generalizable value general
    -> Generalizable value moreGeneral
mapGeneral changeGeneral =
    \( value_, function ) ->
        value_
            |> generalWith (function.toGeneral >> changeGeneral)


{-| Take the [`value`](#value) and transform it using a specific `to...` function.

    toGeneral =
        generalize .toGeneral

is a declaration in this module.

-}
transform :
    (transformations -> (value -> generalized))
    -> ( value, transformations )
    -> generalized
transform otherGeneralizeFunction =
    \( value_, function ) ->
        value_ |> (function |> otherGeneralizeFunction)


{-| Short for [`generalize .toGeneral`](#generalize).
-}
toGeneral :
    ( value
    , { transformations_ | toGeneral : value -> general }
    )
    -> general
toGeneral =
    transform .toGeneral



-- TOP


type alias Top general =
    Generalizable general general


top :
    Generalizable value_ general
    -> Top general
top =
    toGeneral >> isTop


isTop : general -> Generalizable general general
isTop alreadyAny =
    alreadyAny |> generalWith identity
