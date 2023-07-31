module Elm.Code.Expression exposing
    ( parenthesized, access, op, call, if_
    , of_, case_
    , letDestructuring, in_
    , apL, apR
    , int, float, add, sub, mul, pow
    , append, string
    , list, cons
    , accessFun, opFun, lambda
    , composeL, composeR
    , record, updateFields
    , and, or
    , unit, tuple2, tuple3
    , named
    )

{-|


## expressions

@docs val, parenthesized, access, op, call, if_
@docs of_, Case, case_
@docs letDestructuring, letFunction letValue, letFunction, in_
@docs apL, apR


### number specific

@docs int, float, add, sub, mul, pow


### appendable specific

@docs append, string


#### list specific

@docs list, cons


### function specific

@docs accessFun, opFun, lambda
@docs composeL, composeR


### record specific

@docs RecordSpecific
@docs record, updateFields


### `Bool` specific

@docs true, false, and, or


### other specific

@docs unit, tuple2, tuple3
@docs ignore, ignoreAdvanced, keep, keepAdvanced
@docs slash, questionMark

-}

import Elm.Code exposing (Origin)
import Elm.Code.Declaration exposing (DestructuringLetDeclaration, LetDeclaration(..), LetDeclarationAny(..))
import Elm.Code.Pattern exposing (Pattern)
import Elm.Code.Util exposing (Both2, SpecificOrGeneral(..))
import Elm.Code.Util.Generalizable exposing (Generalizable, generalWith)
import Elm.Code.Util.Suppliable exposing (Suppliable)
import Elm.Syntax.Expression exposing (Expression(..))
import Fillable exposing (Empty)
import Stack exposing (StackFilled)


notComparableWith :
    (SpecificOrGeneral
        specificExpression
        (GeneralSpecificExpression
            (Expression
                (SpecificExpressionNotComparable Possibly specificExpression specificPattern)
            )
            specificPattern
        )
     -> SpecificExpressionAny
    )
    ->
        (Both2
            (Expression
                (SpecificExpressionNotComparable Possibly specificExpression specificPattern)
            )
         -> Both2ExpressionAny
        )
    ->
        SpecificOrGeneral
            specificExpression
            (GeneralSpecificExpression
                (Expression
                    (SpecificExpressionNotComparable Possibly specificExpression specificPattern)
                )
                specificPattern
            )
    ->
        Expression
            (SpecificExpressionNotComparable Possibly specificExpression specificPattern)
notComparableWith toSpecificExpressionAny both2ToAny specificExpression =
    ( specificExpression
        |> mapGeneral
            (mapGeneralSpecificExpression
                (mapSpecific SpecificExpressionIn)
            )
    , { toAny =
            mapGeneral
                (mapGeneralSpecificExpression
                    (mapSpecific (\(SpecificExpressionIn out) -> out))
                )
                |> toSpecificExpressionAny
      , both2ToAny =
            Tuple.mapBoth
                (mapSpecific
                    (\(SpecificExpressionIn out) -> out)
                )
                |> both2ToAny
      , comparable = Fillable.empty
      }
    )
        |> Specific


mapGeneralSpecificExpression :
    (expression -> expressionMapped)
    -> GeneralSpecificExpression expression specificPattern
    -> GeneralSpecificExpression expressionMapped specificPattern
mapGeneralSpecificExpression generalSpecificExpression =
    case generalSpecificExpression of
        _ ->
            Debug.todo ""


named : Origin -> ExpressionNotNamed never_ specific_
named origin =
    origin
        |> generalWith Named
        |> General


{-| A `()` value.

    import Elm.Code exposing (call, val, local, unit)

    val (local "lazyValue") |> call ( unit (), [] )

would generate

    lazyValue ()

See [`local`](#local).

-}
unit : () -> UnitExpression
unit unitValue =
    unitValue
        |> notComparableWith
            (Unity >> SpecificExpressionAny)
            (Unity >> Both2ExpressionAny)


boolValueExpression : Bool -> BoolExpression
boolValueExpression boolValue =
    boolValue |> BoolExpression |> boolExpression


boolExpression : BoolLiteralExpression -> BoolExpression
boolExpression boolLiteralExpression =
    boolLiteralExpression
        |> notComparableWith
            (Booly >> SpecificExpressionAny)
            (Booly >> Both2ExpressionAny)


{-| `True`.

    import Elm.Code exposing (updateFields, field, true)

    \user ->
        updateFields user
            ( field "active" true, [] )

would generate

    { user | active = True }

-}
true : BoolExpression
true =
    True |> boolValueExpression


{-| `False`.

    import Elm.Code exposing (updateFields, field, false)

    \user ->
        updateFields user
            ( field "active" false, [] )

would generate

    { user | active = False }

-}
false : BoolExpression
false =
    False |> boolValueExpression


{-| An `Int`.

    import Elm.Code exposing (tuple3, int, Base(..))

    tuple3 ( 3 |> int Base10, 13 |> int Base10, 2012 |> int Base10 )

would generate

    ( 3, 13, 2012 )

[`hex`](#hex) to use a different [`Base`](#Base).

-}
int : Base -> Int -> NumberExpression specific_ pattern_
int base intValue =
    ( intValue, base )
        |> General
        |> generalWith
            (\numberSpecific ->
                { toAny =
                    \() ->
                        numberSpecific
                            |> specificNumberExpressionAny
                            |> Specific
                , toComparable =
                    (\() ->
                        numberSpecific
                            |> NumberComparably
                    )
                        |> just
                }
            )
        |> Specific
        |> Specific


{-| A `Float`.

    import Elm.Code exposing (tuple2, float)

    tuple2 ( float 3.13, float 20.12 )

would generate

    ( 3.13, 20.12 )

-}
float : Float -> FloatExpression
float floatValue =
    floatValue
        |> FloatExpression
        |> generalWith Floaty
        |> Specific
        |> generalWith
            (\numberSpecific ->
                { toAny =
                    \() ->
                        numberSpecific
                            |> specificNumberExpressionAny
                            |> Specific
                , toComparable =
                    (\() ->
                        numberSpecific
                            |> NumberComparably
                    )
                        |> just
                }
            )
        |> Specific
        |> Specific


{-| A `String`.

    import Elm.Code exposing (val, call, string, local)

    val (local "greet")
        |> call ( string "world", [] )

would generate

    greet "world"

See [`local`](#local).

-}
string :
    String
    -> StringExpression
string string_ =
    string_
        |> generalWith Stringy
        |> Specific
        |> generalWith
            (specificAppendableAny >> Appendably)
        |> Specific


{-| An infix operation.

    import Elm.Code exposing (int, sub, op)

    \x -> x |> op sub (int 1)

would generate

    x - 1

See [`InfixOperatorReference`](#InfixOperatorReference).

-}
op :
    OperatorOn specific_ operation
    -> operation
op infixOperator =
    infixOperator |> Generalizable.transform .operation


{-| An operator in its prefix function shape.

    import Elm.Code exposing (val, call, opFun, add, int)

    val (from "List" "map")
        |> call ( opFun add |> call ( int 1, [] ), [] )

would generate

    List.map ((+) 1)

See [`InfixOperator`](#InfixOperator) and [`Origin`](#Origin).

-}
opFun :
    OperatorOn
        operatorSpecific_
        (Expression (SpecificExpressionNotComparable leftPossiblyOrNever_ leftSpecific_ leftPattern_)
         -> Expression (SpecificExpressionNotComparable rightPossiblyOrNever_ rightSpecific_ rightPattern_)
         -> Expression (SpecificExpressionNotComparable resultPossiblyOrNever_ resultSpecific_ resultPattern_)
        )
    -> FunctionExpression
opFun operator =
    (operator.code |> toGeneral)
        |> OperatorFunctionExpression
        |> notComparableWith
            (Functiony >> SpecificExpressionAny)
            (Functiony >> Both2ExpressionAny)


{-| A function or value.

    import Elm.Code exposing (val, call, string, local)

    val (local "greet")
        |> call ( string "world", [] )

would generate

    greet "world"

See [`local`](#local).

For values from a different module, you can get extra type-safety by annotating:

    import Elm.Code exposing (CharExpression, ExpressionNotNamed, FunctionExpression, IntExpression, from, val)

    charToCode :
        ExpressionNotNamed
            Never
            (FunctionSpecificExpression CharExpression IntExpression)
    charToCode =
        val (from "Char" "toCode")

-}
named : Origin -> ExpressionNotNamed Never specific_
named origin =
    Elm.Expression.Internal.named


requiredImports :
    Expression (SpecificExpressionNotComparable specific_ pattern_ possiblyOrNever_)
    -> List Imports
requiredImports expression =
    let
        step =
            requiredImports
    in
    case expression |> any of
        General general ->
            case general of
                _ ->
                    Debug.todo ""

        Specific specific ->
            case specific of
                Recordy recordSpecific ->
                    let
                        stepFields fields =
                            fields
                                |> List.NonEmpty.toList
                                |> List.concatMap (Tuple.second >> step)
                    in
                    case recordSpecific of
                        EmptyRecordConstructorExpression origin ->
                            []

                        RecordFieldsExpression fields ->
                            fields |> stepFields

                        RecordUpdateExpression valOrigin updatedFields ->
                            (case valOrigin |> Tuple.first of
                                DeclaredIn moduleName ->
                                    [ [ import_ moduleName ] |> importsFromList ]

                                DeclaredLocally ->
                                    []
                            )
                                ++ (updatedFields |> stepFields)

                _ ->
                    Debug.todo ""


{-| An expression `(`wrapped in parens`)`.
-}
parenthesized : Expression specific -> Expression specific
parenthesized expressionToParenthesize =
    expressionToParenthesize
        |> Parenthesized
        |> General
        |> Specific


{-| A 2-tuple.

    import Elm.Code exposing (tuple2, float)

    tuple2 ( float 3.13, float 20.12 )

would generate

    ( 3.13, 20.12 )

-}
tuple2 :
    ( Expression (SpecificExpressionNotComparable firstSpecificExpression firstPattern canTupleBeComparable)
    , Expression (SpecificExpressionNotComparable secondSpecificExpression secondPattern canTupleBeComparable)
    )
    ->
        Expression
            (SpecificExpressionNotComparable
                ( Expression (SpecificExpressionNotComparable firstSpecificExpression firstPattern canTupleBeComparable)
                , Expression (SpecificExpressionNotComparable secondSpecificExpression secondPattern canTupleBeComparable)
                )
                (Pattern ( firstPattern, secondPattern ))
                canTupleBeComparable
            )
tuple2 ( firstExpression, secondExpression ) =
    ( firstExpression, secondExpression )
        |> Tuple.mapBoth any any
        |> generalWith Tuple2y
        |> Specific


{-| A 3-tuple.

    import Elm.Code exposing (tuple3, int)

    tuple3 ( int 3, int 13, int 2012 )

would generate

    ( 3, 13, 2012 )

-}
tuple3 :
    ( Expression (SpecificExpressionNotComparable firstSpecificExpression firstPattern canTupleBeComparable)
    , Expression (SpecificExpressionNotComparable secondSpecificExpression secondPattern canTupleBeComparable)
    , Expression (SpecificExpressionNotComparable thirdSpecificExpression thirdPattern canTupleBeComparable)
    )
    ->
        Expression
            (SpecificExpressionNotComparable
                ( Expression (SpecificExpressionNotComparable firstSpecificExpression firstPattern canTupleBeComparable)
                , Expression (SpecificExpressionNotComparable secondSpecificExpression secondPattern canTupleBeComparable)
                , Expression (SpecificExpressionNotComparable thirdSpecificExpression thirdPattern canTupleBeComparable)
                )
                (Pattern ( firstPattern, secondPattern, thirdPattern ))
                canTupleBeComparable
            )
tuple3 ( firstExpression, secondExpression, thirdExpression ) =
    ( firstExpression, secondExpression, thirdExpression )
        |> mapAll3 any any any
        |> generalWith Tuple3y
        |> Specific


{-| Arguments applied to a function.

    import Elm.Code exposing (val, call, string, local)

    val (local "greet")
        |> call (string "world") []

would generate

    greet "world"

See [`Origin`](#Origin).

Note: if arguments after the first are of a different specific (e.g. one is a number, the other is a list) you'll have to use [`use`](#use).

-}
call :
    Expression
        (SpecificExpressionNotComparable
            aArgument_
            aArgumentKind_
            canFirstArgumentBeNotComparable_
        )
    ->
        List
            (Expression
                (SpecificExpressionNotComparable
                    followingArgument_
                    followingArgumentKind_
                    canFollowingArgumentBeNotComparable_
                )
            )
    -> FunctionExpression
    -> Expression specific_
call firstArgument followingArguments function =
    Call
        function
        (topAndBelow firstArgument followingArguments
            |> Stack.mapTop any
            |> Stack.mapBelowTop any
            |> Stack.toTopAndBelow
        )
        |> General


{-| A `List`.

    import Elm.Code exposing (list, string)

    list
        [ string "hello"
        , string "world"
        ]

would generate

    [ "hello", "world" ]

Here specifically, you could also write

    [ "hello", "world" ]
        |> List.map string
        |> list

-}
list :
    List
        (Expression
            (SpecificExpressionNotComparable elementSpecific elementPattern possiblyOrNever)
        )
    -> ListExpressionNotComparable elementSpecific elementPattern possiblyOrNever
list elements =
    elements
        |> ListExpression
        |> generalWith Listy
        |> Specific
        |> generalWith
            (\appendably ->
                { toAny =
                    appendably
                        |> Appendably
                        |> SpecificExpressionAny
                , toComparable = Debug.todo ""
                }
            )
        |> Specific
        |> Specific


{-| A record.

    import Elm.Code exposing (record, field, string)

    record
        [ field "username" (string "lue")
        , field "icon" (string "🐦")
        ]

would generate

    { username = "lue", icon = "🐦" }

-}
record :
    RecordFieldsLiteralExpression
    -> Generalizable RecordFieldsLiteralExpression RecordExpression
record fields =
    fields
        |> generalWith RecordFieldsExpression
        |> mapGeneral literalToRecordExpression


literalToRecordExpression :
    RecordLiteralExpression
    -> RecordExpression
literalToRecordExpression literalRecordExpression =
    literalRecordExpression
        |> notComparableWith
            (Recordy >> SpecificExpressionAny)
            (Recordy >> Both2ExpressionAny)


{-| A record update expression.

    import Elm.Code exposing (updateFields, string)

    \user ->
        user
            |> updateFields
                (field "status" (string "absent"))
                []

would generate

    { user | status = "absent" }

-}
updateFields :
    Field ExpressionAny
    -> List (Field ExpressionAny)
    -> Generalizable Origin RecordExpression
    -> Generalizable RecordUpdateLiteralExpression RecordExpression
updateFields firstUpdatedField followingUpdatedFields recordExpression =
    ( recordExpression |> Generalizable.value
    , ( firstUpdatedField, followingUpdatedFields )
    )
        |> generalWith RecordUpdateExpression
        |> mapGeneral literalToRecordExpression


{-| An anonymous function.

    import Elm.Code exposing (lambda, var, op, sub, only)

    lambda (only (var "x"))
        (op sub (int 1))

would generate

    \x -> x - 1

-}
lambda :
    Suppliable
        (ListIs NotEmpty PatternAny)
        (toExpression
         -> Expression (SpecificExpressionNotComparable specific pattern possiblyOrNever)
        )
    -> toExpression
    -> Generalizable AnonymousFunctionLiteralExpression FunctionExpression
lambda argumentPatterns toResult =
    ( argumentPatterns |> dropSupplyFunction |> Stack.toTopAndBelow
    , In (toResult |> supplyWith argumentPatterns |> any)
    )
        |> generalWith UnnamedFunctionExpression
        |> mapGeneral literalToFunctionExpression


literalToFunctionExpression :
    FunctionLiteralExpression
    -> FunctionExpression
literalToFunctionExpression literalFunctionExpression =
    literalFunctionExpression
        |> notComparableWith
            (Functiony >> SpecificExpressionAny)
            (Functiony >> Both2ExpressionAny)


{-| An `if ... then ... else ...`.

    import Elm.Code exposing (if_, access, string)

    \user ->
        if_ (user |> access.field "active")
            { then = string "active"
            , else = string "inactive"
            }

would generate

    if user.active then
        "active"

    else
        "inactive"

todo (?): make builder

    \user ->
        if_ (user |> access.field "active")
            |> then_ (string "active")
            |> else_ (string "inactive")

-}
if_ :
    BoolExpression
    ->
        { then_ :
            Expression
                (SpecificExpressionNotComparable resultKind resultPattern canResultBeNotComparable)
        , else_ :
            Expression
                (SpecificExpressionNotComparable resultKind resultPattern canResultBeNotComparable)
        }
    ->
        Expression
            (SpecificExpressionNotComparable resultKind resultPattern canResultBeNotComparable)
if_ condition cases =
    IfExpression condition
        (Then cases.then_)
        (Else cases.else_)
        |> General
        |> Specific


{-| A `case ... of`.

    import Elm.Code exposing (of_, val, local, typePattern, var, list, case_, use)

    \maybe ->
        maybe
            |> of_
                (case_
                    (typePattern [ "Maybe" ], "Just" ) [ var "value" ])
                    (\value -> list [ use value ])
                )
                [ case_
                    (typePattern [ "Maybe" ], "Nothing" ))
                    (list [])
                ]

would generate

    case maybe of
        Just value ->
            [ value ]

        Nothing ->
            []

-}
of_ :
    Case
        pattern
        (Expression
            (SpecificExpressionNotComparable expressionSpecific pattern canExpressionBeNotComparable)
        )
    ->
        List
            (Case
                pattern
                (Expression
                    (SpecificExpressionNotComparable expressionSpecific pattern canExpressionBeNotComparable)
                )
            )
    ->
        Expression
            (SpecificExpressionNotComparable expressionSpecific pattern canExpressionBeNotComparable)
    ->
        Expression
            (SpecificExpressionNotComparable expressionSpecific pattern canExpressionBeNotComparable)
of_ firstCase followingCases expressionToCaseOn =
    CaseExpression expressionToCaseOn
        (Of ( firstCase, followingCases ))
        |> General
        |> Specific


{-| One [case](#Case) match inside a [`of_`](#of_).

    import Elm.Code exposing (of_, val, local, typePattern, var, list, case_, use)

    \maybe ->
        maybe
            |> of_
                (case_
                    (typePattern [ "Maybe" ], "Just" ) [ var "value" ])
                    (\value -> list [ use value ])
                )
                [ case_
                    (typePattern [ "Maybe" ], "Nothing" ))
                    (list [])
                ]

would generate

    case maybe of
        Just value ->
            [ value ]

        Nothing ->
            []

-}
case_ :
    Suppliable pattern (toExpression -> expression)
    -> toExpression
    -> Case pattern expression
case_ pattern toExpression =
    Case (pattern |> dropSupplyFunction)
        (In (toExpression |> supplyWith pattern))


in_ :
    toExpression
    ->
        Suppliable
            (toExpression -> Expression specific)
            (ListIs NotEmpty LetDeclarationAny)
    -> Expression specific
in_ toResultExpression letDeclarations =
    Let letDeclarations.list
        (In
            (toResultExpression
                |> supplyWith letDeclarations
            )
        )
        |> General
        |> Specific


{-| A record field access expression.

    import Elm.Code exposing (access)

    \user -> user |> access.field "name"

would generate

    user.name

-}
access :
    { field :
        String
        -> RecordExpression
        -> Expression specific_
    }
access =
    { field = accessField }


accessField :
    String
    -> RecordExpression
    -> Expression specific_
accessField fieldToAccess recordExpression =
    RecordAccess recordExpression fieldToAccess
        |> General


{-| A record field access function.

    import Elm.Code exposing (accessFun, val, call)

    val (from "List" "map")
        |> call ( accessFun .field "name", [] )

would generate

    List.map .name

-}
accessFun :
    ({ record_ | field : Never } -> Never)
    -> String
    -> FunctionExpression
accessFun _ fieldToAccess =
    RecordAccessFunctionExpression fieldToAccess
        |> generalWith Functiony
        |> Specific



--


any :
    Expression
        (SpecificExpressionNotComparable possiblyOrNever_ specific_ pattern_)
    -> ExpressionAny
any expression =
    expression
        |> mapSpecific
            (\( specific, { toAny } ) ->
                specific |> SpecificExpressionIn |> toAny
            )



--


operationToSyntax :
    Operator
    -> Both2 Expression.Expression
    -> Expression.Expression
operationToSyntax infixOperator arguments =
    let
        ( left, right ) =
            arguments

        { origin, associativity } =
            infixOperator |> operatorInfo
    in
    Expression.OperatorApplication
        (origin |> Tuple.second)
        (associativity |> associativityToSyntax)
        (left |> Node emptyRange)
        (right |> Node emptyRange)
