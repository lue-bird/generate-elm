module Elm.Code.Operator exposing (..)

{-| Binary operator

@docs info, byOrigin


## in expression

@docs and, or
@docs eq, neq
@docs append, cons
@docs lt, le, gt, ge


## [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) transform

@docs associativityToSyntax, associativityFromSyntax

-}

import ELm.Code.Util.Generalizable exposing (generalWith)
import Elm.Code.Common exposing (Appendably(..), BoolExpression, Both2ExpressionAny, Expression, ExpressionAny, Expressiony(..), FloatExpression, FunctionExpression, IntExpression, ListExpressionNotComparable, Numbery(..), Operator, OperatorApply(..), OperatorBoolCombine(..), OperatorCompare(..), OperatorDivide(..), OperatorEqualCheck(..), OperatorFunctionCompose(..), OperatorNumberCombine(..), OperatorParser(..), OperatorUrlParser(..), Origin, ParserAdvancedExpression, ParserAdvancedLiteralExpression, ParserExpression, Parsery(..), SpecificExpressionAny, SpecificExpressionNotComparable, UrlParserExpression, UrlParserLiteralExpression, from)
import Elm.Code.Type exposing (TypeNotUnconstrained(..))
import Elm.Code.Util exposing (SpecificOrGeneral(..), mapGeneral, mapSpecific)
import Elm.Code.Util.Generalizable
import Elm.Docs exposing (Associativity(..))
import Elm.Syntax.Infix
import Stack exposing (top)


info :
    Operator
    ->
        { moduleOrigin : String
        , associativity : Associativity
        , precedence : Int
        , symbol : String
        }
info infixOperator =
    let
        opInfo moduleName symbol associativity precedence =
            { moduleOrigin = moduleName
            , associativity = associativity
            , precedence = precedence
            , symbol = symbol
            }

        basicsOp operatorSymbol associativity precedence =
            opInfo "Basics" operatorSymbol associativity precedence

        listOp operatorSymbol associativity precedence =
            opInfo "List" operatorSymbol associativity precedence

        parserOp operatorSymbol precedence =
            opInfo "Parser" operatorSymbol Left precedence

        parserAdvancedOp operatorSymbol precedence =
            opInfo "Parser.Advanced" operatorSymbol Left precedence

        urlParserOp operatorSymbol associativity precedence =
            opInfo "Url.Parser" operatorSymbol associativity precedence
    in
    case infixOperator of
        OperatorApply ap ->
            case ap of
                ApL ->
                    basicsOp "<|" Right 0

                ApR ->
                    basicsOp "|>" Left 0

        OperatorFunctionCompose compose ->
            case compose of
                ComposeL ->
                    basicsOp "<<" Left 9

                ComposeR ->
                    basicsOp ">>" Right 9

        OperatorBoolCombine boolOp ->
            case boolOp of
                And ->
                    basicsOp "&&" Right 2

                Or ->
                    basicsOp "||" Right 3

        OperatorEqualCheck equalCheckOp ->
            case equalCheckOp of
                Eq ->
                    basicsOp "==" None 4

                Neq ->
                    basicsOp "/=" None 4

        OperatorCompare compOp ->
            case compOp of
                Lt ->
                    basicsOp "<" None 4

                Le ->
                    basicsOp "<=" None 4

                Gt ->
                    basicsOp ">" None 4

                Ge ->
                    basicsOp ">=" None 4

        Append ->
            basicsOp "++" Right 5

        OperatorNumberCombine num ->
            case num of
                Add ->
                    basicsOp "+" Left 6

                Sub ->
                    basicsOp "-" Left 6

                Mul ->
                    basicsOp "*" Left 7

                Pow ->
                    basicsOp "^" Right 8

        Fdiv ->
            basicsOp "/" Left 7

        Idiv ->
            basicsOp "//" Left 7

        Cons ->
            listOp "::" Right 5

        OperatorParser parserOperator ->
            case parserOperator of
                ParserSimply valueOp ->
                    case valueOp of
                        Ignore ->
                            parserOp "|." 6

                        Keep ->
                            parserOp "|=" 5

                ParserAdvancedy valueOp ->
                    case valueOp of
                        Ignore ->
                            parserAdvancedOp "|." 6

                        Keep ->
                            parserAdvancedOp "|=" 5

                UrlParsery valueOp ->
                    case valueOp of
                        Slash ->
                            urlParserOp "</>" Right 7

                        QuestionMark ->
                            urlParserOp "<?>" Left 8


{-| Try to find a [`InfixOperator`](Elm-Code#InfixOperator) with the given [`Origin`](#Origin).
If that's not possible, `Nothing` is returned.
-}
byOrigin : Origin -> Maybe Operator
byOrigin locatedOperatorSymbol =
    [ [ ApL, ApR ]
    , [ ComposeL, ComposeR ] |> List.map OperatorFunctionCompose
    , [ And, Or ] |> List.map OperatorBoolCombine
    , [ Eq, Neq ] |> List.map OperatorEqualCheck
    , [ Lt, Le, Gt, Ge ] |> List.map OperatorCompare
    , [ Append ]
    , [ Add, Sub, Mul, Pow ] |> List.map OperatorNumberCombine
    , [ Fdiv ]
    , [ Idiv ]
    , [ Cons ]
    , [ [ Ignore, Keep ] |> List.map ParserSimply
      , [ Ignore, Keep ] |> List.map ParserAdvancedy
      , [ Slash, QuestionMark ] |> List.map UrlParsery
      ]
        |> List.concat
        |> List.map OperatorParser
    ]
        |> List.concat
        |> List.map (\op_ -> ( op_, info op_ ))
        |> List.filter
            (\( _, { origin } ) ->
                origin == locatedOperatorSymbol
            )
        |> List.head
        |> Maybe.map Tuple.first


associativityToSyntax : Associativity -> Elm.Syntax.Infix.InfixDirection
associativityToSyntax associativity =
    case associativity of
        Left ->
            Elm.Syntax.Infix.Left

        Right ->
            Elm.Syntax.Infix.Right

        None ->
            Elm.Syntax.Infix.Non



--


{-| A generated infix operator. Used in

  - [`op`](#op)
  - [`opFun`](#opFun)

all infix operators:


#### from `Basics`

  - `<|`: [`apL`](#apL)
  - `|>`: [`apR`](#apR)
  - `<<`: [`composeL`](#composeL)
  - `>>`: [`composeR`](#composeR)
  - `&&`: [`and`](#and)
  - `||`: [`or`](#or)
  - `==`: [`eq`](#eq)
  - `/=`: [`neq`](#neq)
  - `<`: [`lt`](#lt)
  - `<=`: [`le`](#le)
  - `>`: [`gt`](#gt)
  - `>=`: [`ge`](#ge)
  - `+`: [`add`](#add)
  - `-`: [`sub`](#sub)
  - `*`: [`mul`](#mul)
  - `/`: [`fdiv`](#fdiv)
  - `//`: [`idiv`](#idiv)
  - `^`: [`pow`](#pow)
  - `++`: [`append`](#append)


#### from `List`

  - `::`: [`cons`](#cons)


#### from `Parser`

  - `|=`: [`keep`](#keep)
  - `|.`: [`ignore`](#ignore)


#### from `Parser.Advanced`

  - `|=`: [`keepAdvanced`](#keepAdvanced)
  - `|.`: [`ignoreAdvanced`](#ignoreAdvanced)


#### from `Url.Parser`

  - `</>`: [`slash`](#slash)
  - `<?>`: [`questionMark`](#questionMark)

-}
type alias OperatorOn specific operation =
    ( specific
    , { toGeneral : specific -> Operator
      , operation : specific -> operation
      }
    )


infixOperatorWith :
    (specificOperator -> Operator)
    -> (specificOperator -> ( left, right ) -> result)
    -> specificOperator
    -> OperatorOn specificOperator (left -> right -> result)
infixOperatorWith toGeneralInfixOperator operation infixOperator =
    { code =
        infixOperator
            |> generalWith toGeneralInfixOperator
    , operation =
        \inOperator l r ->
            operation inOperator ( l, r )
    }


boolCombineInfixOperator :
    BoolCombineOperator
    -> BoolInfixOperatorOn BoolCombineOperator BoolExpression
boolCombineInfixOperator =
    infixOperatorWith
        BoolCombineOperator
        CombineBoolOperationExpression
        (generalWith Booly >> Specific)


type alias BoolInfixOperatorOn specificOperator argumentExpression =
    OperatorOn
        specificOperator
        (argumentExpression
         -> argumentExpression
         -> BoolExpression
        )


and : BoolInfixOperatorOn BoolCombineOperator BoolExpression
and =
    And |> boolCombineInfixOperator


or : BoolInfixOperatorOn BoolCombineOperator BoolExpression
or =
    Or |> boolCombineInfixOperator


equalCheckInfixOperator :
    EqualCheckOperator
    -> BoolInfixOperatorOn EqualCheckOperator (Expression argumentSpecific)
equalCheckInfixOperator =
    infixOperatorWith
        EqualCheckOperator
        (\inOp arguments ->
            EqualCheckOperationExpression inOp
                (arguments |> Tuple.mapBoth expressionAny)
        )
        (Booly |> Specific)


eq : BoolInfixOperatorOn EqualCheckOperator (Expression argumentSpecific)
eq =
    Eq |> equalCheckInfixOperator


neq : BoolInfixOperatorOn EqualCheckOperator (Expression argumentSpecific)
neq =
    Neq |> equalCheckInfixOperator


specificNumberExpressionAny :
    SpecificNumberExpression specific_ pattern
    -> NumberSpecificExpressionAny
specificNumberExpressionAny numberExpression =
    let
        step =
            mapExpression specificNumberExpressionAny
                Numbery
    in
    numberExpression
        |> mapGeneral
            (\general ->
                case general of
                    NegateExpression inner ->
                        inner |> step |> NegateExpression

                    NumberCombineOperationExpression left inOp right ->
                        NumberCombineOperationExpression
                            (step left)
                            inOp
                            (step right)
            )


specificAppendableAny :
    SpecificAppendableSpecificExpressionNotComparable
        possiblyOrNever
        appendableExpression
        literal
        pattern
    -> SpecificAppendableSpecificExpressionAny
specificAppendableAny appendableSpecificExpression =
    appendableSpecificExpression
        |> mapGeneral
            (\(AppendOperationExpression left right) ->
                AppendOperationExpression
                    (left
                        |> mapExpression specificAppendableAny
                            AppendableSpecificExpression
                    )
                    (right
                        |> mapExpression specificAppendableAny
                            AppendableSpecificExpression
                    )
            )


type alias CompareOperatorOn expressionSpecific patternSpecific =
    OperatorOn
        CompareOperator
        (Expression (SpecificExpressionNotComparable Never expressionSpecific patternSpecific)
         -> Expression (SpecificExpressionNotComparable Never expressionSpecific patternSpecific)
         -> BoolExpression
        )


compareInfixOperator :
    CompareOperator
    -> CompareOperatorOn expressionSpecific_ pattern_
compareInfixOperator =
    infixOperatorWith
        CompareOperator
        (\inOp ( l, r ) ->
            CompareOperationExpression inOp
                ( l |> mapSpecific top
                , r |> mapSpecific top
                )
        )
        (Booly |> Specific)


lt : CompareOperatorOn expressionSpecific_ pattern_
lt =
    Lt |> compareInfixOperator


le : CompareOperatorOn expressionSpecific_ pattern_
le =
    Le |> compareInfixOperator


gt : CompareOperatorOn expressionSpecific_ pattern_
gt =
    Gt |> compareInfixOperator


ge : CompareOperatorOn expressionSpecific_ pattern_
ge =
    Ge |> compareInfixOperator


functionComposeInfixOperator :
    FunctionComposeOperator
    ->
        OperatorOn
            FunctionComposeOperator
            (FunctionExpression
             -> FunctionExpression
             -> FunctionExpression
            )
functionComposeInfixOperator =
    infixOperatorWith
        FunctionComposeOperator
        FunctionCompositionOperationExpression
        (generalWith Functiony
            >> Specific
        )


composeR :
    OperatorOn
        FunctionComposeOperator
        (FunctionExpression
         -> FunctionExpression
         -> FunctionExpression
        )
composeR =
    ComposeR |> functionComposeInfixOperator


composeL :
    OperatorOn
        FunctionComposeOperator
        (FunctionExpression
         -> FunctionExpression
         -> FunctionExpression
        )
composeL =
    ComposeL |> functionComposeInfixOperator


numberCombineInfixOperator :
    NumberCombineOperator
    ->
        OperatorOn
            NumberCombineOperator
            (SpecificNumberSpecificExpression specific pattern
             -> SpecificNumberSpecificExpression specific pattern
             -> SpecificNumberSpecificExpression specific pattern
            )
numberCombineInfixOperator =
    infixOperatorWith
        NumberCombineOperator
        NumberCombineOperationExpression
        (specificNumberExpressionAny
            >> Numbery
            |> Specific
        )


add :
    OperatorOn
        NumberCombineOperator
        (SpecificNumberSpecificExpression specific pattern
         -> SpecificNumberSpecificExpression specific pattern
         -> SpecificNumberSpecificExpression specific pattern
        )
add =
    Add |> numberCombineInfixOperator


sub :
    OperatorOn
        NumberCombineOperator
        (SpecificNumberSpecificExpression specific pattern
         -> SpecificNumberSpecificExpression specific pattern
         -> SpecificNumberSpecificExpression specific pattern
        )
sub =
    Sub |> numberCombineInfixOperator


mul :
    OperatorOn
        NumberCombineOperator
        (SpecificNumberSpecificExpression specific pattern
         -> SpecificNumberSpecificExpression specific pattern
         -> SpecificNumberSpecificExpression specific pattern
        )
mul =
    Mul |> numberCombineInfixOperator


pow :
    OperatorOn
        NumberCombineOperator
        (SpecificNumberSpecificExpression specific pattern
         -> SpecificNumberSpecificExpression specific pattern
         -> SpecificNumberSpecificExpression specific pattern
        )
pow =
    Pow |> numberCombineInfixOperator


{-| `<|`
-}
apL :
    OperatorOn
        { apL : () }
        (FunctionExpression
         -> ExpressionAny
         -> ExpressionAny
        )
apL =
    { apL = () }
        |> infixOperatorWith
            (\_ -> ApL)
            (\_ -> ApLOperationExpression >> General)
            identity


{-| `|>`
-}
apR :
    OperatorOn
        { apR : () }
        (ExpressionAny
         -> FunctionExpression
         -> ExpressionAny
        )
apR =
    { apR = () }
        |> infixOperatorWith
            (\_ -> ApR)
            (\_ -> ApROperationExpression >> General)
            identity


parserInfixOperator :
    ParserOperator
    ->
        OperatorOn
            ParserOperator
            (ParserExpression
             -> ParserExpression
             -> ParserExpression
            )
parserInfixOperator =
    infixOperatorWith
        ParserOperator
        ParserOperationExpression
        (generalWith Parsery >> Specific)


ignore :
    OperatorOn
        ParserOperator
        (ParserExpression
         -> ParserExpression
         -> ParserExpression
        )
ignore =
    Ignore |> parserInfixOperator


keep :
    OperatorOn
        ParserOperator
        (ParserExpression
         -> ParserExpression
         -> ParserExpression
        )
keep =
    Keep |> parserInfixOperator


parserAdvancedOperator :
    ParserOperator
    ->
        OperatorOn
            ParserOperator
            (ParserAdvancedExpression
             -> ParserAdvancedExpression
             -> ParserAdvancedExpression
            )
parserAdvancedOperator =
    infixOperatorWith
        ParserAdvancedOperator
        ParserAdvancedOperationExpression
        (\inOp arguments ->
            ParserAdvancedOperationExpression inOp arguments
                |> literalToParserAdvancedExpression
        )


literalToParserAdvancedExpression : ParserAdvancedLiteralExpression -> ParserAdvancedExpression
literalToParserAdvancedExpression advancedParserLiteral =
    advancedParserLiteral
        |> notComparableWith
            (ParserAdvancedy >> SpecificExpressionAny)
            (ParserAdvancedy >> Both2ExpressionAny)


ignoreAdvanced :
    OperatorOn
        ParserOperator
        (ParserAdvancedExpression
         -> ParserAdvancedExpression
         -> ParserAdvancedExpression
        )
ignoreAdvanced =
    Ignore |> parserAdvancedOperator


keepAdvanced :
    OperatorOn
        ParserOperator
        (ParserAdvancedExpression
         -> ParserAdvancedExpression
         -> ParserAdvancedExpression
        )
keepAdvanced =
    Keep |> parserAdvancedOperator


urlParserOperator :
    UrlParserOperator
    ->
        OperatorOn
            UrlParserOperator
            (UrlParserExpression
             -> UrlParserExpression
             -> UrlParserExpression
            )
urlParserOperator =
    infixOperatorWith
        UrlParserInfixOperator
        (\inOp args ->
            UrlParserOperationExpression inOp args
                |> literalToUrlParserExpression
        )


literalToUrlParserExpression : UrlParserLiteralExpression -> UrlParserExpression
literalToUrlParserExpression urlParserLiteral =
    urlParserLiteral
        |> notComparableWith
            (UrlParsery >> SpecificExpressionAny)
            (UrlParsery >> Both2ExpressionAny)


slash :
    OperatorOn
        UrlParserOperator
        (UrlParserExpression
         -> UrlParserExpression
         -> UrlParserExpression
        )
slash =
    Slash |> urlParserOperator


questionMark :
    OperatorOn
        UrlParserOperator
        (UrlParserExpression
         -> UrlParserExpression
         -> UrlParserExpression
        )
questionMark =
    QuestionMark |> urlParserOperator


{-| `/`
-}
fdiv :
    InfixOperatorOn
        { fdiv : () }
        (FloatExpression -> FloatExpression -> FloatExpression)
fdiv =
    { fdiv = () }
        |> infixOperatorWith (\_ -> Fdiv)
            (\_ ->
                FdivOperationExpression
                    >> generalWith Floaty
                    >> Specific
            )
            (generalWith
                (specificNumberExpressionAny >> Numbery)
                |> Specific
            )


{-| `//`
-}
idiv :
    InfixOperatorOn
        { idiv : () }
        (IntExpression -> IntExpression -> IntExpression)
idiv =
    { idiv = () }
        |> infixOperatorWith (\_ -> Idiv)
            (\_ ->
                IdivOperationExpression
                    >> generalWith Inty
                    >> Specific
            )
            (Specific
                << generalWith
                    (specificNumberExpressionAny
                        >> Numbery
                    )
            )


{-| `++`
-}
append :
    InfixOperatorOn
        { append : () }
        (AppendableExpression
            specific
            pattern
            possiblyOrNever
         ->
            AppendableExpression
                specific
                pattern
                possiblyOrNever
         ->
            AppendableExpression
                specific
                pattern
                possiblyOrNever
        )
append =
    { append = () }
        |> infixOperatorWith (\_ -> Append)
            (\_ -> AppendOperationExpression)
            (generalWith
                (specificAppendableAny
                    >> mapListAnyAppendable anyListElements
                    >> Appendably
                )
                >> Specific
            )


{-| `::`
-}
cons :
    InfixOperatorOn
        { cons : () }
        (Expression
            (SpecificExpressionNotComparable
                listElementSpecific
                listElementPattern
                canListElementBeNotComparable
            )
         -> ListExpressionNotComparable listElementSpecific listElementPattern canListElementBeNotComparable
         -> ListExpressionNotComparable listElementSpecific listElementPattern canListElementBeNotComparable
        )
cons =
    { cons = () }
        |> infixOperatorWith (\_ -> Cons)
            (\_ ->
                ConsOperationExpression
                    >> generalWith Listy
                    >> Specific
            )
            (generalWith
                (mapListAppendable anyListElements
                    >> specificAppendableAny
                    >> generalWith identity
                    |> Specific
                    >> Appendably
                )
                |> Specific
            )
