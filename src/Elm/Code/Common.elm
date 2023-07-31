module Elm.Code.Common exposing
    ( Expressiony(..), Tuply(..), Comparably(..), Appendably(..), Parsery(..)
    , Origin, ModuleOrigin(..), local, from
    , Base(..), Field, field
    , Numbery(..)
    , Expression
    , SpecificExpressionAny, ExpressionAny
    , BoolExpression, BoolLiteralExpression, BoolSpecificExpression, Both2ComparableExpressionAny, Both2ComparableExpressiony, Both2ComparableExpressionyIn, Both2ExpressionAny, Both2Expressiony, Both2ExpressionyIn, CharExpression, CharSpecificExpression, ComparableExpressionAny, ComparablePatternAny, ComparableSpecificPatternAny, ComparableSpecificPatterny, ComparableSpecificPatternyIn, ComparableTuple2Pattern, ComparableTuple2SpecificExpression, ComparableTuple3Pattern, ComparableTuple3SpecificExpression, FloatExpression, FloatSpecificExpression, FunctionExpression, GlslExpression, GlslSpecificExpression, IntExpression, IntSpecificExpression, ListExpressionNotComparable, ListExpressionNotComparableIn, ListLiteralExpressionNotComparable, ParserAdvancedExpression, ParserAdvancedLiteralExpression, ParserAdvancedSpecificExpression, ParserExpression, ParserLiteralExpression, ParserSpecificExpression, RecordExpression, StringExpression, StringExpressionIn, StringLiteralExpression, StringSpecificExpression, Tuple2Expression, Tuple2LiteralExpression, Tuple2SpecificExpression, Tuple3Expression, Tuple3LiteralExpression, Tuple3SpecificExpression, UnionExpression, UnionLiteralExpression, UnionSpecificExpression, UnitExpression, UnitSpecificExpression, UrlParserExpression, UrlParserLiteralExpression, UrlParserSpecificExpression
    , Operator, OperatorApply(..), OperatorDivide(..), OperatorBoolCombine(..), OperatorFunctionCompose(..), OperatorNumberCombine(..), OperatorEqualCheck(..), OperatorCompare(..), OperatorParser(..), OperatorUrlParser(..)
    , GeneralPattern(..), Pattern, PatternAny
    , BoolSpecificPattern, CharSpecificPattern, IntSpecificPattern, ListSpecificPattern, NotDestructurableSpecificPattern, RecordSpecificPattern, StringSpecificPattern, Tuple2SpecificPattern, Tuple3SpecificPattern, UnionSpecificPattern, UnitSpecificPattern
    , TypeAliasDeclaration
    , FunctionSpecificExpression, NumberExpression, RecordSpecificExpression
    )

{-| Types and functions used in multiple modules.


## common

@docs Expressiony, Tuply, Comparably, Appendably, Parsery
@docs ModuleName, Origin, ModuleOrigin, local, from
@docs Base, Field, field


## expression

@docs Numbery

@docs Expression
@docs SpecificExpressionAny, ExpressionAny
@docs BoolExpression, BoolLiteralExpression, BoolSpecificExpression, Both2ComparableExpressionAny, Both2ComparableExpressiony, Both2ComparableExpressionyIn, Both2ExpressionAny, Both2Expressiony, Both2ExpressionyIn, CharExpression, CharSpecificExpression, ComparableExpressionAny, ComparablePatternAny, ComparableSpecificPatternAny, ComparableSpecificPatterny, ComparableSpecificPatternyIn, ComparableTuple2Pattern, ComparableTuple2SpecificExpression, ComparableTuple3Pattern, ComparableTuple3SpecificExpression, FloatExpression, FloatSpecificExpression, FunctionExpression, GlslExpression, GlslSpecificExpression, IntExpression, IntSpecificExpression, ListExpressionNotComparable, ListExpressionNotComparableIn, ListLiteralExpressionNotComparable, ParserAdvancedExpression, ParserAdvancedLiteralExpression, ParserAdvancedSpecificExpression, ParserExpression, ParserLiteralExpression, ParserSpecificExpression, RecordExpression, StringExpression, StringExpressionIn, StringLiteralExpression, StringSpecificExpression, Tuple2Expression, Tuple2LiteralExpression, Tuple2SpecificExpression, Tuple3Expression, Tuple3LiteralExpression, Tuple3SpecificExpression, UnionExpression, UnionLiteralExpression, UnionSpecificExpression, UnitExpression, UnitSpecificExpression, UrlParserExpression, UrlParserLiteralExpression, UrlParserSpecificExpression


## operator

@docs Operator, OperatorApply, OperatorDivide, OperatorBoolCombine, OperatorFunctionCompose, OperatorNumberCombine, OperatorEqualCheck, OperatorCompare, OperatorParser, OperatorUrlParser


## pattern

@docs GeneralPattern, Pattern, PatternAny
@docs BoolSpecificPattern, CharSpecificPattern, IntSpecificPattern, ListSpecificPattern, NotDestructurableSpecificPattern, RecordSpecificPattern, StringSpecificPattern, Tuple2SpecificPattern, Tuple3SpecificPattern, UnionSpecificPattern, UnitSpecificPattern


## declaration

@docs DeclarationAny
@docs TypeDeclaration, FunctionDeclaration, TypeAliasDeclaration

-}

import Elm.Code.Util exposing (Both2, SpecificOrGeneral(..))
import Elm.Code.Util.Generalizable exposing (Generalizable, generalWith, toGeneral)
import Fillable exposing (Empty)
import Possibly exposing (Possibly)
import Stack exposing (StackFilled)


type Expressiony unity chary glsly booly numbery appendably functiony recordy parsery uniony tuply
    = Unity unity
    | Booly booly
    | Chary chary
    | Glsly glsly
    | Numbery numbery
    | Appendably appendably
    | Functiony functiony
    | Recordy recordy
    | Parsery parsery
    | Uniony uniony
    | Tuply tuply


type Tuply tuple2y tuple3y
    = Tuple2y tuple2y
    | Tuple3y tuple3y


type Comparably chary numbery compappendy tuply
    = ComparableChary chary
    | ComparableNumbery numbery
    | Compappendy compappendy
    | ComparableTuply tuply


type Appendably stringy listy
    = Stringy stringy
    | Listy listy


type Parsery parserSimply parserAdvancedy urlParsery
    = ParserSimply parserSimply
    | ParserAdvancedy parserAdvancedy
    | UrlParsery urlParsery



--


type Numbery inty floaty
    = Inty inty
    | Floaty floaty



--


{-| An expression of a [`SpecificExpression`](#SpecificExpression)-specific.

[`ExpressionAny`](#ExpressionAny) covers all possible specifics

Some elm constructs aren't supported (yet) because they aren't supported by
[elm-syntax](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) & [elm-syntax-dsl](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/).
A PR there would be very helpful 💙

  - comments
  - `"""multi-line string"""` as a `MultiLineStringExpression` variant on [`StringLiteralExpression`](#StringLiteralExpression)

-}
type alias Expression specificExpression =
    SpecificOrGeneral
        specificExpression
        GeneralExpression


type alias SpecificExpressionNotComparable possiblyOrNever specificExpression specificPattern =
    ( SpecificOrGeneral
        specificExpression
        (GeneralSpecificExpression
            (Expression
                (SpecificExpressionInNotComparable possiblyOrNever specificExpression specificPattern)
            )
            specificPattern
        )
    , { toAny :
            SpecificOrGeneral
                specificExpression
                (GeneralSpecificExpression
                    (Expression
                        (SpecificExpressionInNotComparable possiblyOrNever specificExpression specificPattern)
                    )
                    specificPattern
                )
            -> SpecificExpressionAny
      , both2ToAny :
            Both2
                (Expression
                    (SpecificExpressionInNotComparable possiblyOrNever specificExpression specificPattern)
                )
            -> Both2ExpressionAny
      , toComparable :
            Empty
                possiblyOrNever
                { any :
                    SpecificOrGeneral
                        specificExpression
                        (GeneralSpecificExpression
                            (Expression
                                (SpecificExpressionInNotComparable possiblyOrNever specificExpression specificPattern)
                            )
                            specificPattern
                        )
                    -> ComparableSpecificExpressionAny
                , both2ToAny :
                    Both2
                        (Expression
                            (SpecificExpressionInNotComparable possiblyOrNever specificExpression specificPattern)
                        )
                    -> Both2ComparableExpressionAny
                }
      }
    )


type SpecificExpressionInNotComparable possiblyOrNever specificExpression specificPattern
    = SpecificExpressionIn (SpecificExpressionNotComparable possiblyOrNever specificExpression specificPattern)


{-| A representation of any possible expression.
-}
type alias ExpressionAny =
    Expression SpecificExpressionAny


type GeneralExpression
    = Named Origin
    | RecordAccess RecordExpression String
    | ApLOperation ( FunctionExpression, ExpressionAny )
    | ApROperation ( ExpressionAny, FunctionExpression )
    | Call FunctionExpression (StackFilled ExpressionAny)


type GeneralSpecificExpression expression patternSpecific
    = Parenthesized expression
    | Let
        (Empty
            Never
            (StackFilled LetDeclarationAny)
        )
        (In expression)
    | IfExpression BoolExpression (Then expression) (Else expression)
    | CaseExpression
        expression
        (Of
            ( List (Case patternSpecific expression)
            , Case (Pattern patternSpecific) expression
            )
        )


{-| One [case](Elm-Code-Expression#case_) match inside a `case`-[`of`](Elm-Code-Expression#of_).
-}
type Case pattern expression
    = Case pattern (In expression)


type Then expressionWhereIsTrue
    = Then expressionWhereIsTrue


type Else expressionWhereIsFalse
    = Else expressionWhereIsFalse


type Of cases
    = Of cases


type In result
    = In result


{-| GLSL shader code.
-}
type GlslLiteralExpression
    = Glsl String


{-| An expression specific that can definitely be called:

  - function literal / anonymous function / lambda (`λ` / `\`)
  - record field access function
  - operator function
  - composed functions
  - record/union constructors with arguments

todo: add result type

-}
type FunctionLiteralExpression
    = RecordAccessFunctionExpression String
    | OperatorFunctionExpression Operator
    | UnnamedFunctionExpression AnonymousFunctionLiteralExpression
    | FunctionCompositionOperationExpression OperatorFunctionCompose (Both2 FunctionExpression)
    | ConstructorFunctionExpression Origin


type alias AnonymousFunctionLiteralExpression =
    ( StackFilled PatternAny, In ExpressionAny )


type RecordLiteralExpression
    = RecordFieldsExpression RecordFieldsLiteralExpression
    | RecordUpdateExpression RecordUpdateLiteralExpression
    | EmptyRecordConstructorExpression Origin


type alias RecordFieldsLiteralExpression =
    List (Field ExpressionAny)


type alias RecordUpdateLiteralExpression =
    ( Origin, StackFilled (Field ExpressionAny) )


type ComparableSpecificExpressionAny
    = ComparableSpecificExpression
        (ComparableSpecificExpressiony
            CharSpecificExpression
            CharSpecificPattern
            IntSpecificExpression
            IntSpecificPattern
            FloatSpecificExpression
            NotDestructurableSpecificPattern
            StringSpecificExpression
            StringSpecificPattern
            ComparableTuple2SpecificExpression
            ComparableSpecificTuple2Pattern
            ComparableTuple3SpecificExpression
            ComparableSpecificTuple3Pattern
        )


type alias ComparableSpecificExpressiony charSpecificExpressiony charSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny =
    Comparably
        charSpecificExpressiony
        (Numbery
            intSpecificExpressiony
            floatSpecificExpressiony
        )
        (Appendably
            stringSpecificExpressiony
            (ComparableSpecificExpressionyIn
                (ListSpecificExpressionNotComparable Never charSpecificExpressiony charSpecificPatterny)
                (ListSpecificPattern charSpecificPatterny)
                (ListSpecificExpressionNotComparable Never intSpecificExpressiony intSpecificPatterny)
                (ListSpecificPattern intSpecificPatterny)
                (ListSpecificExpressionNotComparable Never floatSpecificExpressiony floatSpecificPatterny)
                (ListSpecificPattern floatSpecificPatterny)
                (ListSpecificExpressionNotComparable Never stringSpecificExpressiony stringSpecificPatterny)
                (ListSpecificPattern stringSpecificPatterny)
                (ListSpecificExpressionNotComparable Never tuple2SpecificExpressiony tuple2SpecificPatterny)
                (ListSpecificPattern tuple2SpecificPatterny)
                (ListSpecificExpressionNotComparable Never tuple3SpecificExpressiony tuple3SpecificPatterny)
                (ListSpecificPattern tuple3SpecificPatterny)
            )
        )
        tuple2SpecificExpressiony
        tuple3SpecificExpressiony


type ComparableSpecificExpressionyIn charSpecificExpressiony charSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny
    = ComparableSpecificExpressionIn (ComparableSpecificExpressiony charSpecificExpressiony charSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny)


type alias SpecificNumberExpression specific specificPattern =
    SpecificOrGeneral
        (Generalizable
            (SpecificOrGeneral
                specific
                (SpecificGeneralNumberExpression
                    (SpecificNumberSpecificExpressionIn specific specificPattern)
                )
            )
            NumberSpecificExpressionAny
        )
        ( Int, Base )


type alias SpecificNumberExpressionAny =
    Numbery
        IntSpecificExpression
        FloatSpecificExpression


type NumberSpecificExpressionAny
    = NumberSpecificExpression
        (Numbery
            IntSpecificExpression
            FloatSpecificExpression
        )


type alias NumberExpression specific pattern =
    Expression (SpecificNumberSpecificExpression specific pattern)


type SpecificNumberSpecificExpressionIn specific specificPattern
    = SpecificNumberSpecificExpressionIn (SpecificNumberSpecificExpression specific specificPattern)


type alias SpecificNumberSpecificExpression specific specificPattern =
    SpecificExpressionNotComparable
        Never
        (SpecificNumberExpression specific specificPattern)
        specificPattern


type SpecificGeneralNumberExpression numberExpression
    = NegateExpression numberExpression
    | NumberCombineOperationExpression OperatorNumberCombine (Both2 numberExpression)


type FloatLiteralExpression
    = FloatExpression Float
    | FdivOperationExpression (Both2 FloatExpression)


type IntLiteralExpression
    = IdivOperationExpression (Both2 IntExpression)


type alias SpecificAppendableExpression appendableExpression specific =
    SpecificOrGeneral
        specific
        (AppendableGeneralExpression appendableExpression)


type SpecificExpressionAny
    = SpecificExpressionAny
        (SpecificExpressiony
            UnitSpecificExpression
            UnitSpecificPattern
            BoolSpecificExpression
            BoolSpecificPattern
            CharSpecificExpression
            CharSpecificPattern
            GlslSpecificExpression
            NotDestructurableSpecificPattern
            IntSpecificExpression
            IntSpecificPattern
            FloatSpecificExpression
            NotDestructurableSpecificPattern
            StringSpecificExpression
            StringSpecificPattern
            FunctionSpecificExpression
            NotDestructurableSpecificPattern
            RecordSpecificExpression
            RecordSpecificPattern
            ParserSpecificExpression
            NotDestructurableSpecificPattern
            ParserAdvancedSpecificExpression
            NotDestructurableSpecificPattern
            UrlParserSpecificExpression
            NotDestructurableSpecificPattern
            UnionSpecificExpression
            NotDestructurableSpecificPattern
            Tuple2SpecificExpression
            Tuple2SpecificPattern
            Tuple3SpecificExpression
            Tuple3SpecificPattern
        )


type alias SpecificExpressiony unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny =
    Expressiony
        unitSpecificExpressiony
        boolSpecificExpressiony
        charSpecificExpressiony
        glslSpecificExpressiony
        (Numbery
            intSpecificExpressiony
            floatSpecificExpressiony
        )
        (Appendably
            stringSpecificExpressiony
            (ListSpecificExpressionNotComparablyIn unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny)
        )
        functionSpecificExpressiony
        recordSpecificExpressiony
        (Parsery
            parserSpecificExpressiony
            parserAdvancedSpecificExpressiony
            urlParserSpecificExpressiony
        )
        unionSpecificExpressiony
        (Tuply
            tuple2SpecificExpressiony
            tuple3SpecificExpressiony
        )


type alias ListSpecificExpressionNotComparablyIn unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny =
    SpecificExpressionyIn
        (ListSpecificExpressionNotComparable Possibly unitSpecificExpressiony unitSpecificPatterny)
        (ListSpecificPattern unitSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly boolSpecificExpressiony boolSpecificPatterny)
        (ListSpecificPattern boolSpecificPatterny)
        (ListSpecificExpressionNotComparable Never charSpecificExpressiony charSpecificPatterny)
        (ListSpecificPattern charSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly glslSpecificExpressiony glslSpecificPatterny)
        (ListSpecificPattern glslSpecificPatterny)
        (ListSpecificExpressionNotComparable Never intSpecificExpressiony intSpecificPatterny)
        (ListSpecificPattern intSpecificPatterny)
        (ListSpecificExpressionNotComparable Never floatSpecificExpressiony floatSpecificPatterny)
        (ListSpecificPattern floatSpecificPatterny)
        (ListSpecificExpressionNotComparable Never stringSpecificExpressiony stringSpecificPatterny)
        (ListSpecificPattern stringSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly functionSpecificExpressiony functionSpecificPatterny)
        (ListSpecificPattern functionSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly recordSpecificExpressiony recordSpecificPatterny)
        (ListSpecificPattern recordSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly parserSpecificExpressiony parserSpecificPatterny)
        (ListSpecificPattern parserSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny)
        (ListSpecificPattern parserAdvancedSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly urlParserSpecificExpressiony urlParserSpecificPatterny)
        (ListSpecificPattern urlParserSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly unionSpecificExpressiony unionSpecificPatterny)
        (ListSpecificPattern unionSpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly tuple2SpecificExpressiony tuple2SpecificPatterny)
        (ListSpecificPattern tuple2SpecificPatterny)
        (ListSpecificExpressionNotComparable Possibly tuple3SpecificExpressiony tuple3SpecificPatterny)
        (ListSpecificPattern tuple3SpecificPatterny)


type SpecificExpressionyIn unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny
    = SpecificExpressionyIn (SpecificExpressiony unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny)


type alias SpecificAppendableSpecificExpressionNotComparable possiblyOrNever appendableExpression literal specificPattern =
    SpecificExpressionNotComparable
        possiblyOrNever
        (Generalizable
            (SpecificOrGeneral
                literal
                (AppendableGeneralExpression appendableExpression)
            )
            AppendableSpecificExpressionAny
        )
        specificPattern


type AppendableGeneralExpression appendableExpression
    = AppendOperation (Both2 appendableExpression)


type alias AppendableExpression possiblyOrNever literal pattern =
    Expression
        (SpecificAppendableSpecificExpressionNotComparable
            possiblyOrNever
            (Expression
                (SpecificAppendableSpecificExpressionIn possiblyOrNever literal pattern)
            )
            literal
            pattern
        )


type SpecificAppendableSpecificExpressionIn possiblyOrNever literal pattern
    = SpecificAppendableSpecificIn
        (SpecificAppendableSpecificExpressionNotComparable
            possiblyOrNever
            (Expression
                (SpecificAppendableSpecificExpressionIn literal pattern possiblyOrNever)
            )
            literal
            pattern
        )


type AppendableSpecificExpressionAny
    = AppendableSpecific
        (Appendably
            StringSpecificExpression
            ListSpecificExpressionNotComparableAny
        )


type alias ListSpecificExpressionNotComparableAny =
    ListSpecificExpressionNotComparablyIn
        UnitSpecificExpression
        UnitSpecificPattern
        BoolSpecificExpression
        BoolSpecificPattern
        CharSpecificExpression
        CharSpecificPattern
        GlslSpecificExpression
        NotDestructurableSpecificPattern
        IntSpecificExpression
        IntSpecificPattern
        FloatSpecificExpression
        NotDestructurableSpecificPattern
        StringSpecificExpression
        StringSpecificPattern
        FunctionSpecificExpression
        NotDestructurableSpecificPattern
        RecordSpecificExpression
        RecordSpecificPattern
        ParserSpecificExpression
        NotDestructurableSpecificPattern
        ParserAdvancedSpecificExpression
        NotDestructurableSpecificPattern
        UrlParserSpecificExpression
        NotDestructurableSpecificPattern
        UnionSpecificExpression
        NotDestructurableSpecificPattern
        Tuple2SpecificExpression
        Tuple2SpecificPattern
        Tuple3SpecificExpression
        Tuple3SpecificPattern


type ParserLiteralExpression
    = ParserOperationExpression OperatorParser (Both2 ParserExpression)


type ParserAdvancedLiteralExpression
    = ParserAdvancedOperationExpression OperatorParser (Both2 ParserAdvancedExpression)


type UrlParserLiteralExpression
    = UrlParserOperationExpression OperatorUrlParser (Both2 UrlParserExpression)


type BoolLiteralExpression
    = BoolExpression Bool
    | CombineBoolOperationExpression OperatorBoolCombine (Both2 BoolExpression)
    | CompareOperationExpression OperatorCompare Both2ComparableExpressionAny
    | EqualCheckOperationExpression OperatorEqualCheck Both2ExpressionAny


type Both2ComparableExpressionAny
    = Both2ComparableExpression
        (Both2ComparableExpressiony
            CharSpecificExpression
            CharSpecificPattern
            IntSpecificExpression
            IntSpecificPattern
            FloatSpecificExpression
            NotDestructurableSpecificPattern
            StringSpecificExpression
            StringSpecificPattern
            ComparableTuple2SpecificExpression
            ComparableTuple2SpecificPattern
            ComparableTuple3SpecificExpression
            ComparableTuple3SpecificPattern
        )


type alias ComparableTuple2SpecificExpression =
    SpecificExpressionNotComparable
        Never
        ( ComparableExpressionAny, ComparableExpressionAny )
        ComparableTuple2Pattern


type alias ComparableTuple2Pattern =
    Pattern (SpecificPattern ( ComparablePatternAny, ComparablePatternAny ))


type alias ComparableTuple3SpecificExpression =
    SpecificExpressionNotComparable
        Never
        ( ComparableExpressionAny, ComparableExpressionAny, ComparableExpressionAny )
        ComparableTuple3Pattern


type alias ComparableTuple3Pattern =
    Pattern (SpecificPattern ( ComparablePatternAny, ComparablePatternAny, ComparablePatternAny ))


type alias Both2ComparableExpressiony charSpecificExpressiony charSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny =
    Comparably
        (Both2 (Expression charSpecificExpressiony))
        (Numbery
            (Both2 (Expression intSpecificExpressiony))
            (Both2 (Expression floatSpecificExpressiony))
        )
        (Appendably
            (Both2 (Expression stringSpecificExpressiony))
            (Both2ComparableExpressionyIn
                (ListSpecificExpressionNotComparable Never charSpecificExpressiony charSpecificPatterny)
                (ListSpecificPattern charSpecificPatterny)
                (ListSpecificExpressionNotComparable Never intSpecificExpressiony intSpecificPatterny)
                (ListSpecificPattern intSpecificPatterny)
                (ListSpecificExpressionNotComparable Never floatSpecificExpressiony floatSpecificPatterny)
                (ListSpecificPattern floatSpecificPatterny)
                (ListSpecificExpressionNotComparable Never stringSpecificExpressiony stringSpecificPatterny)
                (ListSpecificPattern stringSpecificPatterny)
                (ListSpecificExpressionNotComparable Never tuple2SpecificExpressiony tuple2SpecificPatterny)
                (ListSpecificPattern tuple2SpecificPatterny)
                (ListSpecificExpressionNotComparable Never tuple3SpecificExpressiony tuple3SpecificPatterny)
                (ListSpecificPattern tuple3SpecificPatterny)
            )
        )
        (Tuply
            (Both2 (Expression tuple2SpecificExpressiony))
            (Both2 (Expression tuple3SpecificExpressiony))
        )


type Both2ComparableExpressionyIn charSpecificExpressiony charSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny
    = Both2ComparableExpressionyIn (Both2ComparableExpressiony charSpecificExpressiony charSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny)


type alias ComparablePatternAny =
    Pattern ComparableSpecificPatternAny


type ComparableSpecificPatternAny
    = ComparableSpecificPattern
        (ComparableSpecificPatterny
            CharSpecificPattern
            IntSpecificPattern
            NotDestructurableSpecificPattern
            StringSpecificPattern
            (SpecificPattern
                ( ComparablePatternAny, ComparablePatternAny )
            )
            (SpecificPattern
                ( ComparablePatternAny, ComparablePatternAny, ComparablePatternAny )
            )
        )


type alias ComparableSpecificPatterny chary inty floaty stringy tuple2y tuple3y =
    Comparably
        chary
        (Numbery
            inty
            floaty
        )
        (Appendably
            stringy
            (ComparableSpecificPatternyIn
                (ListSpecificPattern chary)
                (ListSpecificPattern inty)
                (ListSpecificPattern floaty)
                (ListSpecificPattern stringy)
                (ListSpecificPattern tuple2y)
                (ListSpecificPattern tuple3y)
            )
        )
        (Tuply
            tuple2y
            tuple3y
        )


type ComparableSpecificPatternyIn chary inty floaty stringy tuple2y tuple3y
    = ComparableSpecificPatternyIn (ComparableSpecificPatterny chary inty floaty stringy tuple2y tuple3y)


type Both2ExpressionAny
    = Both2ExpressionAny
        (Both2Expressiony
            UnitExpression
            UnitSpecificPattern
            BoolExpression
            BoolSpecificPattern
            CharExpression
            CharSpecificPattern
            GlslExpression
            NotDestructurableSpecificPattern
            IntExpression
            IntSpecificPattern
            FloatExpression
            NotDestructurableSpecificPattern
            StringExpression
            StringSpecificPattern
            FunctionExpression
            NotDestructurableSpecificPattern
            RecordExpression
            RecordSpecificPattern
            ParserExpression
            NotDestructurableSpecificPattern
            ParserAdvancedExpression
            NotDestructurableSpecificPattern
            UrlParserExpression
            NotDestructurableSpecificPattern
            UnionExpression
            UnionSpecificPattern
            Tuple2Expression
            Tuple2SpecificPattern
            Tuple3Expression
            Tuple3SpecificPattern
        )


type alias Both2Expressiony unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny =
    Expressiony
        (Both2 (Expression unitSpecificExpressiony))
        (Both2 (Expression boolSpecificExpressiony))
        (Both2 (Expression charSpecificExpressiony))
        (Both2 (Expression glslSpecificExpressiony))
        (Numbery
            (Both2 (Expression intSpecificExpressiony))
            (Both2 (Expression floatSpecificExpressiony))
        )
        (Appendably
            (Both2 (Expression stringSpecificExpressiony))
            (Both2ExpressionyIn
                (ListExpressionNotComparable Possibly unitSpecificExpressiony unitSpecificPatterny)
                (ListSpecificPattern (Pattern unitSpecificPatterny))
                (ListExpressionNotComparable Possibly boolSpecificExpressiony boolSpecificPatterny)
                (ListSpecificPattern (Pattern boolSpecificPatterny))
                (ListExpressionNotComparable Never charSpecificExpressiony charSpecificPatterny)
                (ListSpecificPattern (Pattern charSpecificPatterny))
                (ListExpressionNotComparable Possibly glslSpecificExpressiony glslSpecificPatterny)
                (ListSpecificPattern (Pattern glslSpecificPatterny))
                (ListExpressionNotComparable Never intSpecificExpressiony intSpecificPatterny)
                (ListSpecificPattern (Pattern intSpecificPatterny))
                (ListExpressionNotComparable Never floatSpecificExpressiony floatSpecificPatterny)
                (ListSpecificPattern (Pattern floatSpecificPatterny))
                (ListExpressionNotComparable Never stringSpecificExpressiony stringSpecificPatterny)
                (ListSpecificPattern (Pattern stringSpecificPatterny))
                (ListExpressionNotComparable Possibly functionSpecificExpressiony functionSpecificPatterny)
                (ListSpecificPattern (Pattern functionSpecificPatterny))
                (ListExpressionNotComparable Possibly recordSpecificExpressiony recordSpecificPatterny)
                (ListSpecificPattern (Pattern recordSpecificPatterny))
                (ListExpressionNotComparable Possibly parserSpecificExpressiony parserSpecificPatterny)
                (ListSpecificPattern (Pattern parserSpecificPatterny))
                (ListExpressionNotComparable Possibly parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny)
                (ListSpecificPattern (Pattern parserAdvancedSpecificPatterny))
                (ListExpressionNotComparable Possibly urlParserSpecificExpressiony urlParserSpecificPatterny)
                (ListSpecificPattern (Pattern urlParserSpecificPatterny))
                (ListExpressionNotComparable Possibly unionSpecificExpressiony unionSpecificPatterny)
                (ListSpecificPattern (Pattern unionSpecificPatterny))
                (ListExpressionNotComparable Possibly tuple2SpecificExpressiony tuple2SpecificPatterny)
                (ListSpecificPattern (Pattern tuple2SpecificPatterny))
                (ListExpressionNotComparable Possibly tuple3SpecificExpressiony tuple3SpecificPatterny)
                (ListSpecificPattern (Pattern tuple3SpecificPatterny))
            )
        )
        (Both2 (Expression functionSpecificExpressiony))
        (Both2 (Expression recordSpecificExpressiony))
        (Parsery
            (Both2 (Expression parserSpecificExpressiony))
            (Both2 (Expression parserAdvancedSpecificExpressiony))
            (Both2 (Expression urlParserSpecificExpressiony))
        )
        (Both2 (Expression unionSpecificExpressiony))
        (Tuply
            (Both2 (Expression tuple2SpecificExpressiony))
            (Both2 (Expression tuple3SpecificExpressiony))
        )


type Both2ExpressionyIn unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny
    = Both2ExpressionyIn (Both2Expressiony unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny)


type alias ComparableExpressionAny =
    Expression ComparableSpecificExpressionAny


{-| Terminal/Literal expressions that are lists no matter what.
-}
type ListLiteralExpressionNotComparable elementPossiblyOrNever elementSpecific elementPattern
    = ListExpression
        (List
            (Expression
                (SpecificExpressionNotComparable
                    elementPossiblyOrNever
                    elementSpecific
                    elementPattern
                )
            )
        )
    | ConsOperationExpression
        (Expression
            (SpecificExpressionNotComparable
                elementPossiblyOrNever
                elementSpecific
                elementPattern
            )
        )
        (ListExpressionNotComparable
            elementPossiblyOrNever
            elementSpecific
            elementPattern
        )


type alias UnitExpression =
    Expression UnitSpecificExpression


type alias UnitSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        ()
        UnitPattern


type alias BoolExpression =
    Expression BoolSpecificExpression


type alias BoolSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        BoolLiteralExpression
        BoolPattern


type alias CharExpression =
    Expression CharSpecificExpression


type alias CharSpecificExpression =
    SpecificExpressionNotComparable Never Char CharPattern


type alias IntExpression =
    Expression IntSpecificExpression


type alias IntSpecificExpression =
    SpecificNumberSpecificExpression IntLiteralExpression IntPattern


type alias FloatExpression =
    Expression FloatSpecificExpression


type alias FloatSpecificExpression =
    SpecificNumberSpecificExpression FloatLiteralExpression NotDestructurablePattern


type alias GlslExpression =
    Expression GlslSpecificExpression


type alias GlslSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        GlslLiteralExpression
        NotDestructurablePattern


type alias StringExpression =
    Expression StringSpecificExpression


type alias StringSpecificExpression =
    SpecificAppendableSpecificExpressionNotComparable
        Never
        StringExpressionIn
        StringLiteralExpression
        StringPattern


type StringExpressionIn
    = StringExpressionIn StringExpression


{-| Terminal/Literal expressions that are strings no matter what.
-}
type StringLiteralExpression
    = QuotedOnceStringExpression String


type alias FunctionExpression =
    Expression FunctionSpecificExpression


type alias FunctionSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        FunctionLiteralExpression
        NotDestructurablePattern


type alias RecordExpression =
    Expression RecordSpecificExpression


type alias RecordSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        RecordLiteralExpression
        RecordPattern


type alias ParserExpression =
    Expression ParserSpecificExpression


type alias ParserSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        ParserLiteralExpression
        NotDestructurablePattern


type alias ParserAdvancedExpression =
    Expression ParserAdvancedSpecificExpression


type alias ParserAdvancedSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        ParserAdvancedLiteralExpression
        NotDestructurablePattern


type alias UrlParserExpression =
    Expression UrlParserSpecificExpression


type alias UrlParserSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        UrlParserLiteralExpression
        NotDestructurablePattern


type alias UnionExpression =
    Expression UnionSpecificExpression


type alias UnionSpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        UnionLiteralExpression
        UnionPattern


type alias ListExpressionNotComparable elementPossiblyOrNever elementSpecific elementPattern =
    Expression
        (ListSpecificExpressionNotComparable elementPossiblyOrNever elementSpecific elementPattern)


type alias ListSpecificExpressionNotComparable elementPossiblyOrNever elementExpressionSpecific elementPattern =
    SpecificAppendableSpecificExpressionNotComparable
        (ListExpressionNotComparableIn elementExpressionSpecific elementPattern elementPossiblyOrNever)
        (ListLiteralExpressionNotComparable elementExpressionSpecific elementPattern elementPossiblyOrNever)
        (ListPattern elementPattern)
        elementPossiblyOrNever


type ListExpressionNotComparableIn elementPossiblyOrNever elementSpecific elementPattern
    = ListExpressionIn (ListExpressionNotComparable elementPossiblyOrNever elementSpecific elementPattern)


type alias Tuple2Expression =
    Expression Tuple2SpecificExpression


type alias Tuple2SpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        Tuple2LiteralExpression
        Tuple2Pattern


type alias Tuple2LiteralExpression =
    ( ExpressionAny, ExpressionAny )


type alias Tuple3Expression =
    Expression Tuple3SpecificExpression


type alias Tuple3SpecificExpression =
    SpecificExpressionNotComparable
        Possibly
        Tuple3LiteralExpression
        Tuple3Pattern


type alias Tuple3LiteralExpression =
    ( ExpressionAny, ExpressionAny, ExpressionAny )


type UnionLiteralExpression
    = UnionValue Origin



-- operator


{-| A valid infix operator like `+`, `//` or `<<`.
See [`InfixOperatorReference`](#InfixOperatorReference) on how to create one.

All infix operators:

  - `&&`: [`and`](Elm-Code-Operator#and)
  - `||` [`or`](Elm-Code-Operator#or)
  - `//` [`idiv`](Elm-Code-Operator#idiv)
  - `/` [`fdiv`](Elm-Code-Operator#fdiv)
  - `<<` [`composeL`](Elm-Code-Operator#composeL)
  - `>>` [`composeR`](Elm-Code-Operator#composeR)
  - `+`: [`add`](Elm-Code-Operator#add)
  - `-`: [`sub`](Elm-Code-Operator#sub)
  - `*`: [`mul`](Elm-Code-Operator#mul)
  - `^`: [`pow`](Elm-Code-Operator#pow)
  - `++`: [`append`](Elm-Code-Operator#append)
  - `|=`: [`keep`](Elm-Code-Operator#keep)/[`keepAdvanced`](Elm-Code-Operator#keepAdvanced)
  - `</>`: [`slash`](Elm-Code-Operator#slash)
  - `<?>`: [`questionMark`](Elm-Code-Operator#questionMark)
  - `|.`: [`ignore`](Elm-Code-Operator#ignore)/[`ignoreAdvanced`](Elm-Code-Operator#ignoreAdvanced)
  - `==`: [`eq`](Elm-Code-Operator#eq)
  - `/=`: [`neq`](Elm-Code-Operator#neq)
  - `<`: [`lt`](Elm-Code-Operator#lt)
  - `<=`: [`le`](Elm-Code-Operator#le)
  - `>`: [`gt`](Elm-Code-Operator#gt)
  - `>=`: [`ge`](Elm-Code-Operator#ge)
  - `::`: [`cons`](Elm-Code-Operator#cons)
  - `<|`: [`apL`](Elm-Code-Operator#apL)
  - `|>`: [`apR`](Elm-Code-Operator#apR)

-}
type Operator
    = -- ::
      Cons
    | -- ++
      Append
    | OperatorBoolCombine OperatorBoolCombine
    | OperatorDivide OperatorDivide
    | OperatorFunctionCompose OperatorFunctionCompose
    | OperatorNumberCombine OperatorNumberCombine
    | OperatorEqualCheck OperatorEqualCheck
    | OperatorCompare OperatorCompare
    | OperatorApply OperatorApply
    | OperatorParser
        (Parsery
            OperatorParser
            OperatorParser
            OperatorUrlParser
        )


type OperatorDivide
    = -- //
      Idiv
    | -- /
      Fdiv


type OperatorApply
    = -- <|
      ApL
    | -- |>
      ApR


type OperatorBoolCombine
    = -- &&
      And
    | -- ||
      Or


type OperatorCompare
    = -- <
      Lt
    | -- <=
      Le
    | -- >
      Gt
    | -- >=
      Ge


type OperatorEqualCheck
    = -- ==
      Eq
    | -- /=
      Neq


type OperatorFunctionCompose
    = -- <<
      ComposeL
    | -- >>
      ComposeR


type OperatorNumberCombine
    = -- +
      Add
    | -- -
      Sub
    | -- *
      Mul
    | -- ^
      Pow


type OperatorParser
    = -- |=
      Keep
    | -- |.
      Ignore


type OperatorUrlParser
    = -- </>
      Slash
    | -- <?>
      QuestionMark



--


{-| The full identifying name of a declared thing
which is defined in this module or imported from a different module.

    import A exposing (a)
    a -- ← this is defined somewhere else

    b argument =
        argument -- ← this is defined locally

  - `from "Module.Name" "value"`: the `value` defined in `Module.Name`
  - `local "value"`: the`value` which is defined in the same module

todo (?): rename to Named/FullyQualified or sth.

-}
type alias Origin =
    ( ModuleOrigin, String )


{-| Where a thing is defined.
In the example

    module A

    import B exposing (b)

    a =
        ...

`a` is `DeclaredLocally`, while `b` is `DeclaredIn` in `"B"`.

-}
type ModuleOrigin
    = DeclaredLocally
    | DeclaredIn String


{-| A thing is defined is in the same module. See [`ModuleOrigin`](#ModuleOrigin):
In the example

    module A exposing (a)

    import B exposing (b)

    a =
        ...

`a` is defined locally, while `b` is defined in `"B"`.

-}
local : String -> Origin
local name =
    ( DeclaredLocally, name )


{-| A reference to a thing imported from a different module. See [`Origin`](#Origin):

    import A exposing (a)
    a -- ← this is defined somewhere else

    b argument =
        argument -- ← this is defined locally

  - `from "Module.Name" "value"`: the `value` defined in `Module.Name`
  - `local "value"`: the`value` which is defined in the same module

-}
from : String -> String -> Origin
from moduleOrigin name =
    ( DeclaredIn moduleOrigin, name )



--


{-| Supported number systems in which you can create an `Int` in elm.

See [`inBase`](#inBase)

-}
type Base
    = -- ↓ decimal
      Base10
    | -- ↓ hexadecimal
      Base16


type alias Field value =
    ( String, value )


{-| A record field `name = value /: type`.

    import Elm.Code exposing (recordExtendedBy, field, stringType)

    \named ->
        named
            |> recordExtendedBy (field "name" stringType) []

would generate

    : { named | name : String }

-}
field :
    String
    -> Generalizable specific_ general
    -> Field general
field fieldName valueOrType =
    ( fieldName
    , valueOrType |> toGeneral
    )



-- pattern


type GeneralPattern
    = All_
    | Variable String


type alias Pattern specific =
    SpecificOrGeneral
        (Generalizable specific SpecificPatternAny)
        GeneralPattern


type Patterny unity chary booly inty appendably recordy uniony tuply
    = Unity unity
    | Chary chary
    | Booly booly
    | Inty inty
    | Appendably appendably
    | Recordy recordy
    | Uniony uniony
    | Tuply tuply


{-| One of all specifics of pattern specific data.
-}
type SpecificPatternAny
    = SpecificAny
        (SpecificPatternyFor
            UnitSpecificPattern
            CharSpecificPattern
            BoolSpecificPattern
            RecordSpecificPattern
            IntSpecificPattern
            StringSpecificPattern
            UnionSpecificPattern
            Tuple2SpecificPattern
            Tuple3SpecificPattern
        )


type alias SpecificPatternyFor unity chary booly inty stringy recordy uniony tuple2y tuple3y =
    Patterny
        unity
        chary
        booly
        inty
        (Appendably
            stringy
            (ListSpecificPatternyIn unity chary booly inty stringy recordy uniony tuple2y tuple3y)
        )
        recordy
        uniony
        (Tuply tuple2y tuple3y)


type alias ListSpecificPatternyIn unity chary booly inty stringy recordy uniony tuple2y tuple3y =
    SpecificPatternyIn
        (ListSpecificPattern unity)
        (ListSpecificPattern chary)
        (ListSpecificPattern booly)
        (ListSpecificPattern inty)
        (ListSpecificPattern stringy)
        (ListSpecificPattern recordy)
        (ListSpecificPattern uniony)
        (ListSpecificPattern tuple2y)
        (ListSpecificPattern tuple3y)


type SpecificPatternyIn unity chary booly inty stringy recordy uniony tuple2y tuple3y
    = SpecificPatternIn (SpecificPatternyFor unity chary booly inty stringy recordy uniony tuple2y tuple3y)


{-| A pattern of any specific.

See [`SpecificPatternKind`](#SpecificPatternKind) for all possible specific pattern specifics.

-}
type alias PatternAny =
    SpecificOrGeneral SpecificPatternAny GeneralPattern


type alias SpecificPattern specific =
    SpecificOrGeneral
        specific
        (GeneralSpecificPattern (Pattern specific))


type GeneralSpecificPattern pattern
    = AsPattern pattern String


type AppendableSpecificPatternAny
    = AppendableSpecificPattern
        (Appendably
            StringSpecificPattern
            (ListSpecificPatternyIn
                UnitSpecificPattern
                CharSpecificPattern
                BoolSpecificPattern
                IntSpecificPattern
                StringSpecificPattern
                RecordSpecificPattern
                UnionSpecificPattern
                Tuple2SpecificPattern
                Tuple3SpecificPattern
            )
        )


type ListLiteralPattern element
    = UnConsPattern element (ListPattern element)
    | ListElementsPattern (List element)


type alias NotDestructurablePattern =
    Pattern NotDestructurableSpecificPattern


type alias NotDestructurableSpecificPattern =
    SpecificPattern Never


type alias UnitPattern =
    Pattern UnitSpecificPattern


type alias UnitSpecificPattern =
    SpecificPattern ()


type alias BoolPattern =
    Pattern BoolSpecificPattern


type alias BoolSpecificPattern =
    SpecificPattern Bool


type alias CharPattern =
    Pattern CharSpecificPattern


type alias CharSpecificPattern =
    SpecificPattern Char


type alias IntPattern =
    Pattern IntSpecificPattern


type alias IntSpecificPattern =
    SpecificPattern ( Int, Base )


type alias StringPattern =
    Pattern StringSpecificPattern


type alias StringSpecificPattern =
    SpecificPattern String


type alias RecordPattern =
    Pattern RecordSpecificPattern


type alias RecordSpecificPattern =
    SpecificPattern (List String)


type alias UnionPattern =
    Pattern UnionSpecificPattern


type alias UnionSpecificPattern =
    SpecificPattern UnionLiteralPattern


type alias UnionLiteralPattern =
    ( Origin, List PatternAny )


type alias ListPattern element =
    Pattern (ListSpecificPattern element)


type alias ListSpecificPattern element =
    SpecificPattern (ListLiteralPattern element)


type alias Tuple2Pattern =
    Pattern Tuple2SpecificPattern


type alias Tuple2SpecificPattern =
    SpecificPattern Tuple2LiteralPattern


type alias Tuple2LiteralPattern =
    ( PatternAny, PatternAny )


type alias Tuple3Pattern =
    Pattern Tuple3SpecificPattern


type alias Tuple3SpecificPattern =
    SpecificPattern Tuple3LiteralPattern


type alias Tuple3LiteralPattern =
    ( PatternAny, PatternAny, PatternAny )



--


type alias Documentable documentable =
    { documentable
        | documentation : Maybe DocumentationComment
    }


{-| A module-scope / top-level declaration. Either

  - [`ExpressionDeclaration`](#ExpressionDeclaration)
  - [`UnionTypeDeclaration`](#UnionTypeDeclaration)
  - [`TypeAliasDeclaration`](#TypeAliasDeclaration)
  - [`PortDeclaration`](#PortDeclaration)

-}
type ModuleScopeDeclarationAny
    = VariableModuleScopeDeclaration FunctionOrValueModuleScopeDeclaration
    | TypeAliasDeclaration TypeAliasDeclaration
    | UnionTypeDeclaration UnionTypeDeclaration
    | PortDeclaration PortDeclaration


{-| All the components to build a function declaration:

    {-| [documentation]
    -}
    [name] : [type_]
    [name] [arguments] =
        [expression]

You can customize existing `FunctionDeclaration`s using

  - [`replaceExpression`](#replaceExpression) and [`replaceExpressionAndArguments`](#replaceExpressionAndArguments)
  - [`withDocumentation`](#withDocumentation)
  - [`withType`](#withType)
  - [`withImports`](#withImports)

or create custom helper with [`functionDeclaration`](#functionDeclaration).

-}
type alias FunctionOrValueModuleScopeDeclaration =
    Documentable
        { name : String
        , type_ : Maybe TypeAny
        , parameters : List PatternAny
        , expression : ExpressionAny
        }


{-| The components to build a `type alias`:

    {-| [documentation]
    -}
    type alias [Name] [arguments] =
        [aliasedType]

Customize:

  - [`withDocumentation`](#withDocumentation)
  - [`replaceAliasedType`](#replaceAliasedType) and [`replaceAliasedTypeAndArguments`](#replaceAliasedTypeAndArguments)
  - [`withImports`](#withImports)

Create a `type alias` declaration using [`typeAliasDeclaration`](#typeAliasDeclaration).

-}
type alias TypeAliasDeclaration =
    Documentable
        { name : String
        , parameters : List String
        , aliasedType : TypeAny
        }


{-| The components to build a `type` declaration:

    {-| [documentation]
    -}
    type [Name] [arguments]
        = [Constructors]

-}
type alias UnionTypeDeclaration =
    Documentable
        { name : String
        , parameters : List String
        , variants :
            Empty
                Never
                (StackFilled ( String, List TypeAny ))
        }


{-| The components to build a `port`:

    [name] : [type_]

Customize:

  - [`replaceType`](#replaceType)
  - [`withImports`](#withImports)

Create a `port` declaration using [`portDeclaration`](#portDeclaration).

-}
type alias PortDeclaration =
    RecordWithoutConstructorFunction
        { name : String
        , type_ : TypeAny
        }



--


{-| A declaration in a let block.
-}
type LetDeclarationAny
    = LetAny
        (LetDeclarationy
            UnitSpecificExpression
            UnitSpecificPattern
            BoolSpecificExpression
            BoolSpecificPattern
            CharSpecificExpression
            CharSpecificPattern
            GlslSpecificExpression
            NotDestructurableSpecificPattern
            IntSpecificExpression
            IntSpecificPattern
            FloatSpecificExpression
            NotDestructurableSpecificPattern
            StringSpecificExpression
            StringSpecificPattern
            FunctionSpecificExpression
            NotDestructurableSpecificPattern
            RecordSpecificExpression
            RecordSpecificPattern
            ParserSpecificExpression
            NotDestructurableSpecificPattern
            ParserAdvancedSpecificExpression
            NotDestructurableSpecificPattern
            UrlParserSpecificExpression
            NotDestructurableSpecificPattern
            UnionSpecificExpression
            UnionSpecificPattern
            Tuple2SpecificExpression
            Tuple2SpecificPattern
            Tuple3SpecificExpression
            Tuple3SpecificPattern
        )


type alias LetDeclarationy unitSpecificExpressiony unitSpecificPatterny boolSpecificExpressiony boolSpecificPatterny charSpecificExpressiony charSpecificPatterny glslSpecificExpressiony glslSpecificPatterny intSpecificExpressiony intSpecificPatterny floatSpecificExpressiony floatSpecificPatterny stringSpecificExpressiony stringSpecificPatterny functionSpecificExpressiony functionSpecificPatterny recordSpecificExpressiony recordSpecificPatterny parserSpecificExpressiony parserSpecificPatterny parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny urlParserSpecificExpressiony urlParserSpecificPatterny unionSpecificExpressiony unionSpecificPatterny tuple2SpecificExpressiony tuple2SpecificPatterny tuple3SpecificExpressiony tuple3SpecificPatterny =
    Expressiony
        (LetDeclarationForSpecific unitSpecificExpressiony unitSpecificPatterny)
        (LetDeclarationForSpecific boolSpecificExpressiony boolSpecificPatterny)
        (LetDeclarationForSpecific charSpecificExpressiony charSpecificPatterny)
        (LetDeclarationForSpecific glslSpecificExpressiony glslSpecificPatterny)
        (Numbery
            (LetDeclarationForSpecific intSpecificExpressiony intSpecificPatterny)
            (LetDeclarationForSpecific floatSpecificExpressiony floatSpecificPatterny)
        )
        (Appendably
            (LetDeclarationForSpecific stringSpecificExpressiony stringSpecificPatterny)
            (LetDeclarationyIn
                (ListSpecificExpressionNotComparable Possibly unitSpecificExpressiony unitSpecificPatterny)
                (ListSpecificPattern (Pattern unitSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly boolSpecificExpressiony boolSpecificPatterny)
                (ListSpecificPattern (Pattern boolSpecificPatterny))
                (ListSpecificExpressionNotComparable Never charSpecificExpressiony charSpecificPatterny)
                (ListSpecificPattern (Pattern charSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly glslSpecificExpressiony glslSpecificPatterny)
                (ListSpecificPattern (Pattern glslSpecificPatterny))
                (ListSpecificExpressionNotComparable Never intSpecificExpressiony intSpecificPatterny)
                (ListSpecificPattern (Pattern intSpecificPatterny))
                (ListSpecificExpressionNotComparable Never floatSpecificExpressiony floatSpecificPatterny)
                (ListSpecificPattern (Pattern floatSpecificPatterny))
                (ListSpecificExpressionNotComparable Never stringSpecificExpressiony stringSpecificPatterny)
                (ListSpecificPattern (Pattern stringSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly functionSpecificExpressiony functionSpecificPatterny)
                (ListSpecificPattern (Pattern functionSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly recordSpecificExpressiony recordSpecificPatterny)
                (ListSpecificPattern (Pattern recordSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly parserSpecificExpressiony parserSpecificPatterny)
                (ListSpecificPattern (Pattern parserSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny)
                (ListSpecificPattern (Pattern parserAdvancedSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly urlParserSpecificExpressiony urlParserSpecificPatterny)
                (ListSpecificPattern (Pattern urlParserSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly unionSpecificExpressiony unionSpecificPatterny)
                (ListSpecificPattern (Pattern unionSpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly tuple2SpecificExpressiony tuple2SpecificPatterny)
                (ListSpecificPattern (Pattern tuple2SpecificPatterny))
                (ListSpecificExpressionNotComparable Possibly tuple3SpecificExpressiony tuple3SpecificPatterny)
                (ListSpecificPattern (Pattern tuple3SpecificPatterny))
            )
        )
        (LetDeclarationForSpecific functionSpecificExpressiony functionSpecificPatterny)
        (LetDeclarationForSpecific recordSpecificExpressiony recordSpecificPatterny)
        (Parsery
            (LetDeclarationForSpecific parserSpecificExpressiony parserSpecificPatterny)
            (LetDeclarationForSpecific parserAdvancedSpecificExpressiony parserAdvancedSpecificPatterny)
            (LetDeclarationForSpecific urlParserSpecificExpressiony urlParserSpecificPatterny)
        )
        (LetDeclarationForSpecific unionSpecificExpressiony unionSpecificPatterny)
        (Tuply
            (LetDeclarationForSpecific tuple2SpecificExpressiony tuple2SpecificPatterny)
            (LetDeclarationForSpecific tuple3SpecificExpressiony tuple3SpecificPatterny)
        )


type LetDeclarationyIn unitSpecificExpressiony unitPatterny boolSpecificExpressiony boolPatterny charSpecificExpressiony charPatterny glslSpecificExpressiony glslPatterny intSpecificExpressiony intPatterny floatSpecificExpressiony floatPatterny stringSpecificExpressiony stringPatterny functionSpecificExpressiony functionPatterny recordSpecificExpressiony recordPatterny parserSpecificExpressiony parserPatterny parserAdvancedSpecificExpressiony parserAdvancedPatterny urlParserSpecificExpressiony urlParserPatterny unionSpecificExpressiony unionPatterny tuple2SpecificExpressiony tuple2Patterny tuple3SpecificExpressiony tuple3Patterny
    = LetDeclarationIn (LetDeclarationy unitSpecificExpressiony unitPatterny boolSpecificExpressiony boolPatterny charSpecificExpressiony charPatterny glslSpecificExpressiony glslPatterny intSpecificExpressiony intPatterny floatSpecificExpressiony floatPatterny stringSpecificExpressiony stringPatterny functionSpecificExpressiony functionPatterny recordSpecificExpressiony recordPatterny parserSpecificExpressiony parserPatterny parserAdvancedSpecificExpressiony parserAdvancedPatterny urlParserSpecificExpressiony urlParserPatterny unionSpecificExpressiony unionPatterny tuple2SpecificExpressiony tuple2Patterny tuple3SpecificExpressiony tuple3Patterny)


type LetDeclaration expression pattern
    = DeclarationLetFunctionOrValue (DeclarationFunctionOrValueUndocumented expression)
    | DestructuringLetDeclaration (DestructuringLetDeclaration pattern expression)


type alias DeclarationFunctionOrValueUndocumented expression =
    RecordWithoutConstructorFunction
        { name : String
        , type_ : Maybe TypeAny
        , arguments : List PatternAny
        , result : expression
        }


type alias DestructuringLetDeclaration pattern expression =
    RecordWithoutConstructorFunction
        { pattern : pattern, expression : expression }


type alias LetDeclarationForSpecific specificExpression specificPattern =
    LetDeclaration (Expression specificExpression) (Pattern specificPattern)



--


type alias DocumentationComment =
    List DocumentationBlock


type DocumentationBlock
    = MarkdownDocumentationBlock (List String)
    | ElmCodeDocumentationBlock DocumentationCodeBlock


type alias DocumentationCodeBlock =
    Code
        { moduleHeader : Maybe ModuleHeader
        , implicitImports : Imports
        , snippetsBottomToTop : List DocumentationCodeSnippet
        }


type DocumentationCodeSnippet
    = ExpressionPrintable ExpressionAny
    | DeclarationPrintable ModuleScopeDeclarationAny
    | CustomPrintable String
