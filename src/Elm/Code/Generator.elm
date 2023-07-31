module Elm.Code.Generator exposing
    ( Generator
    , ExpressionGenerator, forExpression
    , DeclarationGenerator, forDeclaration
    , replaceDescription, replaceWhen, mapChecker, mapDeclaration
    , NameChecker, nameChecker
    , acceptEveryQualifiedName
    , RequireQualified(..), AllowsUnqualified, RequiresQualified, alsoCheckUnqualified, onlyCheckQualified
    , checkNameInContext
    , ArgumentsChecker, argumentsChecker
    )

{-| Create and customize elm generators.

@docs Generator
@docs ExpressionGenerator, forExpression
@docs DeclarationGenerator, forDeclaration
@docs replaceDescription, replaceWhen, mapChecker, mapDeclaration


# checker

@docs NameChecker, nameChecker

@docs acceptEveryQualifiedName
@docs RequireQualified, AllowsUnqualified, RequiresQualified, alsoCheckUnqualified, onlyCheckQualified
@docs checkNameInContext

@docs ArgumentsChecker, argumentsChecker

-}

import Dict exposing (Dict)
import Elm.Code exposing (Code, Expression, ExpressionAny, ModuleScopeDeclarationAny, expressionAny)
import Elm.Syntax.Declaration as Declaration
import Misc exposing (updateFirstChar)
import Parser exposing ((|=), Parser)
import ResultME exposing (ResultME)
import Typed exposing (Checked, Public, Typed, isChecked, tag)


{-| A `DeclarationGenerator` has multiple jobs:

1.  figuring out when and what information we have to generate
2.  generating a [`Declaration`](Elm-Code#Declaration) using this information

You can create a generator with [`forDeclaration`](#forDeclaration)
and customize existing ones with

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceWhen`](#replaceWhen) and [`mapWhen`](#mapWhen)

-}
type alias DeclarationGenerator requireQualified information declaration =
    Generator
        (NameChecker requireQualified information)
        ({ declarationName : String }
         -> information
         -> declaration
        )


{-| An `ExpressionGenerator` has multiple jobs:

1.  figure out when and what information we have to generate
2.  generate an expression using this information

You can create a generator with [`forExpression`](#forExpression)
and customize existing ones with

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceWhen`](#replaceWhen) and [`mapWhen`](#mapWhen)

-}
type alias ExpressionGenerator information =
    Generator
        (ArgumentsChecker information)
        (information
         -> Code { code : ExpressionAny }
        )


{-| A `Generator` has multiple jobs:

1.  figuring out when and what information we have to generate
2.  generating [`Elm.Code`](Elm-Code) using this information

Supported generators:

  - [`ExpressionGenerator`](#ExpressionGenerator)
  - [`DeclarationGenerator`](#DeclarationGenerator)

You can customize existing generators using

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceWhen`](#replaceWhen) and [`mapWhen`](#mapWhen)

-}
type alias Generator when what =
    { description : String
    , when : when
    , what : what
    }



--


{-| Check for names of declarations that don't exist. Use [`nameChecker`] to create one and configure it using

  - default: [`alsoCheckUnqualified`](#alsoCheckUnqualified)
  - [`onlyCheckQualified`](#onlyCheckQualified)

-}
type alias NameChecker requireQualified information =
    { requireQualified :
        Typed Checked requireQualified Public RequireQualified
    , checkName :
        { declarations : Dict String Declaration.Declaration }
        -> String
        -> Maybe (ResultME String information)
    }


{-| Whether every call to the declaration needs to be qualified.
Use [`onlyCheckQualified`](#onlyCheckQualified)/[`alsoCheckUnqualified`](#alsoCheckUnqualified) to configure this for a [`NameChecker`](#NameChecker).
-}
type RequireQualified
    = RequiresQualified
    | AllowsUnqualified


{-| A phantom tag showing that [`RequireQualified`](#RequireQualified) is `RequiresQualified`.
-}
type RequiresQualified
    = -- tag constructor shouldn't be exposed
      RequiresQualifiedTag


{-| A phantom tag showing that [`RequireQualified`](#RequireQualified) is `AllowsUnqualified`.
-}
type AllowsUnqualified
    = -- tag constructor shouldn't be exposed
      AllowsUnqualifiedTag


{-| A [`Checker`] for arguments supplied to `toGenerate description ...`. Use [`argumentsChecker`] to create one.
-}
type alias ArgumentsChecker information =
    List ExpressionAny
    -> ResultME String information


{-| Check for a specific name format: prefix, suffix, infix etc. using [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/).

    import Elm.Generator as Generator exposing (AllowsUnqualified, NameChecker, RequiresQualified)
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Parser exposing ((|.), (|=))
    import Review.Generate

    arrFromNChecker : NameChecker RequiresQualified { n : Int }
    arrFromNChecker =
        Generator.nameChecker
            (Parser.succeed (\n -> { n = n })
                |. Parser.symbol "from"
                |= Parser.int
                |. Parser.end
            )
            |> Generator.onlyCheckQualified

  - [`onlyCheckQualified`](#onlyCheckQualified)

More examples:

    natNPlusChecker : NameChecker AllowsUnqualified { n : Int }
    natNPlusChecker =
        Generator.nameChecker
            (Parser.succeed (\n -> { n = n })
                |. Parser.symbol "Nat"
                |= Parser.int
                |. Parser.symbol "Plus"
                |. Parser.end
            )

    mapFieldHelper : NameChecker AllowsUnqualified { fieldName : String }
    mapFieldHelper =
        RecordFieldHelper.update
            |> Generator.replaceWhen
                (Generator.nameChecker
                    (Parser.succeed (\name -> { fieldName = name })
                        |. Parser.symbol "map"
                        |= (Parser.chompWhile (\_ -> True)
                                |> Parser.getChompedString
                                |> Parser.map String.Extra.decapitalize
                           )
                        |. Parser.end
                    )
                )

-}
nameChecker :
    Parser nameInformation
    -> NameChecker AllowsUnqualified nameInformation
nameChecker nameParser =
    { checkName =
        \_ ->
            Parser.run nameParser
                >> Result.toMaybe
                >> Maybe.map Ok
    , requireQualified =
        tag AllowsUnqualified
            |> isChecked AllowsUnqualifiedTag
    }


{-| If every given name gives us the information to generate a declaration.
If names follow a specific name format and need to be parsed, use [`nameChecker`](#nameChecker)

    import Review.Generate
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Elm.Generator as Generator

    Review.Generate.inModule [ "Field", "Update" ]
        (RecordFieldHelper.update
            |> Generator.replaceWhen
                (Generator.acceptEveryQualifiedName
                    (\name -> { fieldName = name })
                )
        )

-}
acceptEveryQualifiedName :
    (String -> nameInformation)
    -> NameChecker RequiresQualified nameInformation
acceptEveryQualifiedName toNameInformation =
    { checkName = \_ -> toNameInformation >> Ok >> Just
    , requireQualified =
        tag RequiresQualified
            |> isChecked RequiresQualifiedTag
    }



--


{-| Grab some information from all the declarations in this module
using information you already have.

    todo encoder / decoder example

-}
checkNameInContext :
    ({ declarations : Dict String Declaration.Declaration }
     -> information
     -> ResultME String newInformation
    )
    -> NameChecker requireQualified information
    -> NameChecker requireQualified newInformation
checkNameInContext checkContext_ =
    \{ checkName, requireQualified } ->
        { checkName =
            \context ->
                checkName context
                    >> Maybe.map
                        (Result.andThen
                            (checkContext_ context)
                        )
        , requireQualified = requireQualified
        }


{-| Check for a specific name format: prefix, suffix, infix etc. using [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/).

    import Elm.Generator as Generator
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    RecordFieldHelper.update
        |> Generator.mapWhen
            Generator.onlyCheckQualified

If you want to allow unqualified names on a [`NameChecker`](#NameChecker) which does [`onlyCheckQualified`](#onlyCheckQualified) names,
use [`alsoCheckUnqualified`](#alsoCheckUnqualified).

-}
onlyCheckQualified :
    NameChecker requireQualified_ nameInformation
    -> NameChecker RequiresQualified nameInformation
onlyCheckQualified nameChecker_ =
    { checkName = nameChecker_.checkName
    , requireQualified =
        tag RequiresQualified
            |> isChecked RequiresQualifiedTag
    }


{-| If you want to allow unqualified names on a [`NameChecker`](#NameChecker) which does [`onlyCheckQualified`](#onlyCheckQualified) names,
use [`alsoCheckUnqualified`](#alsoCheckUnqualified):

    import Elm.Generator as Generator
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Parser exposing ((|.), (|=))
    import Review.Generate

    arrFromNChecker =
        Generator.nameChecker
            (Parser.succeed (\n -> { n = n })
                |. Parser.symbol "from"
                |= Parser.int
                |. Parser.end
            )
            |> Generator.onlyCheckQualified

    -- fromN will automatically generate Arr.fromN
    fromN =
        arrFromN
            |> Generator.mapWhen
                Generator.alsoCheckUnqualified

-}
alsoCheckUnqualified :
    NameChecker requireQualified_ nameInformation
    -> NameChecker AllowsUnqualified nameInformation
alsoCheckUnqualified nameChecker_ =
    { checkName = nameChecker_.checkName
    , requireQualified =
        tag AllowsUnqualified
            |> isChecked AllowsUnqualifiedTag
    }


{-| Check for specific arguments: strings, numbers etc..
Return `Err message` for unexpected arguments.

    import Elm.Syntax.Expression as Expression
    import Elm.Generator as Generator

    Generator.argumentsChecker
        (\arguments ->
            case arguments of
                [ Expression.Integer lo, Expression.Integer hi ] ->
                    Ok { lowerBound = lo, higherBound = hi }

                _ ->
                    ResultME.error "expected 2 Int arguments"
        )

more examples:

    import Elm.Generator as Generator
    import Elm.Syntax.Expression as Expression
    import SvgParser

    svgChecker =
        Generator.argumentsChecker
            (\arguments ->
                case arguments of
                    [ Expression.Literal svgString ] ->
                        svgString
                            |> SvgParser.parseToNode
                            |> ResultME.fromResult

                    _ ->
                        ResultME.error "expected 1 string argument"
            )

-}
argumentsChecker :
    (List ExpressionAny
     -> ResultME String information
    )
    -> ArgumentsChecker information
argumentsChecker checkArguments =
    checkArguments


{-| Use a different [`NameChecker`](#NameChecker)/[`ArgumentsChecker`](#ArgumentsChecker).
-}
replaceWhen :
    newChecker
    -> Generator checker_ elm
    -> Generator newChecker elm
replaceWhen checker =
    \generator_ ->
        { when = checker
        , description = generator_.description
        , what = generator_.what
        }


{-| Change how to handle checking.
See [`replaceWhen`](#replaceWhen) you need a completely different checker.
-}
mapChecker :
    (checker -> newChecker)
    -> Generator checker elm
    -> Generator newChecker elm
mapChecker alterChecker =
    \generator_ ->
        generator_
            |> replaceWhen
                (alterChecker generator_.when)



--


{-| Create a [`DeclarationGenerator`](#DeclarationGenerator) with a description,
a check for when it should generate, imports and a function that generates an elm [`Declaration`](Elm-Code#Declaration).

You can customize existing generators using

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceWhen`](#replaceWhen)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    import Elm.Generator as Generator exposing (DeclarationGenerator, ExpressionF(..), FunctionDeclaration, RequiresQualified, TypeF(..))
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Parser

    customLens :
        DeclarationGenerator
            RequiresQualified
            { fieldName : String }
            FunctionDeclaration
    customLens =
        Generator.forDeclaration "CustomLens"
            (Generator.nameChecker
                (RecordFieldHelper.fieldNameParserUntil Parser.end)
                |> Generator.requireQualified
            )
            (\{ declarationName } { fieldName } ->
                Generator.valueDeclaration declarationName
                    (let
                        { access, set } =
                            functionsForField fieldName
                     in
                     CallExpression
                        (ValueExpression ( [ "CustomLens" ], "create" ))
                        [ access, set ]
                    )
                    |> Code.withDocumentation
                        (emptyDocComment
                            |> markdown
                                ("`CustomLens` for the field `." ++ fieldName ++ "`.")
                        )
                    |> Code.withAnnotation
                        (Type ( [ "CustomLens" ], "CustomLens" )
                            [ ExtendedRecordType "record"
                                [ ( fieldName, VarType fieldName ) ]
                            , VarType fieldName
                            ]
                        )
                    |> Generator.withImports
                        [ Generator.import_ ( "CustomLens", [] )
                            |> Generator.withExposing
                                [ exposeTypesAndAliases [ "CustomLens" ] ]
                        ]
            )

`customLens` will generate lenses in the form

    import CustomLens exposing (CustomLens)

    {-| `CustomLens` for the field `.score`.
    -}
    score : CustomLens { record | score : score } score
    score =
        CustomLens.create .score (\score_ r -> { r | score = score_ })

-}
forDeclaration :
    String
    -> NameChecker requireQualified information
    ->
        ({ declarationName : String }
         -> information
         -> declaration
        )
    -> DeclarationGenerator requireQualified information declaration
forDeclaration description checker elm =
    { description = description
    , when = checker
    , what =
        \{ declarationName } ->
            elm { declarationName = declarationName }
    }



--
--


{-| Create a [`DeclarationGenerator`](#DeclarationGenerator) with a description,
a check for when it should generate, imports and a function that generates an elm [`Declaration`](Elm-Code#Declaration).

You can customize existing generators using

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceWhen`](#replaceWhen)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    import Elm.Generator as Generator exposing (Generator)
    import Elm.Code as Code

    Generator.forExpression "elm/svg"
        (Generator.argumentsChecker
            (\arguments ->
                case arguments of
                    [ Expression.Literal svgString ] ->
                        svgString |> SvgParser.parseToNode

                    _ ->
                        Err "expected 1 string argument"
            )
        )
        (\svgNode ->
            Generator.expression (svgToElm svgNode)
                |> Code.withImports
                    [ Code.import_ "Svg"
                    , Code.import_ "Svg.Attributes"
                    ]
        )

-}
forExpression :
    String
    -> ArgumentsChecker information
    ->
        (information
         -> Code { code : Expression specific_ }
        )
    -> ExpressionGenerator information
forExpression description checker elm =
    { description = description
    , when = checker
    , what =
        elm >> mapCode expressionAny
    }


mapCode :
    (code -> mappedCode)
    -> Code { code : code }
    -> Code { code : mappedCode }
mapCode changeCode code =
    { imports = code.imports
    , code = code.code |> changeCode
    }


{-| Change the generated elm.

    import Elm.Code as Code exposing (call, exposeTypesAndAliases, val)
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    monocleWithRecordAliasConstructor =
        RecordFieldHelper.monocle
            >> Generator.mapDeclaration
                (\_ { fieldName } ->
                    Code.replaceImplementation
                        (let
                            { access, set } =
                                RecordFieldHelper.functionsForField fieldName
                         in
                         val ( [ "Lens" ], "Lens" )
                            |> (call access >> call set)
                        )
                        >> Code.withImports
                            [ Code.import_ ( "Monocle", [ "Lens" ] )
                                |> Code.withExposing
                                    (exposeTypesAndAliases [ "Lens" ])
                            ]
                )

-}
mapDeclaration :
    ({ declarationName : String }
     -> information
     -> elm
     -> newElm
    )
    -> DeclarationGenerator requireQualified information elm
    -> DeclarationGenerator requireQualified information newElm
mapDeclaration changeElm =
    \generator_ ->
        { description = generator_.description
        , when = generator_.when
        , what =
            \context information ->
                let
                    { declarationName } =
                        context
                in
                generator_.what context information
                    |> changeElm
                        { declarationName = declarationName }
                        information
        }


{-| Update the short description of what the [`Generator`](#Generator) generates.
-}
replaceDescription :
    String
    -> Generator checker elm
    -> Generator checker elm
replaceDescription newDescription =
    \generator_ ->
        { generator_
            | description = newDescription
        }



--
{- An expression plus the imports it uses.

       import Elm.Generator as Generator exposing (ExpressionF(..))
       import Elm.Code as Code

       Generator.expression (Code.accessFun .field "id")

   would generate

       .id

   You can customize existing `Expression`s using

   - [`replaceExpression`](Elm-Code#replaceExpression)
   - [`withImports`](Elm-Code#withImports)

-}
