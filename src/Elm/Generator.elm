module Elm.Generator exposing
    ( Generator
    , ExpressionGenerator, forExpression
    , DeclarationGenerator, forDeclaration
    , replaceDescription, replaceChecker, updateChecker, mapElm
    , withImports, Imports, ModuleName
    , Import, importModule, withImportAlias, withoutImportAlias, withExposingAll, withExposing, withoutExposing
    , NameChecker, nameChecker, acceptEveryQualifiedName
    , RequireQualified(..), AllowsUnqualified, RequiresQualified, onlyCheckQualified, alsoCheckUnqualified
    , ArgumentsChecker, argumentsChecker
    , Code, ReferenceLookup
    , replaceExpression
    , Expression, expression
    , Declaration
    , withDocumentation
    , FunctionDeclaration, functionDeclaration, valueDeclaration
    , withAnnotation, replaceExpressionAndArguments
    , TypeAliasDeclaration, typeAliasDeclaration
    , replaceAliasedType, replaceAliasedTypeAndArguments
    , TypeDeclaration, typeDeclaration
    , replaceConstructors, replaceConstructorsAndArguments
    , printUsingSpecifiedImports
    , Generalizable, generalizable
    , updateSpecific, toGeneral
    , Printable, print
    )

{-| Create and customize elm generators.

@docs Generator
@docs ExpressionGenerator, forExpression
@docs DeclarationGenerator, forDeclaration
@docs replaceDescription, replaceChecker, updateChecker, mapElm
@docs withImports, Imports, ModuleName


## import

@docs Import, importModule, withImportAlias, withoutImportAlias, withExposingAll, withExposing, withoutExposing


# checker

@docs NameChecker, nameChecker, acceptEveryQualifiedName
@docs RequireQualified, AllowsUnqualified, RequiresQualified, onlyCheckQualified, alsoCheckUnqualified
@docs ArgumentsChecker, argumentsChecker


# elm

@docs Code, ReferenceLookup
@docs replaceExpression


## expression code

@docs Expression, expression


## declaration

@docs Declaration
@docs withDocumentation


### function/value

@docs FunctionDeclaration, functionDeclaration, valueDeclaration
@docs withAnnotation, replaceExpressionAndArguments


### `type alias`

@docs TypeAliasDeclaration, typeAliasDeclaration
@docs replaceAliasedType, replaceAliasedTypeAndArguments


### `type`

@docs TypeDeclaration, typeDeclaration
@docs replaceConstructors, replaceConstructorsAndArguments


# extra


## test

@docs printUsingSpecifiedImports


## `Generalize`

@docs Generalizable, generalizable
@docs updateSpecific, toGeneral
@docs Printable, print

-}

import Dict exposing (Dict)
import Elm.CodeGen as CodeGen exposing (ModuleName)
import Elm.Pretty as CodeGen
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing ((|=), Parser)
import Pretty
import SyntaxExtra exposing (joinAndSortImports, nameOfExpose, printPretty)
import Typed exposing (Checked, Public, Typed, isChecked, tag)
import Util exposing (fromNonempty)


{-| [`Elm.Syntax.ModuleName`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-ModuleName#ModuleName)
and [`Elm.CodeGen.ModuleName`](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest/Elm-CodeGen#ModuleName)
describe an optional module name.
This will change in the next `elm-syntax` version: [issue about `ModuleName`](https://github.com/stil4m/elm-syntax/issues/70)

Until then, we'll just use this :)

-}
type alias ModuleName =
    ( String, List String )


{-| A `DeclarationGenerator` has multiple jobs:

1.  figuring out when and what information we have to generate
2.  generating a [`Declaration`](#Declaration) using this information with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest)

You can create a generator with [`forDeclaration`](#forDeclaration)
and customize existing ones with

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceChecker`](#replaceChecker) and [`updateChecker`](#updateChecker)

-}
type alias DeclarationGenerator requireQualified information declaration =
    Generator
        (NameChecker requireQualified information)
        information
        declaration


{-| An `ExpressionGenerator` has multiple jobs:

1.  figuring out when and what information we have to generate
2.  generating an [`Expression`](#Expression) using this information with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest)

You can create a generator with [`forExpression`](#forExpression)
and customize existing ones with

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceChecker`](#replaceChecker) and [`updateChecker`](#updateChecker)

-}
type alias ExpressionGenerator information expression =
    Generator (ArgumentsChecker information) information expression


{-| A `Generator` has multiple jobs:

1.  figuring out when and what information we have to generate
2.  generating elm using this information with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest)

Supported generators: [`ExpressionGenerator`] and [`DeclarationGenerator`].

You can customize existing generators using

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceChecker`](#replaceChecker) and [`updateChecker`](#updateChecker)

-}
type alias Generator checker information elm =
    { description : String
    , checker : checker
    , elm :
        { declarationName : String
        , imports : Imports
        }
        -> information
        -> elm
    }


{-| Check for names of declarations that don't exist. Use [`nameChecker`] to create one and configure it using

  - [`onlyCheckQualified`](#onlyCheckQualified)/[`alsoCheckUnqualified`](#alsoCheckUnqualified)

-}
type alias NameChecker requireQualified information =
    { requireQualified :
        Typed Checked requireQualified Public RequireQualified
    , nameParser : Parser information
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
    List CodeGen.Expression
    -> Result String information


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
            |> Generator.replaceChecker
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
    { nameParser = nameParser
    , requireQualified =
        tag AllowsUnqualified
            |> isChecked AllowsUnqualifiedTag
    }


{-| If every given name gives us the information to generate a declaration.
If names follow a specific name format and need to be parsed, use [`nameChecker`](#nameChecker)

    import Review.Generate
    import  Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Elm.Generator as Generator

    Review.Generate.inModule [ "Field", "Update" ]
        (RecordFieldHelper.update
            |> Generator.replaceChecker
                (Generator.acceptEveryQualifiedName
                    (\name -> { fieldName = name })
                )
        )

-}
acceptEveryQualifiedName :
    (String -> nameInformation)
    -> NameChecker RequiresQualified nameInformation
acceptEveryQualifiedName nameInformation =
    { nameParser =
        Parser.succeed nameInformation
            |= (Parser.chompWhile (\_ -> True)
                    |> Parser.getChompedString
               )
    , requireQualified =
        tag RequiresQualified
            |> isChecked RequiresQualifiedTag
    }


{-| Check for a specific name format: prefix, suffix, infix etc. using [`elm/parser`](https://package.elm-lang.org/packages/elm/parser/latest/).

    import Elm.Generator as Generator
    import Parser exposing ((|.), (|=))
    import Review.Generate
    import  Elm.Generator.RecordFieldHelper as RecordFieldHelper

    Review.Generate.inModule [ "Field", "Update" ]
        (RecordFieldHelper.update
            |> Generator.replaceChecker
                (Generator.nameChecker
                    (Parser.succeed (\name -> { fieldName = name })
                        |= Parser.getSource
                    )
                    |> Generator.onlyCheckQualified
                )
        )

If you want to allow unqualified names on a [`NameChecker`](#NameChecker) which does [`onlyCheckQualified`](#onlyCheckQualified) names,
use [`alsoCheckUnqualified`](#alsoCheckUnqualified).

-}
onlyCheckQualified :
    NameChecker requireQualified_ nameInformation
    -> NameChecker RequiresQualified nameInformation
onlyCheckQualified nameChecker_ =
    { nameParser = nameChecker_.nameParser
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
            |> Generator.updateChecker
                Generator.alsoCheckUnqualified

-}
alsoCheckUnqualified :
    NameChecker requireQualified_ nameInformation
    -> NameChecker AllowsUnqualified nameInformation
alsoCheckUnqualified nameChecker_ =
    { nameParser = nameChecker_.nameParser
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
                    Err "expected 2 Int arguments"
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
                        svgString |> SvgParser.parseToNode

                    _ ->
                        Err "expected 1 string argument"
            )

-}
argumentsChecker :
    (List CodeGen.Expression
     -> Result String information
    )
    -> ArgumentsChecker information
argumentsChecker checkArguments =
    checkArguments


{-| Use a different [`NameChecker`](#NameChecker)/[`ArgumentsChecker`](#ArgumentsChecker).
-}
replaceChecker :
    newChecker
    -> Generator checker_ information elm
    -> Generator newChecker information elm
replaceChecker checker =
    \generator_ ->
        { checker = checker
        , description = generator_.description
        , elm = generator_.elm
        }


{-| Change how to handle checking.
See [`replaceChecker`](#replaceChecker) you need a completely different checker.
-}
updateChecker :
    (checker -> newChecker)
    -> Generator checker information elm
    -> Generator newChecker information elm
updateChecker alterChecker =
    \generator_ ->
        generator_
            |> replaceChecker
                (alterChecker generator_.checker)



--


{-| Create a [`DeclarationGenerator`](#DeclarationGenerator) with a description,
a check for when it should generate, imports and a function that generates an elm [`Declaration`](#Declaration).

You can customize existing generators using

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceChecker`](#replaceChecker)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    import Elm.Generator as Generator exposing (DeclarationGenerator, FunctionDeclaration, RequiresQualified)
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    customLens :
        DeclarationGenerator
            RequiresQualified
            { fieldName : String }
            FunctionDeclaration
    customLens =
        Generator.forDeclaration "CustomLens"
            (Generator.nameChecker
                (Parser.succeed (\name -> { fieldName = name })
                    |= Parser.getSource
                )
                |> Generator.requireQualified
            )
            (\{ fq, declarationName } { fieldName } ->
                Generator.valueDeclaration declarationName
                    (let
                        { access, set } =
                            functionsForField fieldName
                     in
                     fq.construct ( "CustomLens", [] ) "create" [ access, set ]
                    )
                    |> Generator.withDocumentation
                        (emptyDocComment
                            |> markdown
                                ("`CustomLens` for the field `." ++ fieldName ++ "`.")
                        )
                    |> Generator.withAnnotation
                        (fq.typed
                            ( "CustomLens", [] )
                            "CustomLens"
                            [ extRecordAnn "record"
                                [ ( fieldName, typeVar fieldName ) ]
                            , typeVar fieldName
                            ]
                        )
                    |> Generator.withImports
                        [ Generator.importModule ( "CustomLens", [] )
                            |> Generator.withExposing
                                [ typeOrAliasExpose "CustomLens" ]
                        ]
            )

`customLens` will generate lenses in the form

    import CustomLens exposing (CustomLens)

    {-| `CustomLens` for the field `.score`.
    -}
    score : CustomLens { record | score : score } score
    score =
        CustomLens.create .score (\score_ r -> { r | score = score_ })

**What's `fq`?** → [`ReferenceLookup`](#ReferenceLookup).

-}
forDeclaration :
    String
    -> NameChecker requireQualified information
    ->
        ({ declarationName : String
         , fq : ReferenceLookup
         }
         -> information
         -> declaration
        )
    -> DeclarationGenerator requireQualified information declaration
forDeclaration description checker elm =
    { description = description
    , checker = checker
    , elm =
        \{ declarationName, imports } ->
            elm
                { declarationName = declarationName
                , fq = referenceLookup imports
                }
    }


{-| **What's a `ReferenceLookup`?**

The final imports might be different from what you expect.

  - Anyone can specify different imports with [`withImports`](#withImports)
  - The module it generates in could already have imports with a different alias etc.

Use a `ReferenceLookup` to look in the actual imports for your declaration
so that it can use the correct form (with module alias, no module alias, unqualified).

    import Elm.Generator as Generator
    import  Elm.Generator.RecordFieldHelper as RecordFieldHelper

    Generator.forDeclaration "CustomLens"
        ...
        (\{ fq, declarationName } { fieldName } ->
            Generator.valueDeclaration declarationName
                (let
                    { access, set } =
                        RecordFieldHelper.functionsForField fieldName
                    in
                    fq.construct ( "CustomLens", [] )
                        "create"
                        [ access, set ]
                )
        )

To make sure that you always use `fq.val` instead of `Elm.CodeGen.fqVal` for example,
add this elm-review rule to your config:

    NoFunctionOutsideOfModules.rule
        [ ( [ "Elm.CodeGen.fqVal"
            , "Elm.CodeGen.fqFun"
            , "Elm.CodeGen.fqConstruct"
            , "Elm.CodeGen.fqNamedPattern"
            , "Elm.CodeGen.fqTyped"
            ]
          , []
          )
        ]

using [NeoVier's elm-review-no-function-outside-of-modules](https://package.elm-lang.org/packages/NeoVier/elm-review-no-function-outside-of-modules/latest/)

-}
type alias ReferenceLookup =
    { val :
        ModuleName
        -> String
        -> CodeGen.Expression
    , construct :
        ModuleName
        -> String
        -> List CodeGen.Expression
        -> CodeGen.Expression
    , namedPattern :
        ModuleName
        -> String
        -> List CodeGen.Pattern
        -> CodeGen.Pattern
    , typed :
        ModuleName
        -> String
        -> List CodeGen.TypeAnnotation
        -> CodeGen.TypeAnnotation
    }


{-| Create a [`ReferenceLookup`](#ReferenceLookup) from [`Imports`](#Imports).
-}
referenceLookup : Imports -> ReferenceLookup
referenceLookup imports =
    let
        qualification moduleName name =
            case imports |> Dict.get moduleName of
                Just { alias, exposed } ->
                    let
                        moduleNameOrAlias : () -> List String
                        moduleNameOrAlias () =
                            case alias of
                                Just hasAlias ->
                                    [ hasAlias ]

                                Nothing ->
                                    moduleName |> fromNonempty
                    in
                    case exposed of
                        Just (Exposing.All _) ->
                            []

                        Just (Exposing.Explicit list) ->
                            if
                                list
                                    |> List.any
                                        (\(Node _ expose) ->
                                            nameOfExpose expose == name
                                        )
                            then
                                []

                            else
                                moduleNameOrAlias ()

                        Nothing ->
                            moduleNameOrAlias ()

                Nothing ->
                    moduleName |> fromNonempty

        fq : (List String -> String -> fq) -> ModuleName -> String -> fq
        fq fqCode moduleName name =
            fqCode (qualification moduleName name) name
    in
    { val = fq CodeGen.fqVal
    , construct = fq CodeGen.fqConstruct
    , namedPattern = fq CodeGen.fqNamedPattern
    , typed = fq CodeGen.fqTyped
    }


{-| Create a [`DeclarationGenerator`](#DeclarationGenerator) with a description,
a check for when it should generate, imports and a function that generates an elm [`Declaration`](#Declaration).

You can customize existing generators using

  - [`mapElm`](#mapElm)
  - [`replaceDescription`](#replaceDescription)
  - [`replaceChecker`](#replaceChecker)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    import Elm.Generator as Generator exposing (Generator)

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
        (\{ fq } svgNode ->
            Generator.expression
                (svgToElm { fq = fq } svgNode)
                |> Generator.withImports
                    [ Generator.importModule ( "Svg", [] )
                    , Generator.importModule ( "Svg", [ "Attribute" ] )
                    ]
        )

**What's `fq`?** → See [`ReferenceLookup`](#ReferenceLookup).

-}
forExpression :
    String
    -> ArgumentsChecker information
    ->
        ({ declarationName : String
         , fq : ReferenceLookup
         }
         -> information
         -> declaration
        )
    -> ExpressionGenerator information declaration
forExpression description checker elm =
    { description = description
    , checker = checker
    , elm =
        \{ declarationName, imports } ->
            elm
                { declarationName = declarationName
                , fq = referenceLookup imports
                }
    }


{-| Change the generated elm.

    import Elm.Generator as Generator
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    monocleWithRecordAliasConstructor =
        RecordFieldHelper.monocle
            >> Generator.mapElm
                (\{ fq } { fieldName } ->
                    Generator.replaceImplementation
                        (let
                            { access, set } =
                                RecordFieldHelper.functionsForField fieldName
                         in
                         fq.construct ( "Lens", [] ) "Lens" [ access, set ]
                        )
                        >> Generator.withImports
                            [ Generator.importModule
                                ( "Monocle", [ "Lens" ] )
                                |> Generator.withExposing
                                    [ typeOrAliasExpose "Lens" ]
                            ]
                )

**What's `fq`?** → See [`ReferenceLookup`](#ReferenceLookup).

-}
mapElm :
    ({ declarationName : String
     , fq : ReferenceLookup
     }
     -> information
     -> elm
     -> newElm
    )
    -> Generator checker information elm
    -> Generator checker information newElm
mapElm changeElm =
    \generator_ ->
        { description = generator_.description
        , checker = generator_.checker
        , elm =
            \context information ->
                let
                    { declarationName, imports } =
                        context
                in
                generator_.elm context information
                    |> changeElm
                        { declarationName = declarationName
                        , fq = referenceLookup imports
                        }
                        information
        }


{-| Update the short description of what the [`Generator`](#Generator) generates.
-}
replaceDescription :
    String
    -> Generator checker information elm
    -> Generator checker information elm
replaceDescription newDescription =
    \generator_ ->
        { generator_
            | description = newDescription
        }



--


{-| [`Import`](#Import)s by imported module name.
-}
type alias Imports =
    Dict
        ModuleName
        { alias : Maybe String
        , exposed : Maybe CodeGen.Exposing
        }


{-| The components to build an import statement:

    import [Module.Name] [as Alias] [exposing (exposed)]

example:

    import Elm.Generator as Generator

    Generator.importModule ( "Array", [ "Extra" ] )
        |> Generator.withImportAlias "Array"
        |> Generator.withExposing [ funExpose "pop" ]
    --> import Array.Extra as Array exposing (pop)

builders:

  - [`importModule`](#importModule)
  - [`withImportAlias`](#withImportAlias), [`withoutImportAlias`](#withoutImportAlias)
  - [`withExposing`](#withExposing), [`withExposingAll`](#withExposingAll), [`withoutExposing`](#withoutExposing)

-}
type alias Import =
    { name : ModuleName
    , alias : Maybe String
    , exposed : Maybe CodeGen.Exposing
    }


{-| Create an import statement without a module alias and without exposing.

    import Elm.Generator as Generator

    Generator.importModule ( "Module", [ "Name" ] )
    --> import Module.Name

builders:

  - [`importModule`](#importModule)
  - [`withImportAlias`](#withImportAlias), [`withoutImportAlias`](#withoutImportAlias)
  - [`withExposing`](#withExposing), [`withExposingAll`](#withExposingAll), [`withoutExposing`](#withoutExposing)

-}
importModule : ( String, List String ) -> Import
importModule moduleName =
    { name = moduleName
    , alias = Nothing
    , exposed = Nothing
    }


{-| Add an import alias.

    import Elm.Generator as Generator

    Generator.importModule ( "Module", [ "Name" ] )
        |> Generator.withImportAlias "Alias"
    --> import Module.Name as Alias

-}
withImportAlias : String -> Import -> Import
withImportAlias alias import_ =
    { import_ | alias = Just alias }


{-| `import Module` without `as ...`.
-}
withoutImportAlias : Import -> Import
withoutImportAlias import_ =
    { import_ | alias = Nothing }


{-| Add an explicit exposing list to the [`Import`](#Import).

    import Elm.Generator as Generator

    Generator.importModule ( "Module", [ "Name" ] )
        |> Generator.withExposing
            [ typeOrAliasExpose "Type" ]
    --> import Module.Name exposing (Type)

-}
withExposing : List CodeGen.TopLevelExpose -> Import -> Import
withExposing exposingList import_ =
    { import_
        | exposed =
            Just (CodeGen.exposeExplicit exposingList)
    }


{-| Add exposing everything to the [`Import`](#Import).

    import Elm.Generator as Generator

    Generator.importModule ( "Module", [ "Name" ] )
        |> Generator.withExposingAll
    --> import Module.Name exposing (..)

-}
withExposingAll : Import -> Import
withExposingAll import_ =
    { import_
        | exposed = Just CodeGen.exposeAll
    }


{-| `import Module as Alias` without `exposing ...`.
-}
withoutExposing : Import -> Import
withoutExposing import_ =
    { import_ | exposed = Nothing }



--


{-| The common parts of [`FunctionDeclaration`](#FunctionDeclaration), [`TypeDeclaration`](#TypeDeclaration) and [`TypeAliasDeclaration`](#TypeAliasDeclaration):
They all have a name and an optional documentation comment.
-}
type alias Declaration declaration =
    Code
        { declaration
            | name : String
            , documentation :
                Maybe (CodeGen.Comment CodeGen.DocComment)
        }


{-| Elm code that possibly needs imports to compile. See [`Declaration`](#Declaration) and [`Expression`](#Expression).
-}
type alias Code code =
    Printable
        { code
            | imports :
                Dict
                    ModuleName
                    { alias : Maybe String
                    , exposed : Maybe CodeGen.Exposing
                    }
        }


{-| Data you can read and modify, plus a function to turn that data into an elm [`Pretty.Doc`](https://package.elm-lang.org/packages/the-sett/elm-pretty-printer/latest/Pretty#Doc) (see [`print`](#print)).
See [`Generalizable`](#Generalizable).
-}
type alias Printable a =
    Generalizable a Pretty.Doc


{-| Data you can read and modify, plus a function to turn that data into a more general form.
-}
type alias Generalizable specific general =
    { specific : specific
    , toGeneral : specific -> general
    }


{-| Change the specific value based on its current value.

    withX :
        X
        -> Generalizable { x : Maybe X, hasY : Bool } A
        -> Generalizable { x : Maybe X, hasY : Bool } A
    withX x =
        updateSpecific (\a -> { a | x = Just x })

-}
updateSpecific :
    (specific -> specific)
    -> Generalizable specific general
    -> Generalizable specific general
updateSpecific changeSpecific generalizable_ =
    generalizable_
        |> setSpecific
            (generalizable_.specific |> changeSpecific)


setSpecific :
    specific
    -> Generalizable specific general
    -> Generalizable specific general
setSpecific newSpecific generalizable_ =
    { generalizable_ | specific = newSpecific }


{-| Attach a function that can turn the given data into a more general format.

    type Configuration
        = DoA { part : APart, x : Maybe X }
        | DoB { part : BPart, y : Maybe Y }

    doA aPart =
        { part = aPart, x = Default }
            |> generalizable DoA

    withX x =
        updateSpecific (\a -> { a | x = Just x })

    without x =
        updateSpecific (\a -> { a | x = Just x })

    run configuration =
        case configuration |> toGeneral of
            DoA { part, x } ->
            ...

-}
generalizable :
    (specific -> general)
    -> specific
    -> Generalizable specific general
generalizable toGeneral_ specific =
    { specific = specific, toGeneral = toGeneral_ }


{-| Take the generalized form and generalize it to an even more general form.
-}
furtherGeneralizable :
    (general -> moreGeneral)
    -> Generalizable specific general
    -> Generalizable specific moreGeneral
furtherGeneralizable toMoreGeneral generalizable_ =
    generalizable_.specific
        |> generalizable
            (generalizable_.toGeneral
                >> toMoreGeneral
            )


{-| Take the specific data and turn it into a more general form.
-}
toGeneral : Generalizable specific_ general -> general
toGeneral generalizable_ =
    generalizable_.specific
        |> generalizable_.toGeneral



--


{-| Prefer different default imports for the module where the code is generated in. (You still have to `elm install` the desired _dependencies_)

    import Elm.Generator as Generator
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    getSetRecordWithFieldPrefix =
        RecordFieldHelper.monocle
            >> Generator.mapElm
                (\fieldName ->
                    Generator.mapElm
                        (let
                            { access, set } =
                                RecordFieldHelper.functionsForField fieldName
                         in
                         construct "Lens" [ access, set ]
                        )
                        >> Generator.withImports
                            [ Generator.importModule
                                ( "Monocle", [ "Lens" ] )
                                |> Generator.withExposing
                                    [ typeOrAliasExpose "Lens" ]
                            ]
                )

-}
withImports :
    List Import
    -> Code code
    -> Code code
withImports imports code =
    code
        |> updateSpecific
            (\c ->
                { c
                    | imports =
                        imports |> joinAndSortImports
                }
            )



--


{-| You can generate your own documentation:

    import Elm.Generator as Generator
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    accessorsWithDocumentation name { fieldName } =
        RecordFieldHelper.accessors.elm name
            |> Generator.withDocumentation
                (emptyDocComment
                    |> markdown
                        ("elm-accessors lens for the field `." ++ fieldName ++ "`.")
                )

-}
withDocumentation :
    CodeGen.Comment CodeGen.DocComment
    -> Declaration declaration
    -> Declaration declaration
withDocumentation docComment declaration =
    declaration
        |> updateSpecific
            (\d ->
                { d | documentation = docComment |> Just }
            )



--


{-| Component that are needed to build an expression.

You can customize existing `Expression`s using

  - [`replaceExpression`](#replaceExpression)
  - [`withImports`](#withImports)

-}
type alias Expression =
    Code { expression : CodeGen.Expression }


{-| An [`Expression`](#Expression) plus the imports it uses.

    import Elm.Generator as Generator

    Generator.expression (accessFun ".id")
        |> Generator.print ()
    --> ".id"

-}
expression : CodeGen.Expression -> Expression
expression expression_ =
    { expression = expression_
    , imports = Dict.empty
    }
        |> generalizable
            (.expression >> CodeGen.prettyExpression)


{-| Return the same result using a different expression.

For [`FunctionDeclaration`](#FunctionDeclaration): [`replaceExpressionAndArguments`](#replaceExpressionAndArguments) allows using different arguments.

-}
replaceExpression :
    expression
    -> Code { code | expression : expression }
    -> Code { code | expression : expression }
replaceExpression newExpression code =
    code
        |> updateSpecific
            (\c -> { c | expression = newExpression })



--


{-| All the components to build a function declaration:

    {-| [documentation]
    -}
    [name] : [annotation]
    [name] [arguments] =
        [expression]

You can customize existing `FunctionDeclaration`s using

  - [`replaceExpression`](#replaceExpression) and [`replaceExpressionAndArguments`](#replaceExpressionAndArguments)
  - [`withDocumentation`](#withDocumentation)
  - [`withAnnotation`](#withAnnotation)
  - [`withImports`](#withImports)

or create custom helper with [`functionDeclaration`](#functionDeclaration).

-}
type alias FunctionDeclaration =
    Declaration
        { annotation : Maybe CodeGen.TypeAnnotation
        , arguments : List CodeGen.Pattern
        , expression : CodeGen.Expression
        }


{-| Use a different type annotation for the generated helper.

    import Elm.Generator as Generator

    svgDeclaration { declarationName, fq } { svgNode } =
        Generator.valueDeclaration declarationName
            (svgNode |> toElmSvgCode { fq = fq })
            |> Generator.withAnnotation
                (typed "Svg" [ typeVar "msg" ])

-}
withAnnotation :
    annotation
    -> Declaration { declaration | annotation : Maybe annotation }
    -> Declaration { declaration | annotation : Maybe annotation }
withAnnotation newAnnotation functionDeclaration_ =
    functionDeclaration_
        |> updateSpecific
            (\d ->
                { d | annotation = newAnnotation |> Just }
            )


{-| Return the same result using different argument patterns and a different implementation.

Use [`replaceExpression`](#replaceExpression) if the arguments should stay the same.

-}
replaceExpressionAndArguments :
    arguments
    -> expression
    ->
        Declaration
            { declaration
                | arguments : arguments
                , expression : expression
            }
    ->
        Declaration
            { declaration
                | arguments : arguments
                , expression : expression
            }
replaceExpressionAndArguments newArguments newExpression functionDeclaration_ =
    functionDeclaration_
        |> updateSpecific
            (\d ->
                { d
                    | arguments = newArguments
                    , expression = newExpression
                }
            )



--


{-| The components to build a `type` declaration:

    {-| [documentation]
    -}
    type [Name] [arguments]
        = [Constructors]

-}
type alias TypeDeclaration =
    Declaration
        { arguments : List String
        , constructors :
            List ( String, List CodeGen.TypeAnnotation )
        }


{-| Build a [`TypeDeclaration`](#TypeDeclaration) without documentation and without imports.

    import Elm.Generator as Generator

    Generator.typeDeclaration "List"
        [ "a" ]
        [ ( "Nil", [] )
        , ( "Cons", [ typeVar "a", typed "List" [ typeVar "a" ] ] )
        ]
        |> Generator.print
    --> """type List a
    -->     = Nil
    -->     | Cons a (List a)"""

-}
typeDeclaration :
    String
    -> List String
    -> List ( String, List CodeGen.TypeAnnotation )
    -> TypeDeclaration
typeDeclaration name typeArguments constructors =
    { name = name
    , arguments = typeArguments
    , constructors = constructors
    , documentation = Nothing
    , imports = Dict.empty
    }
        |> generalizable
            (\typeDecl ->
                CodeGen.customTypeDecl
                    typeDecl.documentation
                    typeDecl.name
                    typeDecl.arguments
                    typeDecl.constructors
            )
        |> furtherGeneralizable (CodeGen.prettyDeclaration 100)


{-| Change the constructors of the `TypeDeclaration`. The arguments and the documentation are kept.
-}
replaceConstructors :
    constructors
    -> Declaration { declaration | constructors : constructors }
    -> Declaration { declaration | constructors : constructors }
replaceConstructors newConstructors typeDeclaration_ =
    typeDeclaration_
        |> updateSpecific
            (\d ->
                { d | constructors = newConstructors }
            )


{-| Change the arguments and constructors of the `TypeDeclaration`. Only the documentation is kept.
-}
replaceConstructorsAndArguments :
    arguments
    -> constructors
    ->
        Declaration
            { declaration
                | arguments : arguments
                , constructors : constructors
            }
    ->
        Declaration
            { declaration
                | arguments : arguments
                , constructors : constructors
            }
replaceConstructorsAndArguments arguments constructors typeDeclaration_ =
    typeDeclaration_
        |> updateSpecific
            (\d ->
                { d
                    | arguments = arguments
                    , constructors = constructors
                }
            )



--


{-| The components to build a `type alias`:

    {-| [documentation]
    -}
    type alias Name [arguments] =
        [aliasedType]

Customize:

  - [`withDocumentation`](#withDocumentation)
  - [`replaceAliasedType`](#replaceAliasedType) and [`replaceAliasedTypeAndArguments`](#replaceAliasedTypeAndArguments)
  - [`withImports`](#withImports)

Create a `type alias` declaration using [`typeAliasDeclaration`](#typeAliasDeclaration).

-}
type alias TypeAliasDeclaration =
    Declaration
        { arguments : List String
        , aliasedType : CodeGen.TypeAnnotation
        }


{-| Create a [`TypeAliasDeclaration`] without documentation and without imports.

An example:

    import Elm.Generator as Generator

    Generator.typeAliasDeclaration "ResultE"
        [ "e", "a" ]
        (typed "Result"
            [ typed "List" [ typeVar "e" ]
            , typeVar "a"
            ]
        )
        |> Generator.print
    --> """type alias ResultE e a =
    -->     Result (List e) a"""

-}
typeAliasDeclaration : String -> List String -> CodeGen.TypeAnnotation -> TypeAliasDeclaration
typeAliasDeclaration name arguments aliasedType =
    { name = name
    , arguments = arguments
    , aliasedType = aliasedType
    , documentation = Nothing
    , imports = Dict.empty
    }
        |> generalizable
            (\aliasDecl ->
                CodeGen.aliasDecl
                    aliasDecl.documentation
                    aliasDecl.name
                    aliasDecl.arguments
                    aliasDecl.aliasedType
            )
        |> furtherGeneralizable (CodeGen.prettyDeclaration 100)


{-| Use the same arguments to alias a different type.
See [`replaceAliasedTypeAndArguments`](#replaceAliasedTypeAndArguments) if you need different arguments.
-}
replaceAliasedType :
    CodeGen.TypeAnnotation
    -> TypeAliasDeclaration
    -> TypeAliasDeclaration
replaceAliasedType aliasedType typeAliasDeclaration_ =
    typeAliasDeclaration_
        |> updateSpecific (\d -> { d | aliasedType = aliasedType })


{-| Use the same arguments to alias a different type.
See [`replaceAliasedType`](#replaceAliasedType) if it has the same arguments.
-}
replaceAliasedTypeAndArguments :
    List String
    -> CodeGen.TypeAnnotation
    -> TypeAliasDeclaration
    -> TypeAliasDeclaration
replaceAliasedTypeAndArguments arguments aliasedType typeAliasDeclaration_ =
    typeAliasDeclaration_
        |> updateSpecific
            (\d ->
                { d
                    | arguments = arguments
                    , aliasedType = aliasedType
                }
            )



--


{-| Print something that can turn itself into elm [`Pretty.Doc`](https://package.elm-lang.org/packages/the-sett/elm-pretty-printer/latest/Pretty#Doc).

    import Elm.Generator as Generator

    test "custom FieldHelperGenerator"
        (\() ->
            customFieldHelperGenerator.elm { fieldName = "test" }
                |> Generator.print { declarationName = "test" }
                |> Expect.equal
                    """{-| A lens for the field `.test`.
    -}
    test : CustomLens { record | test : test } test
    test =
        customLens
            { access = .test
            , set = \\test_ r -> { r | test = test_ }
            }"""
        )

-}
print : Printable a_ -> String
print printable =
    printable |> toGeneral |> printPretty


{-| Print elm.

    import Test exposing (test)
    import Expect
    import Elm.Generator as Generator

    test "custom FieldHelperGenerator"
        (\() ->
            customFieldHelperGenerator.elm
                |> Generator.printUsingSpecifiedImports "test"
                    { fieldName = "test" }
                |> Expect.equal
                    """{-| A lens for the field `.test`.
    -}
    test : CustomLens { record | test : test } test
    test =
        customLens
            { access = .test
            , set = \\test_ r -> { r | test = test_ }
            }"""
        )

-}
printUsingSpecifiedImports :
    String
    -> information
    ->
        ({ declarationName : String
         , imports :
            Imports
         }
         -> information
         -> Code elm_
        )
    -> String
printUsingSpecifiedImports declarationName information declaration =
    let
        imports =
            declaration
                { declarationName = declarationName
                , imports = Dict.empty
                }
                information
                |> .specific
                |> .imports
    in
    declaration
        { declarationName = declarationName
        , imports = imports
        }
        information
        |> print


{-| Create a [`FunctionDeclaration`](#FunctionDeclaration) without a documentation comment and without a type annotation.

    setterDeclaration { declarationName, fq } { fieldName } =
        Generator.functionDeclaration declarationName
            [ varPattern (fieldName ++ "_")
            , varPattern "record"
            ]
            (update "record"
                [ ( fieldName, val (fieldName ++ "_") ) ]
            )
            |> Generator.withDocumentation
                (emptyDocComment
                    |> markdown
                        ("Setter for the field `." ++ fieldName ++ "`.")
                )
            |> Generator.withAnnotation
                (fq.typed ( "CustomLens", [] )
                    "CustomLens"
                    [ extRecordAnn "record"
                        [ ( fieldName, typeVar fieldName ) ]
                    , typeVar fieldName
                    ]
                )

-}
functionDeclaration :
    String
    -> List CodeGen.Pattern
    -> CodeGen.Expression
    -> FunctionDeclaration
functionDeclaration name arguments expression_ =
    { name = name
    , documentation = Nothing
    , annotation = Nothing
    , arguments = arguments
    , expression = expression_
    , imports = Dict.empty
    }
        |> generalizable
            (\funDecl ->
                CodeGen.funDecl
                    funDecl.documentation
                    funDecl.annotation
                    funDecl.name
                    funDecl.arguments
                    funDecl.expression
            )
        |> furtherGeneralizable
            (CodeGen.prettyDeclaration 100)


{-| Create a [`FunctionDeclaration`](#FunctionDeclaration) without arguments, without a documentation comment and without a type annotation.
This is equivalent to

    Elm.Generator.functionDeclaration _ []

an example:

    import Elm.Generator as Generator
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    customLensDeclaration { declarationName, fq } { fieldName } =
        Generator.valueDeclaration declarationName
            (let
                { access, set } =
                    RecordFieldHelper.functionsForField fieldName
             in
             fq.construct ( "CustomLens", [] ) "create" [ access, set ]
            )
            |> Generator.withDocumentation
                (emptyDocComment
                    |> markdown
                        ("`CustomLens` for the field `." ++ fieldName ++ "`.")
                )
            |> Generator.withAnnotation
                (typed "CustomLens"
                    [ extRecordAnn "record"
                        [ ( fieldName, typeVar fieldName ) ]
                    , typeVar fieldName
                    ]
                )

-}
valueDeclaration : String -> CodeGen.Expression -> FunctionDeclaration
valueDeclaration name expression_ =
    functionDeclaration name [] expression_
