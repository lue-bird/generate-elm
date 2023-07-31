module Elm.Code.Module exposing
    ( Import, Imports
    , import_, implicitImports, importsFromList
    , withAs, withoutAs
    , Exposing, ExplicitExposing
    , withExposingAll, withExposing, addOpenTypeExposes, addTypeAndAliasExposes, addValueExposes, withoutExposing
    )

{-|


## import

@docs Import, Imports


## import statements

@docs import_, implicitImports, importsFromList


### `as`

@docs withAs, withoutAs


### `exposing`

@docs Exposing, ExplicitExposing
@docs withExposingAll, withExposing, addOpenTypeExposes, addTypeAndAliasExposes, addValueExposes, withoutExposing

-}

import AssocSet
import Dict exposing (Dict)
import Elm.Code.Common exposing (from)
import Elm.Code.Operator.Common as Operator exposing (Operator)
import Elm.Code.Util exposing (RecordWithoutConstructorFunction)
import Elm.CodeGen as CodeGen
import Elm.Syntax.Exposing
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Set exposing (Set)



--


moduleNameToSyntax : String -> List String
moduleNameToSyntax moduleName =
    moduleName |> String.split "."


moduleNameFromSyntax : List String -> String
moduleNameFromSyntax moduleNameSyntax =
    moduleNameSyntax |> String.join "."


{-| [`Import`](#Import)s by imported module name.
-}
type alias Imports =
    Dict
        String
        { alias : Maybe String
        , exposed : Maybe Exposing
        }


exposedNames : ExplicitExposing -> Set String
exposedNames expose =
    [ expose.values
    , expose.typesAndAliases
    , expose.openTypes
    , expose.infixOperators
        |> AssocSet.toList
        |> List.map
            (\operator ->
                let
                    ( _, symbol ) =
                        operator |> Operator.info |> .origin
                in
                symbol
            )
        |> Set.fromList
    ]
        |> List.foldl Set.union Set.empty


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicitImports : Imports
implicitImports =
    [ import_ "Basics" |> withExposingAll
    , import_ "List" |> withExposing (addTypeAndAliasExposes [ "List" ] >> addInfixExpose Operator.Cons)
    , import_ "Maybe" |> withExposing (addOpenTypeExposes [ "Maybe" ])
    , import_ "Result" |> withExposing (addOpenTypeExposes [ "Result" ])
    , import_ "String" |> withExposing (addTypeAndAliasExposes [ "String" ])
    , import_ "Char" |> withExposing (addTypeAndAliasExposes [ "Char" ])
    , import_ "Tuple"
    , import_ "Debug"
    , import_ "Platform" |> withExposing (addTypeAndAliasExposes [ "Program" ])
    , import_ "Platform.Cmd" |> withAs "Cmd" |> withExposing (addTypeAndAliasExposes [ "Cmd" ])
    , import_ "Platform.Sub" |> withAs "Sub" |> withExposing (addTypeAndAliasExposes [ "Sub" ])
    ]
        |> importsFromList


importsFromList : List Import -> Imports
importsFromList imports =
    imports
        |> List.map
            (\{ moduleName, alias, exposed } ->
                ( moduleName, { alias = alias, exposed = exposed } )
            )
        |> Dict.fromList


{-| Convert an [`Import`](#Import) to an `Elm.Syntax.Import.Import`.
-}
importToSyntax : Import -> Elm.Syntax.Import.Import
importToSyntax moduleImport =
    CodeGen.importStmt
        (moduleImport.moduleName |> moduleNameToSyntax)
        (moduleImport.alias
            |> Maybe.map List.singleton
        )
        (moduleImport.exposed
            |> Maybe.map exposingToSyntax
        )


{-| The components to build an import statement:

    import [Module.Name] [as Alias] [exposing (exposed)]

example:

    import Elm.Code as Code exposing (exposeValues)

    Code.import_ ( "Array", [ "Extra" ] )
        |> Code.withAs "Array"
        |> Code.withExposing [ exposeValues [ "pop" ] ]
    --→ import Array.Extra as Array exposing (pop)

builders:

  - [`import_`](#import_)
  - [`withAs`](#withAs), [`withoutImportAlias`](#withoutImportAlias)
  - [`withExposing`](#withExposing), [`withExposingAll`](#withExposingAll), [`withoutExposing`](#withoutExposing)

-}
type alias Import =
    RecordWithoutConstructorFunction
        { moduleName : String
        , alias : Maybe String
        , exposed : Maybe Exposing
        }


{-| Create an import statement without a module name alias and without exposing.

    import Elm.Code exposing (import_)

    import_ "Module.Name"

would generate

    import Module.Name

builders:

  - [`import_`](#import_)
  - [`withAs`](#withAs), [`withoutImportAlias`](#withoutImportAlias)
  - [`withExposing`](#withExposing), [`withExposingAll`](#withExposingAll), [`withoutExposing`](#withoutExposing)

-}
import_ : String -> Import
import_ moduleName =
    { moduleName = moduleName
    , alias = Nothing
    , exposed = Nothing
    }


{-| Use an import alias.

    import Elm.Code as Code

    Code.import_ ( "Module", [ "Name" ] )
        |> Code.withAs "Alias"
    --→ import Module.Name as Alias

-}
withAs : String -> Import -> Import
withAs alias =
    \theImport -> { theImport | alias = Just alias }


{-| `import Module` without `as ...`.
-}
withoutAs : Import -> Import
withoutAs =
    \theImport -> { theImport | alias = Nothing }


{-| `exposing`.


#### `ExposingAll`

[`withExposingAll`](#withExposingAll) would generate

    exposing (..)


#### `Exposing`

[`withExposing`](#withExposing):

    withExposing
        (addValueExposes [ "hello" ]
            >> addTypeAndAliasExposes [ "World" ]
            >> addOpenTypeExposes [ "Options" ]
        )

would generate

    exposing (hello, World, Options(..))

-}
type Exposing
    = ExposingAll
    | Exposing ExplicitExposing


{-| The specific of a single exposed item.


#### `ValueExpose`

    ValueExpose push

would generate

    -- exposing (
          push
    --    )


#### `TypeAliasExpose`

    TypeAliasExpose CanAlsoBeAType

would generate

    -- exposing (
          CanAlsoBeAType
    --    )


#### `OpenTypeExpose`

    OpenTypeExpose Tree

would generate

    -- exposing (
          Tree(..)
    --    )


#### `InfixExpose`

    OpenTypeExpose Tree

would generate

    -- exposing (
          Tree(..)
    --    )

-}
type alias ExplicitExposing =
    RecordWithoutConstructorFunction
        { values : Set String
        , typesAndAliases : Set String
        , openTypes : Set String
        , infixOperators : AssocSet.Set Operator
        }


{-| Convert an `Elm.Syntax.Exposing.Exposing` into an [`Elm.Module.Exposing`](#Exposing).
-}
syntaxToExposingIn : String -> Elm.Syntax.Exposing.Exposing -> Exposing
syntaxToExposingIn moduleName exposing_ =
    case exposing_ of
        Elm.Syntax.Exposing.All _ ->
            ExposingAll

        Elm.Syntax.Exposing.Explicit exposes ->
            exposes
                |> List.foldl
                    (\(Node _ topLevelExpose) expose ->
                        case topLevelExpose of
                            Elm.Syntax.Exposing.InfixExpose name ->
                                case Operator.byOrigin (from moduleName name) of
                                    Just okOperator ->
                                        expose |> addInfixExpose okOperator

                                    Nothing ->
                                        expose

                            Elm.Syntax.Exposing.FunctionExpose name ->
                                { expose
                                    | values =
                                        expose.values |> Set.insert name
                                }

                            Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                { expose
                                    | typesAndAliases =
                                        expose.typesAndAliases |> Set.insert name
                                }

                            Elm.Syntax.Exposing.TypeExpose { name, open } ->
                                case open of
                                    Just _ ->
                                        { expose
                                            | openTypes =
                                                expose.openTypes |> Set.insert name
                                        }

                                    Nothing ->
                                        { expose
                                            | typesAndAliases =
                                                expose.typesAndAliases |> Set.insert name
                                        }
                    )
                    { values = Set.empty
                    , typesAndAliases = Set.empty
                    , openTypes = Set.empty
                    , infixOperators = AssocSet.empty
                    }
                |> Exposing


{-| Convert an [`Code.Exposing`](#Exposing) into an `Elm.Syntax.Exposing.Exposing`.
-}
exposingToSyntax : Exposing -> Elm.Syntax.Exposing.Exposing
exposingToSyntax exposing_ =
    case exposing_ of
        ExposingAll ->
            CodeGen.exposeAll

        Exposing expose ->
            [ expose.values
                |> Set.toList
                |> List.map CodeGen.funExpose
            , expose.typesAndAliases
                |> Set.toList
                |> List.map CodeGen.typeOrAliasExpose
            , expose.openTypes
                |> Set.toList
                |> List.map CodeGen.openTypeExpose
            , expose.infixOperators
                |> AssocSet.toList
                |> List.map
                    (Operator.info
                        >> .origin
                        >> (\( _, name ) -> Elm.Syntax.Exposing.InfixExpose name)
                    )
            ]
                |> List.concat
                |> CodeGen.exposeExplicit


{-| Add an explicit exposing list to the [`Import`](#Import).

    import Elm.Code as Code exposing (addTypeAndAliasExposes, addValueExposes)

    Generator.import_ ( "Module", [ "Name" ] )
        |> Generator.withExposing
            (addTypeAndAliasExposes [ "AType", "BType" ]
                >> addValueExposes [ "a", "b" ]
            )
    --→ import Module.Name exposing (AType, BType, a, b)

-}
withExposing : (ExplicitExposing -> ExplicitExposing) -> Import -> Import
withExposing addExposes =
    \theImport ->
        { theImport
            | exposed =
                Exposing
                    ({ values = Set.empty
                     , typesAndAliases = Set.empty
                     , openTypes = Set.empty
                     , infixOperators = AssocSet.empty
                     }
                        |> addExposes
                    )
                    |> Just
        }


{-| Conveniently expose multiple functions or values.

    import Elm.Code exposing (import_, withExposing, addValueExposes)

    import_ ( "Html", [] )
        |> withExposing
            (addValueExposes [ "div", "text" ])

-}
addValueExposes :
    List String
    -> ExplicitExposing
    -> ExplicitExposing
addValueExposes newExposedValues explicitExposing =
    { explicitExposing
        | values =
            newExposedValues |> Set.fromList
    }


{-| Conveniently expose multiple type aliases and closed (no `Type(..)`) types.

    import Elm.Code exposing (import_, withExposing, exposeTypesAndAliases)

    import_ ( "Lazy", [ "Tree" ] )
        |> withExposing
            (addTypeAndAliasExposes [ "Tree", "Forest" ])

-}
addTypeAndAliasExposes :
    List String
    -> ExplicitExposing
    -> ExplicitExposing
addTypeAndAliasExposes newExposedTypesAndAliases explicitExposing =
    { explicitExposing
        | typesAndAliases =
            newExposedTypesAndAliases |> Set.fromList
    }


{-| Conveniently expose types including all their variants (`Type(..)`).

    import Elm.Code exposing (import_, withExposing, addOpenTypeExposes)

    import_ ( "Elm", [ "Syntax", "Exposing" ] )
        |> withExposing
            (addOpenTypeExposes
                [ "Exposing", "TopLevelExpose" ]
            )

-}
addOpenTypeExposes :
    List String
    -> ExplicitExposing
    -> ExplicitExposing
addOpenTypeExposes newExposedOpenTypes explicitExposing =
    { explicitExposing
        | openTypes =
            newExposedOpenTypes |> Set.fromList
    }


{-| Conveniently expose multiple type aliases and closed types.

    import_ "Parser"
        |> withExposing (addInfixExpose ignore)

would generate

    import Parser exposing ((|.))

`addInfixExpose` isn't exposed
because infix operators can't be qualified, so they are always exposed automatically.

-}
addInfixExpose : Operator -> ExplicitExposing -> ExplicitExposing
addInfixExpose newInfixOperator explicitExposing =
    { explicitExposing
        | infixOperators =
            explicitExposing.infixOperators
                |> AssocSet.insert newInfixOperator
    }


{-| Add exposing everything to the [`Import`](#Import).

    import Elm.Code as Code

    Generator.import_ ( "Module", [ "Name" ] )
        |> Generator.withExposingAll
    --→ import Module.Name exposing (..)

-}
withExposingAll : Import -> Import
withExposingAll =
    \theImport ->
        { theImport | exposed = Just ExposingAll }


{-| `import Module as Alias` without `exposing ...`.
-}
withoutExposing : Import -> Import
withoutExposing =
    \theImport -> { theImport | exposed = Nothing }



--


type alias ModuleHeader =
    RecordWithoutConstructorFunction
        { kind : ModuleKind
        , moduleName : String
        , exposed : Exposing
        }


normalModuleHeader : String -> Exposing -> ModuleHeader
normalModuleHeader moduleName exposed =
    { specific = Normal
    , moduleName = moduleName
    , exposed = exposed
    }


portModuleHeader : String -> Exposing -> ModuleHeader
portModuleHeader moduleName exposed =
    { specific = Port
    , moduleName = moduleName
    , exposed = exposed
    }


moduleHeaderToSyntax : ModuleHeader -> Elm.Syntax.Module.Module
moduleHeaderToSyntax moduleHeader_ =
    let
        moduleWith =
            case moduleHeader_.specific of
                Port ->
                    Elm.Syntax.Module.PortModule

                Normal ->
                    Elm.Syntax.Module.NormalModule

        moduleHeaderData : Elm.Syntax.Module.DefaultModuleData
        moduleHeaderData =
            { moduleName =
                moduleHeader_.moduleName
                    |> moduleNameToSyntax
                    |> Node emptyRange
            , exposingList =
                moduleHeader_.exposed
                    |> exposingToSyntax
                    |> Node emptyRange
            }
    in
    moduleWith moduleHeaderData


type ModuleKind
    = Port
    | Normal
