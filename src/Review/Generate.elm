module Review.Generate exposing
    ( rule
    , Config
    , InSameModuleConfig, inSameModule
    , InDifferentModuleConfig, inModule
    , DeclarationInsertLocation, belowMarker, belowAllDeclarations, belowDeclarations
    , locally
    , replaceStub, ReplaceStubConfig
    )

{-|

@docs rule


## configure

@docs Config


### in same module

@docs InSameModuleConfig, inSameModule


### in different module

@docs InDifferentModuleConfig, inModule


### where to put declarations

@docs DeclarationInsertLocation, belowMarker, belowAllDeclarations, belowDeclarations
@docs locally


### replacing a stub

@docs replaceStub, ReplaceStubConfig

-}

import Elm.Code exposing (Code, ExpressionAny, ModuleScopeDeclarationAny)
import Elm.Code.Generator exposing (AllowsUnqualified, DeclarationGenerator, ExpressionGenerator)
import Elm.CodeGen as CodeGen
import Generalizable exposing (Generalizable, toGeneral)
import Review.Generate.Internal as Internal
import Review.Rule exposing (Rule)


{-| Finds references to declarations that don't exist and generates elm to either

  - create the missing declaration: [`inSameModule`](#inSameModule) or [`inModule`](#inModule)
  - create an expression in this place: [`replaceStub`](#replaceStub)


### Example

    import Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Review.Generate

    config =
        [ Review.Generate.inModule
            [ "Accessors", "Library", "Fields" ]
            RecordFieldHelper.accessors
            |> Review.Generate.rule
        ]

another module:

    module Player exposing (scoreAPoint)

    import Accessors.Library.Fields as Field

    scoreAPoint =
        Accessors.over Field.score ((+) 1)


### Fail

    module Accessors.Library.Fields exposing (name)
    ...


### Success

    module Accessors.Library.Fields exposing (score)
    ...

-}
rule :
    Generalizable
        config_
        (Config forChecking_ information_ declaration_)
    -> Rule
rule ruleConfig =
    Internal.rule (ruleConfig |> toGeneral)


{-| The [rule](#rule)'s configuration.

  - [`Review.Generate.replaceStub`](#replaceStub)
    generate an expression where some given token like `toGenerateX` is noticed

  - [`Review.Generate.inSameModule`](#inSameModule)
    generate a missing declaration of some format in the module it was used

  - [`Review.Generate.inModule`](#inModule)
    generate a missing declaration in a given module (and import it if necessary)

-}
type alias Config information requireQualified declaration =
    Internal.Config information requireQualified declaration


{-| Configuration for what kind of expression will be generated as a replacement for some kind of stub.
See [`Review.Generate.replaceStub`](#replaceStub).
-}
type alias ReplaceStubConfig information =
    { stub : String
    , generator : ExpressionGenerator information
    }


{-| Configuration for what kind of declaration will be generated in what module.
See [`Review.Generate.inModule`](#inModule).

Configure where the generated declarations will be placed:

  - default: [`locally`](#locally)
  - [`belowMarker`](#belowMarker)
  - [`belowAllDeclarations`](#belowAllDeclarations)
  - [`belowDeclarations`](#belowDeclarations)

-}
type alias InSameModuleConfig information declaration =
    Internal.InSameModuleConfig information declaration


{-| Configuration for what kind of declaration will be generated in what module.
See [`Review.Generate.inSameModule`](#inSameModule).

Configure where the generated declarations will be placed:

  - [`belowMarker`](#belowMarker)
  - [`belowAllDeclarations`](#belowAllDeclarations)
  - [`belowDeclarations`](#belowDeclarations)

-}
type alias InDifferentModuleConfig requireQualified information declaration =
    Internal.InDifferentModuleConfig requireQualified information declaration


{-| Configure where the generated declarations will be placed:

  - [`belowMarker`](#belowMarker)
  - [`belowAllDeclarations`](#belowAllDeclarations)
  - [`belowDeclarations`](#belowDeclarations)

There are also other options only available for [`InSameModuleConfig`](#InSameModuleConfig):

  - [`locally`](#locally)

-}
type alias DeclarationInsertLocation otherInsertLocation information =
    Internal.DeclarationInsertLocation otherInsertLocation information


{-| Configure what kind of declaration will be generated in what module.

  - first argument: The module where all declarations will be generated in.

    understand `( "Module", [ "Name" ] )` as `Module.Name`

  - second argument: What kind of declaration to generate: See [`Generator`](Elm-Generator#Generator).

Note: The record field helpers must be qualified, otherwise no fix will be proposed.
Using an alias is also possible:

    import Field.Update as Update
    Update.score


#### Configuration

    import Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Review.Generate

    config =
        [ Review.Generate.inModule ( "Helpers", [ "Field" ] )
            RecordFieldHelper.update
            |> Review.Generate.belowMarker "update"
            |> Review.Generate.rule
        , Review.Generate.inModule ( "Helpers", [ "Field" ] )
            RecordFieldHelper.set
            |> Review.Generate.rule
        , Review.Generate.inModule ( "FieldLens", [] )
            RecordFieldHelper.monocle
            |> Review.Generate.rule
        ]

Configure where the generated declarations will be placed:

  - default: [`belowAllDeclarations`](#belowAllDeclarations)
  - [`belowMarker`](#belowMarker)
  - [`belowDeclarations`](#belowDeclarations)

-}
inModule :
    String
    ->
        DeclarationGenerator
            requireQualified
            information
            (Generalizable declaration ModuleScopeDeclarationAny)
    ->
        Generalizable
            (InDifferentModuleConfig
                requireQualified
                information
                declaration
            )
            (Config information requireQualified declaration)
inModule moduleToGenerateIn generators =
    Internal.inModule moduleToGenerateIn generators


{-| Generate declarations that don't exist yet in the module they were used. See [`DeclarationGenerator`](Elm-Generator#DeclarationGenerator).

    import Elm.Generator.RecordFieldHelper as RecordFieldHelper
    import Review.Generate

    config =
        [ Review.Generate.inSameModule
            RecordFieldHelper.set
            |> Review.Generate.belowMarker "set"
            |> Review.Generate.rule
        , Review.Generate.inSameModule
            RecordFieldHelper.update
            |> Review.Generate.belowMarker "update"
            |> Review.Generate.rule
        ]

Configure where the generated declarations will be placed:

  - default: [`locally`](#locally)
  - [`belowMarker`](#belowMarker)
  - [`belowAllDeclarations`](#belowAllDeclarations)
  - [`belowDeclarations`](#belowDeclarations)

-}
inSameModule :
    DeclarationGenerator
        AllowsUnqualified
        information
        (Generalizable declaration ModuleScopeDeclarationAny)
    ->
        Generalizable
            (InSameModuleConfig information declaration)
            (Config information Never declaration)
inSameModule generator =
    Internal.inSameModule generator


{-| Place the generated declaration after a given marker comment:

    a =
        pearIcon |> ...

    -- ↓ icons

    pearIcon =
        ...

configuration:

    Review.Generate.inSameModule iconGenerator
        |> Review.Generate.belowMarker "↓ icons"

-}
belowMarker :
    String
    ->
        Generalizable
            { config
                | insertLocation :
                    DeclarationInsertLocation otherInsertLocation information
            }
            general
    ->
        Generalizable
            { config
                | insertLocation :
                    DeclarationInsertLocation otherInsertLocation information
            }
            general
belowMarker marker =
    Generalizable.alter
        (\c ->
            { c
                | insertLocation =
                    Internal.InsertBelowMarker marker
            }
        )


{-| Place the generated declaration after all other declarations in a module:

    module Module exposing (a, b, c)

    a =
        ...

    b =
        ...

    c =
        pearIcon |> ...

    pearIcon =
        ...

configuration:

    Review.Generate.inSameModule iconGenerator
        |> Review.Generate.belowAllDeclarations

Ths is the default for [`Review.Generate.inModule`](#inModule).

-}
belowAllDeclarations :
    Generalizable
        { config
            | insertLocation :
                DeclarationInsertLocation otherInsertLocation information
        }
        general
    ->
        Generalizable
            { config
                | insertLocation :
                    DeclarationInsertLocation otherInsertLocation information
            }
            general
belowAllDeclarations =
    Generalizable.alter
        (\c ->
            { c
                | insertLocation =
                    Internal.InsertBelowAllDeclarations
            }
        )


{-| Keep a given order among the declarations:
Place the generated declaration after the last given declaration, else the second last, ...:

    module Module exposing (a, z)

    a =
        ...

    type alias Point =
        ( Float, Float )

    encodePoint =
        ...

    decodePoint =
        ...

    z =
        ...

or if no `encodePoint` exists:

    module Module exposing (a, z)

    a =
        ...

    type alias Point =
        ( Float, Float )

    decodePoint =
        ...

    z =
        ...

configuration:

    import Review.Generate.Coder

    Review.Generate.inSameModule
        Review.Generate.Coder.decode
        |> Review.Generate.belowDeclarations
            (\{ typeOrAliasName } ->
                [ typeOrAliasName
                , "encode" ++ typeOrAliasName
                ]
            )

-}
belowDeclarations :
    (information -> List String)
    ->
        Generalizable
            { config
                | insertLocation :
                    DeclarationInsertLocation otherInsertLocation information
            }
            general
    ->
        Generalizable
            { config
                | insertLocation :
                    DeclarationInsertLocation otherInsertLocation information
            }
            general
belowDeclarations declarationsAbove =
    Generalizable.alter
        (\c ->
            { c
                | insertLocation =
                    Internal.InsertBelowDeclarations
                        declarationsAbove
            }
        )


{-| Place the generated declaration after the first declaration it was used in:

    module Module exposing (a, b, c, generated)

    a =
        ...

    type alias Point =
        ( Float, Float )

    encodePoint =
        ...

    decodePoint =
        ...

    z =
        ...

or if no `encodePoint` exists:

    module Module exposing (a, b, c, generated)

    a =
        ...

    type alias Point =
        ( Float, Float )

    decodePoint =
        ...

    z =
        ...

configuration:

    import Review.Generate.Coder

    Review.Generate.inSameModule
        Review.Generate.Coder.decode
        |> Review.Generate.belowDeclarations
            (\{ typeOrAliasName } ->
                [ typeOrAliasName
                , "encode" ++ typeOrAliasName
                ]
            )

-}
locally :
    Generalizable
        (InSameModuleConfig information declaration)
        config
    ->
        Generalizable
            (InSameModuleConfig information declaration)
            config
locally =
    Generalizable.alter
        (\c ->
            { c
                | insertLocation =
                    Internal.OtherInsertLocation
                        Internal.AfterDeclarationItWasReferencedIn
            }
        )


{-| Generate a given expression where a (non-existent) function with the given name is called

    import Review.Generate
    import Elm.Generator.Svg as Svg

    Review.Generate.replaceStub "toGenerateSvg"
        Svg.Generate
        |> Review.Generate.rule

will replace

    toGenerateSvg """<svg>This is svg</svg>"""

-}
replaceStub :
    String
    -> ExpressionGenerator information
    ->
        Generalizable
            (ReplaceStubConfig information)
            (Config information Never Never)
replaceStub stub generator =
    Internal.replaceStub stub generator
