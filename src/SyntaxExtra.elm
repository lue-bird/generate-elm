module SyntaxExtra exposing (Imports, ModuleName, containsRange, isValueOrCall, nameOfDeclaration, nameOfExpose, printDeclaration, printExpressionSyntax, printImports, reindent, typeds)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.Code as Code exposing (Exposing, Import, declarationToDslSyntax)
import Elm.CodeGen as CodeGen exposing (exposeExplicit, openTypeExpose, typeOrAliasExpose)
import Elm.Pretty as CodeGen
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose(..))
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Import as Import
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Generalizable exposing (Generalizable)
import List.Extra as List
import List.NonEmpty
import Misc exposing (applyIfJust, firstJust, prettyWidth, printPretty)
import Pretty exposing (pretty)
import Stack exposing (StackFilled)


type alias ModuleName =
    String


{-| [`Import`](#Import)s by imported module name.
-}
type alias Imports =
    Dict
        ModuleName
        { alias : Maybe String
        , exposed : Maybe Exposing
        }


nameOfExpose : CodeGen.TopLevelExpose -> String
nameOfExpose expose =
    case expose of
        FunctionExpose name ->
            name

        TypeOrAliasExpose name ->
            name

        InfixExpose name ->
            name

        TypeExpose { name } ->
            name


nameOfDeclaration : Declaration -> String
nameOfDeclaration declaration =
    case declaration of
        FunctionDeclaration fun ->
            fun.declaration
                |> Node.value
                |> .name
                |> Node.value

        AliasDeclaration { name } ->
            name |> Node.value

        CustomTypeDeclaration { name } ->
            name |> Node.value

        PortDeclaration { name } ->
            name |> Node.value

        InfixDeclaration { operator } ->
            operator |> Node.value

        Destructuring _ _ ->
            -- no top-level declaration can be a destructuring declaration
            ""


isValueOrCall : Node Expression -> Maybe { name : String, range : Range }
isValueOrCall expression =
    case expression |> Node.value of
        Expression.FunctionOrValue _ name ->
            { name = name
            , range = expression |> Node.range
            }
                |> Just

        Expression.Application ((Node nameRange (Expression.FunctionOrValue _ name)) :: _ :: _) ->
            { name = name
            , range = nameRange
            }
                |> Just

        _ ->
            Nothing


typeds :
    TypeAnnotation
    -> List { name : String, range : Range }
typeds typeAnnotation =
    let
        collectTypedsInList :
            List (Node TypeAnnotation)
            -> List { name : String, range : Range }
        collectTypedsInList =
            List.concatMap (typeds << Node.value)
    in
    case typeAnnotation of
        Typed (Node range ( _, name )) arguments ->
            (arguments |> collectTypedsInList)
                |> (::) { name = name, range = range }

        Tupled tupled ->
            tupled |> collectTypedsInList

        Record fields ->
            fields
                |> List.map (\(Node _ ( _, field )) -> field)
                |> collectTypedsInList

        GenericRecord _ (Node _ fields) ->
            fields
                |> List.map (\(Node _ ( _, field )) -> field)
                |> collectTypedsInList

        FunctionTypeAnnotation arg result ->
            [ arg, result ] |> collectTypedsInList

        Unit ->
            []

        GenericType _ ->
            []


containsRange : Range -> Range -> Bool
containsRange range =
    \range_ ->
        Range.combine [ range, range_ ]
            == range_



--


{-| todo: move to Elm.Code.
-}
importsToSyntax : Imports -> List Import.Import
importsToSyntax imports =
    imports
        |> Dict.toList
        |> List.map
            (\( moduleName, { alias, exposed } ) ->
                Code.import_ moduleName
                    |> applyIfJust alias Code.withAs
                    |> applyIfJust exposed
                        (\expos ->
                            case expos of
                                Code.Exposing list ->
                                    Code.withExposing (\_ -> list)

                                Code.ExposingAll ->
                                    Code.withExposingAll
                        )
                    |> Code.importToSyntax
            )


printImports : Imports -> String
printImports imports =
    imports
        |> importsToSyntax
        |> CodeGen.prettyImports
        |> printPretty


printDeclaration :
    { imports : Imports }
    -> Generalizable specific_ Code.ModuleScopeDeclarationAny
    -> String
printDeclaration imports declaration =
    declaration
        |> declarationToDslSyntax imports
        |> CodeGen.prettyDeclaration prettyWidth
        |> printPretty


printExpressionSyntax : Expression.Expression -> String
printExpressionSyntax expressionSyntax =
    expressionSyntax
        |> CodeGen.prettyExpression
        |> printPretty


{-| Re-indent a section of generated code to ensure that it doesn't cause issues
when used as a fix.
-}
reindent : Int -> String -> String
reindent amount =
    let
        indent : String
        indent =
            String.repeat (amount - 1) " "
    in
    String.lines
        >> List.map
            (\l ->
                -- Don't indent empty lines
                if String.isEmpty l then
                    l

                else
                    indent ++ l
            )
        >> String.join "\n"
        >> String.trimLeft
