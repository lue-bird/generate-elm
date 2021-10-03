module SyntaxExtra exposing (containsRange, isValueOrCall, joinAndSortImports, nameOfDeclaration, nameOfExpose, printImports, printPretty, reindent, typeds)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.CodeGen as CodeGen
import Elm.Pretty as CodeGen
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose(..))
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import List.Extra as List
import Pretty exposing (pretty)
import Util exposing (firstJust, fromNonempty)


{-| [`Elm.Syntax.ModuleName`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-ModuleName#ModuleName)
and [`Elm.CodeGen.ModuleName`](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest/Elm-CodeGen#ModuleName)
describe an optional module name.
This will change in the next `elm-syntax` version: [issue about `ModuleName`](https://github.com/stil4m/elm-syntax/issues/70)

Until then, we'll just use this :)

-}
type alias ModuleName =
    ( String, List String )


{-| [`Import`](Elm-Generator#Import)s by imported module name.
-}
type alias Imports =
    Dict
        ModuleName
        { alias : Maybe String
        , exposed : Maybe CodeGen.Exposing
        }


joinAndSortImports :
    List
        { name : ( String, List String )
        , alias : Maybe String
        , exposed : Maybe CodeGen.Exposing
        }
    ->
        Dict
            ( String, List String )
            { alias : Maybe String
            , exposed : Maybe CodeGen.Exposing
            }
joinAndSortImports =
    List.map
        (\{ name, alias, exposed } ->
            ( name, { alias = alias, exposed = exposed } )
        )
        >> Dict.fromListDedupe
            (\a b ->
                { alias = firstJust [ a.alias, b.alias ]
                , exposed =
                    case ( a.exposed, b.exposed ) of
                        ( Nothing, bExposed ) ->
                            bExposed

                        ( aExposed, Nothing ) ->
                            aExposed

                        ( Just aExposed, Just bExposed ) ->
                            joinExposings aExposed bExposed
                                |> Just
                }
            )


joinExposings :
    CodeGen.Exposing
    -> CodeGen.Exposing
    -> CodeGen.Exposing
joinExposings aExposed bExposed =
    case ( aExposed, bExposed ) of
        ( Exposing.All _, _ ) ->
            CodeGen.exposeAll

        ( _, Exposing.All _ ) ->
            CodeGen.exposeAll

        ( Exposing.Explicit aExposedList, Exposing.Explicit bExposedList ) ->
            CodeGen.exposeExplicit
                ((aExposedList ++ bExposedList)
                    |> List.foldl
                        (\(Node _ expose) soFar ->
                            let
                                exposeName =
                                    nameOfExpose expose

                                existingExposeWithSameName : Maybe CodeGen.TopLevelExpose
                                existingExposeWithSameName =
                                    soFar
                                        |> List.filter
                                            (nameOfExpose >> (==) exposeName)
                                        |> List.head
                            in
                            case ( expose, existingExposeWithSameName ) of
                                ( TypeExpose { open }, Just (TypeExpose _) ) ->
                                    case open of
                                        Just _ ->
                                            soFar
                                                |> List.updateIf
                                                    (nameOfExpose >> (==) exposeName)
                                                    (\_ -> expose)

                                        Nothing ->
                                            soFar

                                ( _, Just _ ) ->
                                    soFar

                                ( _, Nothing ) ->
                                    expose :: soFar
                        )
                        []
                )


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


printPretty : Pretty.Doc -> String
printPretty =
    pretty 100


printImports :
    Imports
    -> String
printImports =
    Dict.toList
        >> List.map
            (\( moduleName, { alias, exposed } ) ->
                CodeGen.importStmt
                    (moduleName |> fromNonempty)
                    (alias |> Maybe.map List.singleton)
                    exposed
            )
        -- ↓ automatically de-duplicates and sorts imports
        >> CodeGen.prettyImports
        >> printPretty


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
