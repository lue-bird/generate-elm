module Elm.Code.Declaration exposing (..)

{-| Declarations

@docs replaceExpression, withDocumentation
@docs typeParameter


### function/value

@docs functionDeclaration, valueDeclaration
@docs withType, replaceExpressionAndArguments


### `type alias` declaration

@docs typeAliasDeclaration
@docs replaceAliasedType, replaceAliasedTypeAndArguments


### `type` declaration

@docs typeDeclaration
@docs replaceConstructors, replaceConstructorsAndArguments

-}


{-| A function declared in a `let` block.

    import Elm.Code exposing (only, in_, letFunction, field, updateFields)

    \model newName ->
        only
            (letValue "currentUser"
                (model |> access.field "user")
            )
            |> in_
                (\currentUser ->
                    model
                        |> updateFields
                            ( field "user"
                                (currentUser
                                    |> updateFields "name" newName
                                )
                            , []
                            )
                )

would generate

    let
        currentUser =
            model.user
    in
    { model | user = { currentUser | name = newName } }

See [`withType`](#withType)!

-}
letFunctionOrValue :
    String
    ->
        Suppliable
            (toResultExpression -> expression)
            (ListIs canArgumentsBeEmpty_ PatternAny)
    -> toResultExpression
    ->
        Generalizable
            (FunctionOrValueLetDeclaration expression)
            (LetDeclaration expression pattern)
letFunctionOrValue name arguments toResultExpression =
    { name = name
    , type_ = Nothing
    , arguments = arguments.code
    , result =
        toResultExpression
            |> arguments.supply
            |> any
    }
        |> generalWith FunctionOrValueLetDeclaration


letValue :
    String
    -> expression
    ->
        Generalizable
            (FunctionOrValueLetDeclaration expression)
            (LetDeclaration expression pattern)
letValue name expression =
    letFunctionOrValue name empty expression


{-| Destructuring in a `let` block.

    import Elm.Code exposing (only, in_, letDestructuring, tuple2Pattern, op, cons)

    \nonEmpty ->
        only
            (letDestructuring
                (tuple2Pattern ( var "head", var "tail" ))
                (model |> access.field "user")
            )
            |> in_
                (\head tail ->
                    head |> op cons tail
                )

would generate

    let
        ( head, tail ) =
            nonEmpty
    in
    head :: tail

See [`withType`](#withType)!

-}
letDestructuring :
    Suppliable supplyFunction_ pattern
    -> expression
    ->
        Generalizable
            (DestructuringLetDeclaration pattern expression)
            (LetDeclaration pattern expression)
letDestructuring pattern expressionToDestructure =
    ( pattern.code |> patternAny
    , expressionToDestructure |> any
    )
        |> generalWith
            (\( patternThis, expressionThis ) ->
                ( patternThis, expressionThis )
                    |> DestructuringLetDeclaration
            )



--


{-| You can generate your own documentation:

    import Elm.Code as Code
    import Elm.CodeGen exposing (emptyDocComment, markdown)
    import Elm.Generator.RecordFieldHelper as RecordFieldHelper

    accessorsWithDocumentation =
        RecordFieldHelper.accessors
            |> Generator.mapElm
                (\_ { fieldName } ->
                    Code.withDocumentation
                        (emptyDocComment
                            |> markdown
                                ("elm-accessors lens for the field `." ++ fieldName ++ "`.")
                        )
                )

-}
withDocumentation :
    DocumentationComment
    -> Generalizable (Documentable documentable) root
    -> Generalizable (Documentable documentable) root
withDocumentation docComment documentableReference =
    documentableReference
        |> Generalizable.alter
            (\documentableThis ->
                { documentableThis
                    | documentation = docComment |> Just
                }
            )



--


{-| Return the same result using different argument patterns and a different implementation.

Use [`replaceExpression`](#replaceExpression) if the arguments should stay the same.

-}
replaceExpressionAndArguments :
    Suppliable
        (Empty possiblyOrNot_ (StackFilled argument))
        (toExpression -> Expression specific_)
    -> toExpression
    ->
        Generalizable
            FunctionOrValueModuleScopeDeclaration
            ModuleScopeDeclarationAny
    ->
        Generalizable
            FunctionOrValueModuleScopeDeclaration
            ModuleScopeDeclarationAny
replaceExpressionAndArguments newParameters newExpression expressionDeclaration_ =
    expressionDeclaration_
        |> Generalizable.alter
            (\d ->
                { d
                    | parameters = newParameters.code
                    , expression =
                        newExpression
                            |> newParameters.supply
                            |> expressionAny
                }
            )



--


{-| Use a different type annotation for the generated helper.

    import Elm.Code as Code exposing (inferred, type_)

    svgDeclaration { declarationName } { svgNode } =
        Code.valueDeclaration declarationName
            (svgNode |> toSvgCode)
            |> Code.withType
                (\f -> f (inferred "msg"))
                (\msg -> type_ (from "Svg" "Svg") [ msg ])

-}
withType :
    (toType -> Generalizable specific_ TypeAny)
    -> toType
    -> Generalizable UnionTypeDeclaration ModuleScopeDeclarationAny
    -> Generalizable UnionTypeDeclaration ModuleScopeDeclarationAny
withType supplyInferredTypes newTypeAnnotation functionDeclaration_ =
    functionDeclaration_
        |> Generalizable.alter
            (\declarationThis ->
                { declarationThis
                    | type_ =
                        newTypeAnnotation
                            |> supplyInferredTypes
                            |> Just
                }
            )



--


{-| Build a `type` declaration without documentation.

    import Elm.Code as Code exposing (local, typeParameter, type_, use)

    Code.typeDeclaration "List"
        (only (typeParameter "a"))
        (\a ->
            [ ( "Nil", [] )
            , ( "Cons",
                [ use a
                , use (namedType (local "List") [ a ])
                ]
              )
            ]
        )

would generate

    type List a
        = Nil
        | Cons a (List a)

See [`typeParameter`](#typeParameter).

TODO: recursion

-}
typeUnionDeclaration :
    String
    ->
        Suppliable
            (Empty possiblyOrNever_ (StackFilled String))
            (toVariants
             ->
                StackFilled
                    ( String
                    , List (Generalizable specificType_ TypeAny)
                    )
            )
    -> toVariants
    -> Generalizable UnionTypeDeclaration ModuleScopeDeclarationAny
typeUnionDeclaration name typeParameters variants =
    { name = name
    , parameters =
        typeParameters
            |> dropSupplyFunction
            |> Stack.toList
    , variants =
        variants
            |> supplyWith typeParameters
            |> List.NonEmpty.map
                (Tuple.mapSecond (List.map toGeneral))
            |> Stack.fromTopAndBelow
    , documentation = Nothing
    }
        |> generalWith UnionTypeDeclaration



--


{-| A type parameter in a [`typeDeclaration`](#typeDeclaration) or [`typeAliasDeclaration`](#typeAliasDeclaration) declaration.

    import Elm.Code as Code exposing (type_, only, next, typeParameter)

    Code.typeAliasDeclaration "ResultMultiErr"
        (only (typeParameter "err") |> next (typeParameter "a"))
        (\err a ->
            type_ (from "Result" "Result")
                [ use (type_ (from "List.NonEmpty" "NonEmpty") [ err ])
                , use a
                ]
        )

would generate

    type alias ResultMultiErr err a =
        Result (List.NonEmpty.NonEmpty err) a

See [`typeAliasDeclaration`](#typeAliasDeclaration)

-}
typeParameter :
    String
    -> Suppliable String ((VariableType -> to) -> to)
typeParameter typeParameterName =
    typeParameterName
        |> supplying ((|>) (varType typeParameterName))


{-| Create a [`TypeAliasDeclaration`] without documentation and without imports.

    import Elm.Code as Code exposing (type_, only, next, typeParameter)

    Code.typeAliasDeclaration "ResultMultiErr"
        (only (typeParameter "err") |> next (typeParameter "a"))
        (\err a ->
            type_ (from "Result" "Result")
                [ use (type_ (from "List.NonEmpty" "NonEmpty") [ err ])
                , use a
                ]
        )

would generate

    type alias ResultMultiErr err a =
        Result (List.NonEmpty.NonEmpty err) a

See [`typeAliasDeclaration`](#typeAliasDeclaration)

-}
typeAliasDeclaration :
    String
    ->
        Suppliable
            (Empty possiblyOrNever_ (StackFilled String))
            (toType -> Generalizable specific_ TypeAny)
    -> toType
    -> Generalizable TypeAliasDeclaration ModuleScopeDeclarationAny
typeAliasDeclaration name parameters aliasedType =
    { name = name
    , documentation = Nothing
    , parameters = parameters |> dropSupplyFunction
    , aliasedType =
        aliasedType
            |> supplyWith parameters
    }
        |> generalWith TypeAliasDeclaration


{-| Use different arguments to alias a different type.

Use [`replaceAliasedType`](#replaceAliasedType) if it has the same arguments.

-}
replaceAliasedTypeAndArguments :
    Suppliable
        (Empty possiblyOrNever_ (StackFilled String))
        (toType -> Generalizable specific_ TypeAny)
    -> toType
    ->
        Generalizable
            TypeAliasDeclaration
            ModuleScopeDeclarationAny
    ->
        Generalizable
            TypeAliasDeclaration
            ModuleScopeDeclarationAny
replaceAliasedTypeAndArguments parameters aliasedType_ typeAliasDeclaration_ =
    typeAliasDeclaration_
        |> Generalizable.alter
            (\declarationValue ->
                { declarationValue
                    | parameters = parameters.code
                    , aliasedType =
                        aliasedType_
                            |> supplyWith parameters
                            |> toGeneral
                }
            )



--


{-| Create a function declaration without a documentation comment and without a type annotation.

    import Elm.Code as Code exposing (inferred, next, only, recordExtendedBy, updateFields, var)

    setterDeclaration { declarationName } { fieldName } =
        Code.functionDeclaration declarationName
            (only (var (fieldName ++ "_"))
                |> next (var "record")
            )
            (\record newValue ->
                updateFields record
                    ( field fieldName newValue, [] )
            )
            |> Code.withDocumentation
                (emptyDocComment
                    |> markdown
                        ("Setter for the field `." ++ fieldName ++ "`.")
                )
            |> Code.withType
                (\f -> f (inferred "record") (inferred fieldName))
                (type_ (from "CustomLens" "CustomLens")
                    [ use (recordExtendedBy record ( field fieldName field, [] ))
                    , use field
                    ]
                )

-}
functionOrValueModuleScopeDeclaration :
    String
    ->
        Suppliable
            (toExpression -> Expression resultSpecific_)
            (Empty possiblyOrNever_ (StackFilled PatternAny))
    -> toExpression
    ->
        Generalizable
            FunctionOrValueModuleScopeDeclaration
            ModuleScopeDeclarationAny
functionOrValueModuleScopeDeclaration name parameters expression_ =
    { name = name
    , documentation = Nothing
    , type_ = Nothing
    , parameters = parameters.code
    , expression =
        expression_
            |> parameters.supply
            |> expressionAny
    }
        |> generalWith VariableModuleScopeDeclaration



--


declarationToDslSyntax :
    { imports : Imports }
    -> Generalizable specific_ ModuleScopeDeclarationAny
    -> CodeGen.Declaration
declarationToDslSyntax imports declaration =
    case declaration |> toGeneral of
        TypeAliasDeclaration typeAliasDeclaration_ ->
            CodeGen.aliasDecl
                (typeAliasDeclaration_.documentation
                    |> Maybe.map documentationCommentToCodeGen
                )
                typeAliasDeclaration_.name
                typeAliasDeclaration_.parameters
                (typeAliasDeclaration_.aliasedType
                    |> typeToSyntax imports
                )

        UnionTypeDeclaration typeDeclaration_ ->
            CodeGen.customTypeDecl
                (typeDeclaration_.documentation
                    |> Maybe.map documentationCommentToCodeGen
                )
                typeDeclaration_.name
                typeDeclaration_.parameters
                (typeDeclaration_.variants
                    |> Stack.map
                        (Tuple.mapSecond
                            (List.map (typeToSyntax imports))
                        )
                    |> Stack.toList
                )

        VariableModuleScopeDeclaration valueDeclaration_ ->
            CodeGen.funDecl
                (valueDeclaration_.documentation
                    |> Maybe.map documentationCommentToCodeGen
                )
                (valueDeclaration_.type_
                    |> Maybe.map (typeToSyntax imports)
                )
                valueDeclaration_.name
                (valueDeclaration_.parameters
                    |> List.map (patternToSyntax imports)
                )
                (valueDeclaration_.expression
                    |> expressionToSyntax imports
                )

        PortDeclaration portDeclaration ->
            CodeGen.portDecl
                portDeclaration.name
                (portDeclaration.type_ |> typeToSyntax imports)
