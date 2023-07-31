module Review.Generate.Internal exposing (Config, DeclarationInsertLocation(..), InDifferentModuleConfig, InSameModuleConfig, InsertLocationInDifferentModule, InsertLocationInSameModule(..), duplicateMarkerError, errorInArguments, expressionGenerationRequestedError, inModule, inSameModule, missingDeclarationError, missingImportFromGeneratingModule, missingMarkerError, missingModuleError, replaceStub, rule)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.Code as Code exposing (Code, Exposing, Imports, ModuleOrigin(..), ModuleScopeDeclarationAny, Origin, declarationToDslSyntax, expressionToSyntax, implicitImports, importsFromList, moduleNameFromSyntax, originLookupByModules, syntaxToExposingIn)
import Elm.Code.Generator as Generator exposing (AllowsUnqualified, DeclarationGenerator, ExpressionGenerator, RequireQualified(..))
import Elm.Code.Module as DocsModule
import Elm.CodeGen as CodeGen
import Elm.Docs as Docs
import Elm.Pretty as CodeGen
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Generalizable exposing (Generalizable, toGeneral)
import List.Extra as List
import List.NonEmpty
import List.Nonempty
import Misc exposing (dictKeys, firstJust, mapAccumMultipleL, prettyWidth, printPretty, toNonempty, updateFirstChar)
import Pretty exposing (pretty)
import ResultME
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency
import Review.Rule as Rule exposing (ModuleKey, ModuleRuleSchema, ProjectRuleSchema, Rule)
import Set exposing (Set)
import SyntaxExtra exposing (containsRange, isValueOrCall, nameOfDeclaration, nameOfExpose, printDeclaration, printExpressionSyntax, printImports, reindent, typeds)
import Typed exposing (val)


rule : Config information_ requireQualified_ declaration_ -> Rule
rule config =
    case config of
        ReplaceStub replaceStubConfig ->
            generateReplacingStubs replaceStubConfig

        InSameModule inSameModuleConfig ->
            generateInSameModule inSameModuleConfig

        InDifferentModule inDifferentModuleConfig ->
            generateInDifferentModule inDifferentModuleConfig


type alias GenerateReplacingStubProjectContext =
    { inModules :
        Dict
            String
            (GenerateReplacingStubModuleContext { key : ModuleKey })
    , exposedFromModules :
        Dict String (Set String)
    }


type alias GenerateReplacingStubModuleContext more =
    { more
        | exposed : Code.Exposing
        , beforeImports : Location
        , imports : Imports
        , declarations :
            Dict String (Node Declaration.Declaration)
        , stubs :
            List
                { arguments : List Expression
                , range : Range
                }
    }


generateReplacingStubs : ReplaceStubConfig information_ -> Rule
generateReplacingStubs { generator, stub } =
    let
        initialProjectContext : GenerateReplacingStubProjectContext
        initialProjectContext =
            { inModules = Dict.empty
            , exposedFromModules = Dict.empty
            }

        initialModuleContext : GenerateReplacingStubModuleContext {}
        initialModuleContext =
            { beforeImports = { row = 2, column = 1 }
            , imports = Dict.empty
            , declarations = Dict.empty
            , stubs = []
            }

        moduleToProjectContext :
            String
            -> ModuleKey
            -> GenerateReplacingStubModuleContext {}
            -> GenerateReplacingStubProjectContext
        moduleToProjectContext moduleName moduleKey { declarations, stubs, beforeImports, imports } =
            { inModules =
                Dict.singleton moduleName
                    { key = moduleKey
                    , beforeImports = beforeImports
                    , imports = imports
                    , declarations = declarations
                    , stubs = stubs
                    }
            , exposed = Debug.todo ""
            }

        foldProjectContexts :
            GenerateReplacingStubProjectContext
            -> GenerateReplacingStubProjectContext
            -> GenerateReplacingStubProjectContext
        foldProjectContexts a b =
            { inModules = Dict.union a.inModules b.inModules
            }

        finalEvaluation :
            GenerateReplacingStubProjectContext
            -> List (Rule.Error { useErrorForModule : () })
        finalEvaluation { inModules } =
            inModules
                |> Dict.values
                |> List.concatMap
                    (\module_ ->
                        let
                            originLookup =
                                Code.originLookupByModules (Debug.todo "") module_.imports

                            locate =
                                Code.locateOrigin originLookupByModules

                            tryGenerating :
                                { module_
                                    | arguments : List Expression
                                    , range : Range
                                }
                                -> GenerateReplacingStubModuleContext { key : ModuleKey }
                                -> List (Rule.Error scope_)
                            tryGenerating functionOrValue { key, beforeImports, imports } =
                                case
                                    functionOrValue.arguments
                                        |> List.map
                                            (syntaxToExpression { locate = locate })
                                        |> ResultME.combineList
                                        |> ResultME.andThen generator.when
                                of
                                    Err errors ->
                                        [ Rule.errorForModule key
                                            (errorInArguments
                                                { description = generator.description
                                                , errors = errors |> List.Nonempty.toList
                                                }
                                            )
                                            functionOrValue.range
                                        ]

                                    Ok information ->
                                        let
                                            gen =
                                                generate generator.what
                                                    { declarationName = stub
                                                    , imports = imports
                                                    }
                                                    information
                                        in
                                        [ Rule.errorForModuleWithFix key
                                            (expressionGenerationRequestedError
                                                { description = generator.description }
                                            )
                                            functionOrValue.range
                                            ([ [ Fix.replaceRangeBy functionOrValue.range
                                                    (gen.code
                                                        |> expressionToSyntax { imports = gen.imports }
                                                        |> printExpressionSyntax
                                                        |> reindent functionOrValue.range.start.column
                                                    )
                                               ]
                                             , gen.newImports
                                                |> insertImportsFixAt beforeImports
                                             ]
                                                |> List.concat
                                            )
                                        ]
                        in
                        module_.stubs
                            |> List.concatMap
                                (\stub_ ->
                                    tryGenerating stub_ module_
                                )
                    )
    in
    Rule.newProjectRuleSchema "Review.Generate"
        initialProjectContext
        |> Rule.withModuleVisitor
            (withModuleCommentVisitor
                (\(Node { end } _) context ->
                    ( []
                    , { context | beforeImports = end }
                    )
                )
                >> withImportVisitor
                    (\{ moduleName, alias, exposed } context ->
                        ( []
                        , { context
                            | imports =
                                context.imports
                                    |> Dict.insert moduleName
                                        { alias = alias, exposed = exposed }
                          }
                        )
                    )
                >> Rule.withExpressionExitVisitor
                    (\(Node expressionRange expression) context ->
                        ( []
                        , let
                            isStub :
                                Maybe
                                    { arguments : List Expression
                                    , range : Range
                                    }
                            isStub =
                                let
                                    isStubWithArgs :
                                        String
                                        -> arguments
                                        -> Maybe { arguments : arguments, range : Range }
                                    isStubWithArgs name arguments =
                                        if name == stub then
                                            { arguments = arguments
                                            , range = expressionRange
                                            }
                                                |> Just

                                        else
                                            Nothing
                                in
                                case expression of
                                    Expression.FunctionOrValue [] name ->
                                        isStubWithArgs name []

                                    Expression.Application ((Node _ (Expression.FunctionOrValue [] name)) :: arguments) ->
                                        isStubWithArgs name (arguments |> List.map Node.value)

                                    _ ->
                                        Nothing
                          in
                          case isStub of
                            Just newStub ->
                                { context
                                    | stubs =
                                        context.stubs
                                            |> List.filterNot
                                                (\s ->
                                                    newStub.range |> containsRange s.range
                                                )
                                            |> (::) newStub
                                }

                            Nothing ->
                                context
                        )
                    )
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule =
                Rule.initContextCreator
                    (\_ -> initialModuleContext)
            , fromModuleToProject =
                Rule.initContextCreator
                    (\metadata ->
                        moduleToProjectContext
                            (moduleNameFromMetadata metadata)
                    )
                    |> Rule.withMetadata
                    |> Rule.withModuleKey
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


generate :
    ({ declarationName : String }
     -> information
     -> Generalizable declaration ModuleScopeDeclarationAny
    )
    -> { declarationName : String, imports : Imports }
    -> information
    ->
        { code : general
        , imports : Imports
        , newImports : Imports
        }
generate codeGenerator { declarationName, imports } information =
    let
        existingImports : Imports
        existingImports =
            Dict.union imports implicitImports

        generated : Code general
        generated =
            codeGenerator { declarationName = declarationName } information
                |> toGeneral

        newImports : Imports
        newImports =
            -- todo: add exposed, but don't use preferred alias
            Dict.diff generated.imports existingImports
    in
    { code = generated.code
    , imports = Dict.union existingImports newImports
    , newImports = newImports
    }


type alias GenerateInSameModuleBaseContext more =
    { more
        | beforeImports : Location
        , imports : Imports
        , declarations : Dict String (Node Declaration.Declaration)
        , markerRanges : List Range
        , generationRequested :
            Dict String { range : Range }
    }


type alias GenerateInSameModuleModuleContext =
    GenerateInSameModuleBaseContext
        { moduleNameAt : Range -> Maybe (List String) }


type alias GenerateInSameModuleProjectContext =
    { inModules :
        Dict
            String
            (GenerateInSameModuleBaseContext { key : ModuleKey })
    , exposedFromDependencies :
        Dict ModuleName (Set String)
    }


generateInSameModule :
    InSameModuleConfig information_ declaration_
    -> Rule
generateInSameModule config =
    -- todo: synchronize
    let
        { generator } =
            config

        initialProjectContext : GenerateInSameModuleProjectContext
        initialProjectContext =
            { inModules = Dict.empty
            , exposedFromDependencies = Dict.empty
            }

        initialModuleContext :
            ModuleNameLookupTable
            -> GenerateInSameModuleModuleContext
        initialModuleContext moduleNameLookupTable =
            { beforeImports = { row = 2, column = 1 }
            , imports = Dict.empty
            , declarations = Dict.empty
            , markerRanges = []
            , generationRequested = Dict.empty
            , moduleNameAt =
                ModuleNameLookupTable.moduleNameAt
                    moduleNameLookupTable
            }

        moduleToProjectContext :
            ModuleKey
            -> ModuleName
            -> GenerateInSameModuleModuleContext
            -> GenerateInSameModuleProjectContext
        moduleToProjectContext moduleKey moduleName moduleContext =
            { inModules =
                Dict.singleton moduleName
                    { key = moduleKey
                    , beforeImports = moduleContext.beforeImports
                    , imports = moduleContext.imports
                    , declarations = moduleContext.declarations
                    , markerRanges = moduleContext.markerRanges
                    , generationRequested = moduleContext.generationRequested
                    }
            , exposedFromDependencies = Dict.empty
            }

        foldProjectContexts :
            GenerateInSameModuleProjectContext
            -> GenerateInSameModuleProjectContext
            -> GenerateInSameModuleProjectContext
        foldProjectContexts aContext bContext =
            { inModules =
                Dict.union aContext.inModules bContext.inModules
            , exposedFromDependencies =
                Dict.union aContext.exposedFromDependencies
                    bContext.exposedFromDependencies
            }

        finalEvaluation : GenerateInSameModuleProjectContext -> List (Rule.Error e_)
        finalEvaluation { inModules, exposedFromDependencies } =
            let
                checkIfShouldGenerate :
                    GenerateInSameModuleBaseContext { key : ModuleKey }
                    ->
                        { declarationName : String
                        , range : Range
                        }
                    -> List (Rule.Error errorScope_)
                checkIfShouldGenerate module_ generationRequest =
                    case
                        generator.when.checkName
                            { declarations =
                                module_.declarations
                                    |> Dict.map (\_ -> Node.value)

                            {- , locate =
                               locateOriginLookup
                                   (Dict.union
                                       exposedFromDependencies
                                       (inModules
                                           |> Dict.map (\_ -> .declarations >> dictKeys)
                                       )
                                   )
                                   module_.imports
                            -}
                            }
                            generationRequest.declarationName
                    of
                        Just (Ok information) ->
                            generateDeclarationFix
                                module_
                                generationRequest
                                information

                        Just (Err errors) ->
                            [ Rule.errorForModule module_.key
                                (errorsInContext
                                    { description = generator.description
                                    , errors = errors |> List.Nonempty.toList
                                    }
                                )
                                generationRequest.range
                            ]

                        Nothing ->
                            []

                generateDeclarationFix :
                    GenerateInSameModuleBaseContext { key : ModuleKey }
                    -> { declarationName : String, range : Range }
                    -> information_
                    -> List (Rule.Error scope_)
                generateDeclarationFix module_ { declarationName, range } information =
                    let
                        gen =
                            generate generator.elm
                                { declarationName = declarationName
                                , imports = module_.imports
                                }
                                information

                        afterDeclarations : () -> Location
                        afterDeclarations () =
                            module_.declarations
                                |> Dict.values
                                |> List.map Node.range
                                |> Range.combine
                                |> .end

                        generateDeclaration : Location -> List (Rule.Error scope_)
                        generateDeclaration insertLocation =
                            [ Rule.errorForModuleWithFix
                                module_.key
                                (missingDeclarationError declarationName
                                    { description = generator.description }
                                )
                                range
                                ([ [ Fix.insertAt insertLocation
                                        ([ "\n\n\n"
                                         , { code = gen.code, imports = Dict.empty }
                                            |> top
                                            |> printDeclaration { imports = gen.imports }
                                         ]
                                            |> String.concat
                                        )
                                   ]
                                 , gen.newImports
                                    |> insertImportsFixAt module_.beforeImports
                                 ]
                                    |> List.concat
                                )
                            ]
                    in
                    case config.insertLocation of
                        InsertBelowAllDeclarations ->
                            generateDeclaration (afterDeclarations ())

                        InsertBelowMarker marker ->
                            case module_.markerRanges of
                                [ markerRange ] ->
                                    generateDeclaration markerRange.end

                                [] ->
                                    [ Rule.errorForModuleWithFix
                                        module_.key
                                        (missingMarkerError marker
                                            { for = generator.description }
                                        )
                                        range
                                        [ Fix.insertAt (afterDeclarations ())
                                            ([ "\n\n\n-- ", marker, "\n" ]
                                                |> String.concat
                                            )
                                        ]
                                    ]

                                first :: second :: more ->
                                    (first :: second :: more)
                                        |> List.map
                                            (Rule.errorForModule
                                                module_.key
                                                (duplicateMarkerError marker
                                                    { for = generator.description }
                                                )
                                            )

                        InsertBelowDeclarations declarationsAboveFor ->
                            let
                                declarationsAbove =
                                    declarationsAboveFor information

                                insertLocation =
                                    module_.declarations
                                        |> Dict.values
                                        |> List.filter
                                            (\(Node _ decl) ->
                                                declarationsAbove
                                                    |> List.member (nameOfDeclaration decl)
                                            )
                                        |> List.map Node.range
                                        |> Range.combine
                                        |> .end
                            in
                            generateDeclaration insertLocation

                        OtherInsertLocation AfterDeclarationItWasReferencedIn ->
                            let
                                insertLocation =
                                    module_.declarations
                                        |> Dict.values
                                        |> List.filter
                                            (\(Node declRange _) ->
                                                declRange |> containsRange range
                                            )
                                        |> List.map Node.range
                                        |> Range.combine
                                        |> .end
                            in
                            generateDeclaration insertLocation
            in
            inModules
                |> Dict.values
                |> List.concatMap
                    (\module_ ->
                        Dict.diff
                            module_.generationRequested
                            module_.declarations
                            |> Dict.toList
                            |> List.concatMap
                                (\( nameOfDeclarationToGenerate, declarationToGenerate ) ->
                                    checkIfShouldGenerate module_
                                        { declarationName = nameOfDeclarationToGenerate
                                        , range = declarationToGenerate.range
                                        }
                                )
                    )
    in
    Rule.newProjectRuleSchema "Review.Generate"
        initialProjectContext
        |> withDependencyModulesProjectVisitor exposedFromDependenciesVisitor
        |> Rule.withModuleVisitor
            (withModuleCommentVisitor
                (\(Node { end } _) context ->
                    ( []
                    , { context | beforeImports = end }
                    )
                )
                >> Rule.withCommentsVisitor
                    (\comments context ->
                        ( []
                        , case config.insertLocation of
                            InsertBelowMarker marker ->
                                { context
                                    | markerRanges =
                                        comments
                                            |> List.filter
                                                (\(Node _ comment) ->
                                                    (comment |> String.dropLeft 3)
                                                        == marker
                                                )
                                            |> List.map Node.range
                                }

                            _ ->
                                context
                        )
                    )
                >> withImportVisitor
                    (\{ moduleName, alias, exposed } context ->
                        ( []
                        , { context
                            | imports =
                                context.imports
                                    |> Dict.insert moduleName
                                        { alias = alias, exposed = exposed }
                          }
                        )
                    )
                >> withTopLevelDeclarationListVisitor
                    (\declarations context ->
                        ( []
                        , { context | declarations = declarations }
                        )
                    )
                >> Rule.withExpressionEnterVisitor
                    (\expression context ->
                        ( []
                        , isValueOrCall expression
                            |> Maybe.map
                                (\{ name, range } ->
                                    case context.moduleNameAt range of
                                        Just [] ->
                                            { context
                                                | generationRequested =
                                                    context.generationRequested
                                                        |> Dict.insert name { range = range }
                                            }

                                        _ ->
                                            context
                                )
                            |> Maybe.withDefault context
                        )
                    )
                >> withTypeAnnotationVisitor
                    (\(Node _ typeAnnotation) context ->
                        ( []
                        , { context
                            | generationRequested =
                                typeds typeAnnotation
                                    |> List.foldl
                                        (\{ name, range } ->
                                            case context.moduleNameAt range of
                                                Nothing ->
                                                    Dict.insert name { range = range }

                                                Just _ ->
                                                    identity
                                        )
                                        context.generationRequested
                          }
                        )
                    )
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule =
                Rule.initContextCreator
                    (\moduleNameLookupTable _ ->
                        initialModuleContext moduleNameLookupTable
                    )
                    |> Rule.withModuleNameLookupTable
            , fromModuleToProject =
                Rule.initContextCreator
                    (\key metadata ->
                        moduleToProjectContext key
                            (moduleNameFromMetadata metadata)
                    )
                    |> Rule.withModuleKey
                    |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


exposedFromDependenciesVisitor :
    Dict
        ModuleName
        { unions : List Docs.Union
        , aliases : List Docs.Alias
        , values : List Docs.Value
        , binops : List Docs.Binop
        }
    ->
        { projectContext
            | exposedFromDependencies :
                Dict ModuleName (Set String)
        }
    ->
        ( List (Rule.Error { useErrorForModule : () })
        , { projectContext
            | exposedFromDependencies :
                Dict ModuleName (Set String)
          }
        )
exposedFromDependenciesVisitor dependencies context =
    ( []
    , { context
        | exposedFromDependencies =
            dependencies
                |> Dict.map
                    (\_ { unions, aliases, values, binops } ->
                        [ unions |> List.map .name
                        , aliases |> List.map .name
                        , values |> List.map .name
                        , binops |> List.map .name
                        ]
                            |> List.concat
                            |> Set.fromList
                    )
      }
    )


type alias GenerateInDifferentModuleProjectContext =
    { modulesNotToGenerateIn :
        Dict
            ModuleName
            (DifferentModule
                { key : ModuleKey
                , generationRequested :
                    Dict
                        String
                        { range : Range, qualified : Qualified }
                }
            )
    , moduleToGenerateIn :
        Maybe
            (DifferentModule
                (ModuleToGenerateInInformation
                    { key : ModuleKey
                    , moduleNameRange : Range
                    }
                )
            )
    , exposed : Dict ModuleName (Set String)
    }


type alias ModuleToGenerateInInformation more =
    { more
        | exposing_ : Exposing
        , imports : Imports
        , markerRanges : List Range
    }


type alias DifferentModule more =
    { more
        | beforeImports : Location
        , declarations : Dict String (Node Declaration.Declaration)
    }


type alias GenerateInDifferentModuleModuleContext =
    DifferentModule
        { moduleNameAt : Range -> Maybe (List String)
        , kind : GenerateInDifferentModuleModuleContextKind
        }


type GenerateInDifferentModuleModuleContextKind
    = ModuleToGenerateIn (ModuleToGenerateInInformation {})
    | ModuleNotToGenerateIn
        { generationRequested :
            Dict
                String
                { range : Range, qualified : Qualified }
        }


type Qualified
    = Qualified
    | Unqualified


generateInDifferentModule :
    InDifferentModuleConfig requireQualified_ information_ declaration_
    -> Rule
generateInDifferentModule config =
    let
        nameOfModuleToGenerateIn =
            config.name

        { generator } =
            config

        initialProjectContext : GenerateInDifferentModuleProjectContext
        initialProjectContext =
            { modulesNotToGenerateIn = Dict.empty
            , moduleToGenerateIn = Nothing
            , exposed = Dict.empty
            }

        moduleFromProjectContext :
            ModuleName
            -> ModuleNameLookupTable
            -> GenerateInDifferentModuleProjectContext
            -> GenerateInDifferentModuleModuleContext
        moduleFromProjectContext moduleName moduleNameLookupTable _ =
            { kind =
                if moduleName == nameOfModuleToGenerateIn then
                    ModuleToGenerateIn
                        { exposing_ = Exposing.Explicit []
                        , imports = Dict.empty
                        , markerRanges = []
                        }

                else
                    ModuleNotToGenerateIn
                        { generationRequested = Dict.empty }
            , beforeImports = { row = 2, column = 1 }
            , declarations = Dict.empty
            , moduleNameAt =
                ModuleNameLookupTable.moduleNameAt
                    moduleNameLookupTable
            }

        moduleToProjectContext :
            Node ModuleName
            -> ModuleKey
            -> GenerateInDifferentModuleModuleContext
            -> GenerateInDifferentModuleProjectContext
        moduleToProjectContext (Node moduleNameRange moduleName) moduleKey moduleContext =
            case moduleContext.kind of
                ModuleToGenerateIn { exposing_, markerRanges, imports } ->
                    { modulesNotToGenerateIn = Dict.empty
                    , moduleToGenerateIn =
                        { key = moduleKey
                        , moduleNameRange = moduleNameRange
                        , exposing_ = exposing_
                        , beforeImports = moduleContext.beforeImports
                        , imports = imports
                        , declarations = moduleContext.declarations
                        , markerRanges = markerRanges
                        }
                            |> Just
                    , exposed = Dict.empty
                    }

                ModuleNotToGenerateIn { generationRequested } ->
                    { modulesNotToGenerateIn =
                        Dict.singleton moduleName
                            { key = moduleKey
                            , beforeImports = moduleContext.beforeImports
                            , declarations = moduleContext.declarations
                            , generationRequested = generationRequested
                            }
                    , moduleToGenerateIn = Nothing
                    , exposed = Dict.empty
                    }

        foldProjectContexts :
            GenerateInDifferentModuleProjectContext
            -> GenerateInDifferentModuleProjectContext
            -> GenerateInDifferentModuleProjectContext
        foldProjectContexts a b =
            { modulesNotToGenerateIn =
                Dict.union a.modulesNotToGenerateIn
                    b.modulesNotToGenerateIn
            , moduleToGenerateIn =
                [ a, b ]
                    |> List.map .moduleToGenerateIn
                    |> firstJust
            , exposed =
                Dict.union a.exposed
                    b.exposed
            }

        finalEvaluation :
            GenerateInDifferentModuleProjectContext
            -> List (Rule.Error e_)
        finalEvaluation { modulesNotToGenerateIn, moduleToGenerateIn, exposed } =
            let
                generateIfShould :
                    DifferentModule
                        (ModuleToGenerateInInformation
                            { key : ModuleKey
                            , moduleNameRange : Range
                            }
                        )
                    ->
                        { declarationName : String
                        , range : Range
                        , qualified : Qualified
                        , moduleThatRequestsGeneration : ModuleKey
                        }
                    -> List (Rule.Error errorScope_)
                generateIfShould existingModuleToGenerateIn { declarationName, qualified, range, moduleThatRequestsGeneration } =
                    let
                        parseNameAndGenerateIfShould : List (Rule.Error scope_)
                        parseNameAndGenerateIfShould =
                            case
                                generator.when.checkName
                                    { declarations =
                                        existingModuleToGenerateIn.declarations
                                            |> Dict.map (\_ -> Node.value)

                                    {- , locate =
                                       locateOriginLookup
                                           (Dict.union
                                               exposedFromDependencies
                                               (modulesNotToGenerateIn
                                                   |> Dict.map (\_ -> .declarations >> dictKeys)
                                               )
                                           )
                                           existingModuleToGenerateIn.imports
                                    -}
                                    }
                                    declarationName
                            of
                                Just (Ok information) ->
                                    generateDeclarationFix
                                        existingModuleToGenerateIn
                                        { declarationName = declarationName }
                                        information

                                Just (Err errors) ->
                                    [ Rule.errorForModule moduleThatRequestsGeneration
                                        (errorsInContext
                                            { description = generator.description
                                            , errors = errors |> List.Nonempty.toList
                                            }
                                        )
                                        range
                                    ]

                                Nothing ->
                                    []
                    in
                    case qualified of
                        Qualified ->
                            parseNameAndGenerateIfShould

                        Unqualified ->
                            case generator.when.requireQualified |> val of
                                AllowsUnqualified ->
                                    [ parseNameAndGenerateIfShould
                                    , [ Rule.errorForModuleWithFix
                                            moduleThatRequestsGeneration
                                            (missingImportFromGeneratingModule declarationName
                                                { description = generator.description }
                                            )
                                            range
                                            (Code.import_ nameOfModuleToGenerateIn
                                                |> Code.withExposing (Code.addValueExposes [ declarationName ])
                                                |> List.singleton
                                                |> importsFromList
                                                |> insertImportsFixAt
                                                    existingModuleToGenerateIn.beforeImports
                                            )
                                      ]
                                    ]
                                        |> List.concat

                                RequiresQualified ->
                                    []

                generateDeclarationFix :
                    DifferentModule
                        (ModuleToGenerateInInformation
                            { key : ModuleKey
                            , moduleNameRange : Range
                            }
                        )
                    -> { declarationName : String }
                    -> information_
                    -> List (Rule.Error scope_)
                generateDeclarationFix existingModuleToGenerateIn { declarationName } information =
                    let
                        afterDeclarations : () -> Location
                        afterDeclarations () =
                            existingModuleToGenerateIn.declarations
                                |> Dict.values
                                |> List.map Node.range
                                |> Range.combine
                                |> .end

                        generateDeclaration : Location -> List (Rule.Error scope_)
                        generateDeclaration insertLocation =
                            [ Rule.errorForModuleWithFix
                                existingModuleToGenerateIn.key
                                (missingDeclarationError declarationName
                                    { description = generator.description }
                                )
                                existingModuleToGenerateIn.moduleNameRange
                                (let
                                    { code, newImports, imports } =
                                        generate generator.elm
                                            { declarationName = declarationName
                                            , imports = existingModuleToGenerateIn.imports
                                            }
                                            information
                                 in
                                 [ [ Fix.insertAt insertLocation
                                        ([ "\n\n\n"
                                         , { code = code, imports = Dict.empty }
                                            |> top
                                            |> printDeclaration { imports = imports }
                                         ]
                                            |> String.concat
                                        )
                                   ]
                                 , newImports
                                    |> insertImportsFixAt
                                        existingModuleToGenerateIn.beforeImports
                                 , case existingModuleToGenerateIn.exposing_ of
                                    Exposing.Explicit explicitExposing ->
                                        [ Fix.insertAt
                                            (explicitExposing
                                                |> List.map Node.range
                                                |> Range.combine
                                                |> .end
                                            )
                                            (", " ++ declarationName)
                                        ]

                                    _ ->
                                        []
                                 ]
                                    |> List.concat
                                )
                            ]
                    in
                    case config.insertLocation of
                        InsertBelowAllDeclarations ->
                            generateDeclaration (afterDeclarations ())

                        InsertBelowMarker marker ->
                            case existingModuleToGenerateIn.markerRanges of
                                [ markerRange ] ->
                                    generateDeclaration markerRange.end

                                [] ->
                                    [ Rule.errorForModuleWithFix
                                        existingModuleToGenerateIn.key
                                        (missingMarkerError marker
                                            { for = generator.description }
                                        )
                                        existingModuleToGenerateIn.moduleNameRange
                                        [ Fix.insertAt (afterDeclarations ())
                                            ([ "\n\n\n-- ", marker, "\n" ]
                                                |> String.concat
                                            )
                                        ]
                                    ]

                                first :: second :: more ->
                                    (first :: second :: more)
                                        |> List.map
                                            (\range ->
                                                Rule.errorForModule
                                                    existingModuleToGenerateIn.key
                                                    (duplicateMarkerError marker
                                                        { for = generator.description }
                                                    )
                                                    range
                                            )

                        InsertBelowDeclarations declarationsAboveFor ->
                            let
                                declarationsAbove =
                                    declarationsAboveFor information

                                insertLocation =
                                    existingModuleToGenerateIn.declarations
                                        |> Dict.values
                                        |> List.filter
                                            (\(Node _ decl) ->
                                                declarationsAbove
                                                    |> List.member (nameOfDeclaration decl)
                                            )
                                        |> List.map Node.range
                                        |> Range.combine
                                        |> .end
                            in
                            generateDeclaration insertLocation

                        OtherInsertLocation (NoLocationSpecificToDifferentModule ever) ->
                            never ever
            in
            case moduleToGenerateIn of
                Just existingModuleToGenerateIn ->
                    modulesNotToGenerateIn
                        |> Dict.values
                        |> List.concatMap
                            (\{ generationRequested, key } ->
                                Dict.diff
                                    generationRequested
                                    existingModuleToGenerateIn.declarations
                                    |> Dict.toList
                                    |> List.concatMap
                                        (\( declarationName, generationRequest ) ->
                                            generateIfShould existingModuleToGenerateIn
                                                { declarationName = declarationName
                                                , range = generationRequest.range
                                                , moduleThatRequestsGeneration = key
                                                , qualified = generationRequest.qualified
                                                }
                                        )
                            )

                Nothing ->
                    -- maybe instead use a range where it was referenced in
                    [ Rule.globalError
                        (missingModuleError nameOfModuleToGenerateIn
                            { oneDeclarationKind = generator.description }
                        )
                    ]

        isFromModuleToGenerateIn :
            (Range -> Maybe (List String))
            -> Range
            -> Maybe Qualified
        isFromModuleToGenerateIn moduleNameAt range =
            case moduleNameAt range of
                Just (moduleNameHead :: moduleNameRest) ->
                    if
                        ((moduleNameHead :: moduleNameRest)
                            |> moduleNameFromSyntax
                        )
                            == nameOfModuleToGenerateIn
                    then
                        Qualified |> Just

                    else
                        Nothing

                Just [] ->
                    Unqualified |> Just

                Nothing ->
                    Nothing
    in
    Rule.newProjectRuleSchema "Review.Generate"
        initialProjectContext
        |> withDependencyModulesProjectVisitor exposedFromDependenciesVisitor
        |> Rule.withModuleVisitor
            (Rule.withModuleDefinitionVisitor
                (\(Node _ moduleDefinition) moduleContext ->
                    ( []
                    , { moduleContext
                        | kind =
                            case moduleContext.kind of
                                ModuleToGenerateIn moduleToGenerateIn ->
                                    case moduleDefinition of
                                        NormalModule { exposingList } ->
                                            { moduleToGenerateIn
                                                | exposing_ = exposingList |> Node.value
                                            }
                                                |> ModuleToGenerateIn

                                        _ ->
                                            moduleContext.kind

                                ModuleNotToGenerateIn _ ->
                                    moduleContext.kind
                      }
                    )
                )
                >> withModuleCommentVisitor
                    (\(Node { end } _) moduleContext ->
                        ( []
                        , { moduleContext | beforeImports = end }
                        )
                    )
                >> Rule.withCommentsVisitor
                    (\comments context ->
                        ( []
                        , case ( context.kind, config.insertLocation ) of
                            ( ModuleToGenerateIn moduleToGenerateIn, InsertBelowMarker marker ) ->
                                { context
                                    | kind =
                                        { moduleToGenerateIn
                                            | markerRanges =
                                                comments
                                                    |> List.filter
                                                        (\(Node _ comment) ->
                                                            (comment |> String.dropLeft 3)
                                                                == marker
                                                        )
                                                    |> List.map Node.range
                                        }
                                            |> ModuleToGenerateIn
                                }

                            _ ->
                                context
                        )
                    )
                >> withImportVisitor
                    (\{ moduleName, alias, exposed } context ->
                        ( []
                        , case context.kind of
                            ModuleToGenerateIn moduleToGenerateIn ->
                                { context
                                    | kind =
                                        { moduleToGenerateIn
                                            | imports =
                                                moduleToGenerateIn.imports
                                                    |> Dict.insert moduleName
                                                        { alias = alias, exposed = exposed }
                                        }
                                            |> ModuleToGenerateIn
                                }

                            ModuleNotToGenerateIn _ ->
                                context
                        )
                    )
                >> withTopLevelDeclarationListVisitor
                    (\declarations moduleContext ->
                        ( []
                        , { moduleContext | declarations = declarations }
                        )
                    )
                >> Rule.withExpressionEnterVisitor
                    (\expression context ->
                        ( []
                        , case context.kind of
                            ModuleNotToGenerateIn notFieldHelpersModuleContext ->
                                isValueOrCall expression
                                    |> Maybe.andThen
                                        (\{ range, name } ->
                                            isFromModuleToGenerateIn context.moduleNameAt range
                                                |> Maybe.map
                                                    (\qualified ->
                                                        { context
                                                            | kind =
                                                                { notFieldHelpersModuleContext
                                                                    | generationRequested =
                                                                        notFieldHelpersModuleContext.generationRequested
                                                                            |> Dict.insert name
                                                                                { qualified = qualified, range = range }
                                                                }
                                                                    |> ModuleNotToGenerateIn
                                                        }
                                                    )
                                        )
                                    |> Maybe.withDefault context

                            ModuleToGenerateIn _ ->
                                context
                        )
                    )
                >> withTypeAnnotationVisitor
                    (\(Node _ typeAnnotation) context ->
                        ( []
                        , case context.kind of
                            ModuleNotToGenerateIn moduleNotToGenerateIn ->
                                { context
                                    | kind =
                                        { moduleNotToGenerateIn
                                            | generationRequested =
                                                moduleNotToGenerateIn.generationRequested
                                                    |> Dict.union
                                                        (typeds typeAnnotation
                                                            |> List.filterMap
                                                                (\{ name, range } ->
                                                                    isFromModuleToGenerateIn context.moduleNameAt
                                                                        range
                                                                        |> Maybe.map
                                                                            (\qualified ->
                                                                                ( name
                                                                                , { qualified = qualified
                                                                                  , range = range
                                                                                  }
                                                                                )
                                                                            )
                                                                )
                                                            |> Dict.fromList
                                                        )
                                        }
                                            |> ModuleNotToGenerateIn
                                }

                            ModuleToGenerateIn _ ->
                                context
                        )
                    )
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule =
                Rule.initContextCreator
                    (\metadata ->
                        moduleFromProjectContext
                            (moduleNameFromMetadata metadata)
                    )
                    |> Rule.withMetadata
                    |> Rule.withModuleNameLookupTable
            , fromModuleToProject =
                Rule.initContextCreator
                    (\metadata ->
                        moduleToProjectContext
                            (moduleNameNodeFromMetadata metadata)
                    )
                    |> Rule.withMetadata
                    |> Rule.withModuleKey
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


insertImportsFixAt : Location -> Imports -> List Fix
insertImportsFixAt insertLocation newImports =
    if newImports |> Dict.isEmpty then
        []

    else
        [ Fix.insertAt insertLocation
            ([ "\n"
             , newImports |> printImports
             ]
                |> String.concat
            )
        ]



--


type Config information requireQualified declaration
    = ReplaceStub (ReplaceStubConfig information)
    | InSameModule (InSameModuleConfig information declaration)
    | InDifferentModule (InDifferentModuleConfig requireQualified information declaration)


type alias ReplaceStubConfig information =
    { stub : String
    , generator : ExpressionGenerator information
    }


type alias InModuleConfig config requireQualified information declaration =
    { config
        | generator :
            DeclarationGenerator
                requireQualified
                information
                declaration
    }


type alias InDifferentModuleConfig requireQualified information declaration =
    InModuleConfig
        { name : ModuleName
        , insertLocation :
            DeclarationInsertLocation
                InsertLocationInDifferentModule
                information
        }
        requireQualified
        information
        declaration


type alias InSameModuleConfig information declaration =
    InModuleConfig
        { insertLocation :
            DeclarationInsertLocation
                InsertLocationInSameModule
                information
        }
        AllowsUnqualified
        information
        declaration


type DeclarationInsertLocation otherInsertLocation information
    = InsertBelowMarker String
    | InsertBelowDeclarations (information -> List String)
    | InsertBelowAllDeclarations
    | OtherInsertLocation otherInsertLocation


type InsertLocationInSameModule
    = AfterDeclarationItWasReferencedIn


type InsertLocationInDifferentModule
    = -- We could directly alias Never
      -- but adding options later would be a breaking change
      NoLocationSpecificToDifferentModule Never


inModule :
    ModuleName
    ->
        DeclarationGenerator
            requireQualified
            information
            declarationKind
    ->
        Generalizable
            (InDifferentModuleConfig
                requireQualified
                information
                declarationKind
            )
            (Config
                information
                requireQualified
                declarationKind
            )
inModule moduleToGenerateIn generator =
    { this =
        { name = moduleToGenerateIn
        , generator = generator
        , insertLocation = InsertBelowAllDeclarations
        }
    , rootWith = InDifferentModule
    }
        |> Reference.fromRecord


inSameModule :
    DeclarationGenerator
        AllowsUnqualified
        information
        declarationKind
    ->
        Generalizable
            (InSameModuleConfig information declarationKind)
            (Config information Never declarationKind)
inSameModule generator =
    { this =
        { generator = generator
        , insertLocation =
            OtherInsertLocation AfterDeclarationItWasReferencedIn
        }
    , rootWith = InSameModule
    }
        |> Reference.fromRecord


replaceStub :
    String
    -> ExpressionGenerator information
    ->
        Generalizable
            (ReplaceStubConfig information)
            (Config information Never Never)
replaceStub stub generator =
    { this =
        { generator = generator
        , stub = stub
        }
    , rootWith = ReplaceStub
    }
        |> Reference.fromRecord



--


type alias ErrorInfo =
    { message : String
    , details : List String
    }


expressionGenerationRequestedError : { description : String } -> ErrorInfo
expressionGenerationRequestedError { description } =
    { message =
        [ description |> updateFirstChar Char.toUpper
        , " can be generated"
        ]
            |> String.concat
    , details =
        [ [ "Replace it with the auto-generated ", description, " through the fix." ]
            |> String.concat
        ]
    }


missingDeclarationError :
    String
    -> { description : String }
    -> ErrorInfo
missingDeclarationError name { description } =
    { message =
        [ description |> updateFirstChar Char.toUpper
        , " `"
        , name
        , "` doesn't exist yet"
        ]
            |> String.concat
    , details =
        [ [ "Add the auto-generated ", description, " through the fix." ]
            |> String.concat
        ]
    }


missingImportFromGeneratingModule :
    String
    -> { description : String }
    -> ErrorInfo
missingImportFromGeneratingModule name { description } =
    { message =
        [ "Import the "
        , description |> updateFirstChar Char.toUpper
        , " `"
        , name
        , "`"
        ]
            |> String.concat
    , details =
        [ [ "The value "
          , name
          , " looks like a "
          , description
          , ", so I want to import it here."
          ]
            |> String.concat
        ]
    }


missingModuleError :
    ModuleName
    -> { oneDeclarationKind : String }
    -> ErrorInfo
missingModuleError moduleName { oneDeclarationKind } =
    { message =
        [ "Missing module `"
        , moduleName
        , "`"
        ]
            |> String.concat
    , details =
        [ [ "Create a new module `"
          , moduleName
          , ".elm` that will contain every generated "
          , oneDeclarationKind
          , "."
          ]
            |> String.concat
        ]
    }


missingMarkerError : String -> { for : String } -> ErrorInfo
missingMarkerError marker { for } =
    { message =
        [ "Missing marker comment `-- "
        , marker
        , "`"
        ]
            |> String.concat
    , details =
        [ [ "I need a marker comment `-- "
          , marker
          , "` where every generated "
          , for
          , " will be placed."
          ]
            |> String.concat
        ]
    }


duplicateMarkerError : String -> { for : String } -> ErrorInfo
duplicateMarkerError marker { for } =
    { message =
        [ "Duplicate marker comment `-- "
        , marker
        , "`"
        ]
            |> String.concat
    , details =
        [ [ "I can't decide below which of the marker comments `-- "
          , marker
          , "` a new generated "
          , for
          , " should be placed. Choose one and remove the others."
          ]
            |> String.concat
        ]
    }


errorInArguments :
    { description : String
    , errors : List String
    }
    -> ErrorInfo
errorInArguments { description, errors } =
    { message =
        [ "Invalid arguments given to "
        , description
        , " generator"
        ]
            |> String.concat
    , details = errors
    }


errorsInContext :
    { description : String
    , errors : List String
    }
    -> ErrorInfo
errorsInContext { description, errors } =
    { message =
        [ "Bad context for `"
        , description
        , "` generator"
        ]
            |> String.concat
    , details = errors
    }



--


withModuleCommentVisitor :
    (Node String
     -> moduleContext
     -> ( List (Rule.Error {}), moduleContext )
    )
    -> ModuleRuleSchema schemaState moduleContext
    ->
        ModuleRuleSchema
            { schemaState | hasAtLeastOneVisitor : () }
            moduleContext
withModuleCommentVisitor visitor moduleRuleSchema =
    moduleRuleSchema
        |> Rule.withCommentsVisitor
            (\comments moduleContext ->
                case
                    comments
                        |> List.find
                            (String.startsWith "{-|" << Node.value)
                of
                    Just (Node commentRange comment) ->
                        visitor
                            (Node commentRange
                                (comment |> String.dropLeft 3)
                            )
                            moduleContext

                    Nothing ->
                        ( [], moduleContext )
            )


withImportVisitor :
    ({ moduleName : ModuleName
     , alias : Maybe String
     , exposed : Maybe Code.Exposing
     , range : Range
     }
     -> moduleContext
     -> ( List (Rule.Error {}), moduleContext )
    )
    -> ModuleRuleSchema schemaState moduleContext
    -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withImportVisitor visitor moduleRuleSchema =
    moduleRuleSchema
        |> Rule.withImportVisitor
            (\(Node range { moduleName, moduleAlias, exposingList }) ->
                visitor
                    { moduleName =
                        moduleName |> Node.value |> moduleNameFromSyntax
                    , alias =
                        moduleAlias
                            |> Maybe.andThen
                                (List.head << Node.value)
                    , exposed =
                        exposingList
                            |> Maybe.map (syntaxToExposingIn << Node.value)
                    , range = range
                    }
            )


withTopLevelDeclarationListVisitor :
    (Dict String (Node Declaration.Declaration)
     -> moduleContext
     -> ( List (Rule.Error {}), moduleContext )
    )
    -> ModuleRuleSchema schemaState moduleContext
    ->
        ModuleRuleSchema
            { schemaState | hasAtLeastOneVisitor : () }
            moduleContext
withTopLevelDeclarationListVisitor visitor moduleRuleSchema =
    moduleRuleSchema
        |> Rule.withDeclarationListVisitor
            (\declarations context ->
                visitor
                    (declarations
                        |> Dict.fromListBy
                            (nameOfDeclaration << Node.value)
                    )
                    context
            )


withTypeAnnotationVisitor :
    (Node TypeAnnotation
     -> moduleContext
     -> ( List (Rule.Error {}), moduleContext )
    )
    -> ModuleRuleSchema schemaState moduleContext
    ->
        ModuleRuleSchema
            { schemaState | hasAtLeastOneVisitor : () }
            moduleContext
withTypeAnnotationVisitor visitor moduleRuleSchema =
    moduleRuleSchema
        |> Rule.withDeclarationEnterVisitor
            (\(Node _ declaration) context ->
                let
                    typeAnnotations : List (Node TypeAnnotation)
                    typeAnnotations =
                        case declaration of
                            Declaration.FunctionDeclaration { signature } ->
                                signature
                                    |> Maybe.map
                                        (\(Node _ { typeAnnotation }) ->
                                            [ typeAnnotation ]
                                        )
                                    |> Maybe.withDefault []

                            Declaration.AliasDeclaration { typeAnnotation } ->
                                [ typeAnnotation ]

                            Declaration.CustomTypeDeclaration { constructors } ->
                                constructors
                                    |> List.concatMap
                                        (\(Node _ { arguments }) ->
                                            arguments
                                        )

                            Declaration.PortDeclaration { typeAnnotation } ->
                                [ typeAnnotation ]

                            Declaration.InfixDeclaration _ ->
                                []

                            Declaration.Destructuring _ _ ->
                                []
                in
                typeAnnotations
                    |> mapAccumMultipleL visitor context
            )
        |> Rule.withExpressionEnterVisitor
            (\(Node _ expression) context ->
                case expression of
                    Expression.LetExpression { declarations } ->
                        declarations
                            |> List.filterMap
                                (\(Node _ letDeclaration) ->
                                    case letDeclaration of
                                        Expression.LetFunction { signature } ->
                                            signature
                                                |> Maybe.map
                                                    (\(Node _ { typeAnnotation }) ->
                                                        typeAnnotation
                                                    )

                                        Expression.LetDestructuring _ _ ->
                                            Nothing
                                )
                            |> mapAccumMultipleL visitor context

                    _ ->
                        ( [], context )
            )


withDependencyModulesProjectVisitor :
    (Dict
        ModuleName
        { unions : List Docs.Union
        , aliases : List Docs.Alias
        , values : List Docs.Value
        , binops : List Docs.Binop
        }
     -> projectContext
     -> ( List (Rule.Error { useErrorForModule : () }), projectContext )
    )
    -> ProjectRuleSchema schemaState projectContext moduleContext
    ->
        ProjectRuleSchema
            { schemaState | hasAtLeastOneVisitor : () }
            projectContext
            moduleContext
withDependencyModulesProjectVisitor visitor projectRuleSchema =
    projectRuleSchema
        |> Rule.withDependenciesProjectVisitor
            (\dependencies ->
                dependencies
                    |> Dict.values
                    |> List.concatMap Dependency.modules
                    |> List.map
                        (\{ name, unions, aliases, values, binops } ->
                            ( name
                            , { unions = unions
                              , aliases = aliases
                              , values = values
                              , binops = binops
                              }
                            )
                        )
                    |> Dict.fromList
                    |> visitor
            )


moduleNameFromMetadata : Rule.Metadata -> ModuleName
moduleNameFromMetadata metadata =
    Rule.moduleNameFromMetadata metadata
        |> moduleNameFromSyntax


moduleNameNodeFromMetadata : Rule.Metadata -> Node ModuleName
moduleNameNodeFromMetadata metadata =
    metadata
        |> Rule.moduleNameNodeFromMetadata
        |> Node.map moduleNameFromSyntax
