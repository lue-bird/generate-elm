module Review.Generate.Internal exposing (Config, DeclarationInsertLocation(..), DeclarationInsertLocationInDifferentModule, DeclarationInsertLocationInSameModule(..), InDifferentModuleConfig, InSameModuleConfig, duplicateMarkerError, errorInArguments, expressionGenerationRequestedError, inModule, inSameModule, missingDeclarationError, missingImportFromGeneratingModule, missingMarkerError, missingModuleError, replaceStub, rule)

import Dict exposing (Dict)
import Dict.Extra as Dict
import Elm.CodeGen as CodeGen
import Elm.Generator as Generator exposing (AllowsUnqualified, Code, DeclarationGenerator, ExpressionGenerator, Generalizable, Imports, ModuleName, RequireQualified(..), generalizable)
import Elm.Pretty as CodeGen
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import List.Extra as List
import Parser
import Pretty exposing (pretty)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (ModuleKey, ModuleRuleSchema, Rule)
import SyntaxExtra exposing (containsRange, isValueOrCall, nameOfDeclaration, printImports, reindent, typeds)
import Typed exposing (val)
import Util exposing (firstJust, fromNonempty, mapAccumMultipleL, toNonempty, updateFirstChar)


rule :
    Config information_ codeReplacingStub_ requireQualified_ declaration_
    -> Rule
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
            ModuleName
            (GenerateReplacingStubModuleContext { key : ModuleKey })
    }


type alias GenerateReplacingStubModuleContext more =
    { more
        | beforeImports : Location
        , imports :
            Imports
        , declarations : Dict String (Node Declaration)
        , stubs :
            List
                { arguments : List Expression
                , range : Range
                }
    }


generateReplacingStubs :
    ReplaceStubConfig information_ codeReplacingStub_
    -> Rule
generateReplacingStubs { generator, stub } =
    let
        initialProjectContext : GenerateReplacingStubProjectContext
        initialProjectContext =
            { inModules = Dict.empty
            }

        initialModuleContext : GenerateReplacingStubModuleContext {}
        initialModuleContext =
            { beforeImports = { row = 2, column = 1 }
            , imports = Dict.empty
            , declarations = Dict.empty
            , stubs = []
            }

        moduleToProjectContext :
            ModuleName
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
            let
                tryGenerating :
                    { module_
                        | arguments : List Expression
                        , range : Range
                    }
                    -> GenerateReplacingStubModuleContext { key : ModuleKey }
                    -> List (Rule.Error scope_)
                tryGenerating functionOrValue { key, beforeImports, imports } =
                    case functionOrValue.arguments |> generator.checker of
                        Err errorMessage ->
                            [ Rule.errorForModule key
                                (errorInArguments
                                    { description = generator.description
                                    , errorMessage = errorMessage
                                    }
                                )
                                functionOrValue.range
                            ]

                        Ok information ->
                            let
                                generated =
                                    generate generator.elm
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
                                        (generated.code.specific.expression
                                            |> CodeGen.prettyExpression
                                            |> pretty 100
                                            |> reindent functionOrValue.range.start.column
                                        )
                                   ]
                                 , generated.newImports
                                    |> insertImportsFixAt beforeImports
                                 ]
                                    |> List.concat
                                )
                            ]
            in
            inModules
                |> Dict.values
                |> List.concatMap
                    (\module_ ->
                        module_.stubs
                            |> List.concatMap
                                (\stub_ ->
                                    tryGenerating stub_
                                        module_
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

                                    Expression.Application ((Node _ (Expression.FunctionOrValue [] name)) :: (Node _ firstArgument) :: followingArguments) ->
                                        isStubWithArgs name
                                            (firstArgument
                                                :: (followingArguments |> List.map Node.value)
                                            )

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
    ({ declarationName : String
     , imports : Imports
     }
     -> information
     -> Code code
    )
    ->
        { declarationName : String
        , imports : Imports
        }
    -> information
    ->
        { code : Code code
        , newImports : Imports
        }
generate codeGenerator { declarationName, imports } information =
    let
        defaultImports =
            codeGenerator
                { declarationName = "", imports = Dict.empty }
                information
                |> .specific
                |> .imports

        newImports =
            Dict.diff defaultImports imports
    in
    { code =
        codeGenerator
            { declarationName = declarationName
            , imports = Dict.union imports newImports
            }
            information
    , newImports = newImports
    }


type alias GenerateInSameModuleBaseContext more =
    { more
        | beforeImports : Location
        , imports :
            Imports
        , declarations : Dict String (Node Declaration)
        , markerRanges : List Range
        , generationRequested :
            Dict String { range : Range }
    }


type alias GenerateInSameModuleModuleContext =
    GenerateInSameModuleBaseContext
        { lookupModuleNameAt : Range -> Maybe (List String) }


type alias GenerateInSameModuleProjectContext =
    { inModules :
        Dict
            ModuleName
            (GenerateInSameModuleBaseContext { key : ModuleKey })
    }


generateInSameModule :
    InSameModuleConfig information_ declaration_
    -> Rule
generateInSameModule config =
    let
        { generator } =
            config

        initialProjectContext : GenerateInSameModuleProjectContext
        initialProjectContext =
            { inModules = Dict.empty
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
            , lookupModuleNameAt =
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
            }

        foldProjectContexts :
            GenerateInSameModuleProjectContext
            -> GenerateInSameModuleProjectContext
            -> GenerateInSameModuleProjectContext
        foldProjectContexts aContext bContext =
            { inModules =
                Dict.union aContext.inModules bContext.inModules
            }

        finalEvaluation : GenerateInSameModuleProjectContext -> List (Rule.Error e_)
        finalEvaluation { inModules } =
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
                        Parser.run
                            generator.checker.nameParser
                            generationRequest.declarationName
                    of
                        Ok information ->
                            generateDeclarationFix
                                module_
                                generationRequest
                                information

                        Err _ ->
                            []

                generateDeclarationFix :
                    GenerateInSameModuleBaseContext { key : ModuleKey }
                    -> { declarationName : String, range : Range }
                    -> information_
                    -> List (Rule.Error scope_)
                generateDeclarationFix module_ { declarationName, range } information =
                    let
                        generated =
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
                                         , generated.code |> Generator.print
                                         ]
                                            |> String.concat
                                        )
                                   ]
                                 , generated.newImports
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
                                    case context.lookupModuleNameAt range of
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
                                            case context.lookupModuleNameAt range of
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


type alias GenerateInDifferentModuleProjectContext =
    { modulesThatRequestGeneration :
        List
            { key : ModuleKey
            , beforeImports : Location
            , generationRequested :
                Dict
                    String
                    { range : Range, qualified : Qualified }
            }
    , moduleToGenerateIn :
        Maybe
            (DifferentModule
                (ModuleToGenerateInInformation
                    { key : ModuleKey
                    , moduleNameRange : Range
                    }
                )
            )
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
        , declarations : Dict String (Node Declaration)
    }


type alias GenerateInDifferentModuleModuleContext =
    DifferentModule
        { lookupModuleNameAt : Range -> Maybe (List String)
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
            { modulesThatRequestGeneration = []
            , moduleToGenerateIn = Nothing
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
            , lookupModuleNameAt =
                ModuleNameLookupTable.moduleNameAt
                    moduleNameLookupTable
            }

        moduleToProjectContext :
            Range
            -> ModuleKey
            -> GenerateInDifferentModuleModuleContext
            -> GenerateInDifferentModuleProjectContext
        moduleToProjectContext moduleNameRange moduleKey moduleContext =
            case moduleContext.kind of
                ModuleToGenerateIn { exposing_, markerRanges, imports } ->
                    { modulesThatRequestGeneration = []
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
                    }

                ModuleNotToGenerateIn { generationRequested } ->
                    { modulesThatRequestGeneration =
                        [ { key = moduleKey
                          , beforeImports = moduleContext.beforeImports
                          , generationRequested = generationRequested
                          }
                        ]
                    , moduleToGenerateIn = Nothing
                    }

        foldProjectContexts :
            GenerateInDifferentModuleProjectContext
            -> GenerateInDifferentModuleProjectContext
            -> GenerateInDifferentModuleProjectContext
        foldProjectContexts a b =
            { modulesThatRequestGeneration =
                [ a, b ]
                    |> List.concatMap .modulesThatRequestGeneration
            , moduleToGenerateIn =
                [ a, b ]
                    |> List.map .moduleToGenerateIn
                    |> firstJust
            }

        finalEvaluation :
            GenerateInDifferentModuleProjectContext
            -> List (Rule.Error e_)
        finalEvaluation { modulesThatRequestGeneration, moduleToGenerateIn } =
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
                        parseNameAndGenerateIfShould =
                            case
                                Parser.run
                                    generator.checker.nameParser
                                    declarationName
                            of
                                Ok information ->
                                    generateDeclarationFix
                                        existingModuleToGenerateIn
                                        { declarationName = declarationName }
                                        information

                                Err _ ->
                                    []
                    in
                    case qualified of
                        Qualified ->
                            parseNameAndGenerateIfShould

                        Unqualified ->
                            case generator.checker.requireQualified |> val of
                                AllowsUnqualified ->
                                    [ parseNameAndGenerateIfShould
                                    , [ Rule.errorForModuleWithFix
                                            moduleThatRequestsGeneration
                                            (missingImportFromGeneratingModule declarationName
                                                { description = generator.description }
                                            )
                                            range
                                            (Dict.singleton nameOfModuleToGenerateIn
                                                { alias = Nothing
                                                , exposed =
                                                    CodeGen.exposeExplicit
                                                        [ CodeGen.funExpose declarationName ]
                                                        |> Just
                                                }
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
                                    { code, newImports } =
                                        generate generator.elm
                                            { declarationName = declarationName
                                            , imports = existingModuleToGenerateIn.imports
                                            }
                                            information
                                 in
                                 [ [ Fix.insertAt insertLocation
                                        ([ "\n\n\n"
                                         , code |> Generator.print
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

                        OtherInsertLocation otherInsertLocation ->
                            never otherInsertLocation
            in
            case moduleToGenerateIn of
                Just existingModuleToGenerateIn ->
                    modulesThatRequestGeneration
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
                    [ Rule.globalError
                        (missingModuleError nameOfModuleToGenerateIn
                            { oneDeclarationKind = generator.description }
                        )
                    ]

        {- find a better name -}
        requestsGeneration :
            (Range -> Maybe (List String))
            -> Range
            -> Maybe Qualified
        requestsGeneration lookupModuleNameAt range =
            case lookupModuleNameAt range of
                Just (moduleNameHead :: moduleNameRest) ->
                    if
                        ( moduleNameHead, moduleNameRest )
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
                    (\(Node expressionRange expression) context ->
                        ( []
                        , case context.kind of
                            ModuleToGenerateIn _ ->
                                context

                            ModuleNotToGenerateIn notFieldHelpersModuleContext ->
                                let
                                    insertRequestedGeneration name nameRange =
                                        case
                                            requestsGeneration context.lookupModuleNameAt
                                                nameRange
                                        of
                                            Just qualified ->
                                                { notFieldHelpersModuleContext
                                                    | generationRequested =
                                                        notFieldHelpersModuleContext.generationRequested
                                                            |> Dict.insert name
                                                                { qualified = qualified, range = nameRange }
                                                }

                                            Nothing ->
                                                notFieldHelpersModuleContext
                                in
                                { context
                                    | kind =
                                        (case expression of
                                            Expression.FunctionOrValue _ name ->
                                                insertRequestedGeneration name expressionRange

                                            Expression.Application ((Node nameRange (Expression.FunctionOrValue _ name)) :: _ :: _) ->
                                                insertRequestedGeneration name nameRange

                                            _ ->
                                                notFieldHelpersModuleContext
                                        )
                                            |> ModuleNotToGenerateIn
                                }
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
                                                                    requestsGeneration context.lookupModuleNameAt
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
                            (Rule.moduleNameNodeFromMetadata metadata
                                |> Node.range
                            )
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


type Config information codeReplacingStub requireQualified declaration
    = ReplaceStub (ReplaceStubConfig information codeReplacingStub)
    | InSameModule (InSameModuleConfig information declaration)
    | InDifferentModule (InDifferentModuleConfig requireQualified information declaration)


type alias ReplaceStubConfig information code =
    { stub : String
    , generator :
        ExpressionGenerator
            information
            (Generator.Code { code | expression : CodeGen.Expression })
    }


type alias InModuleConfig config requireQualified information declaration =
    { config
        | generator :
            DeclarationGenerator
                requireQualified
                information
                (Generator.Declaration declaration)
    }


type alias InDifferentModuleConfig requireQualified information declaration =
    InModuleConfig
        { name : ModuleName
        , insertLocation :
            DeclarationInsertLocation
                DeclarationInsertLocationInDifferentModule
                information
        }
        requireQualified
        information
        declaration


type alias InSameModuleConfig information declaration =
    InModuleConfig
        { insertLocation :
            DeclarationInsertLocation
                DeclarationInsertLocationInSameModule
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


type DeclarationInsertLocationInSameModule
    = AfterDeclarationItWasReferencedIn


type alias DeclarationInsertLocationInDifferentModule =
    Never


inModule :
    ModuleName
    ->
        DeclarationGenerator
            requireQualified
            information
            (Generator.Declaration declaration)
    ->
        Generalizable
            (InDifferentModuleConfig
                requireQualified
                information
                declaration
            )
            (Config
                information
                codeReplacingStub_
                requireQualified
                declaration
            )
inModule moduleToGenerateIn generator =
    { name = moduleToGenerateIn
    , generator = generator
    , insertLocation = InsertBelowAllDeclarations
    }
        |> generalizable InDifferentModule


inSameModule :
    DeclarationGenerator
        AllowsUnqualified
        information
        (Generator.Declaration declaration)
    ->
        Generalizable
            (InSameModuleConfig
                information
                declaration
            )
            (Config
                information
                codeReplacingStub_
                requireQualified_
                declaration
            )
inSameModule generator =
    { generator = generator
    , insertLocation =
        OtherInsertLocation AfterDeclarationItWasReferencedIn
    }
        |> generalizable InSameModule


replaceStub :
    String
    ->
        ExpressionGenerator
            information
            (Generator.Code
                { codeReplacingStub | expression : CodeGen.Expression }
            )
    ->
        Generalizable
            (ReplaceStubConfig information codeReplacingStub)
            (Config
                information
                codeReplacingStub
                requireQualified_
                declaration_
            )
replaceStub stub generator =
    { generator = generator
    , stub = stub
    }
        |> generalizable ReplaceStub



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
    let
        moduleNameString =
            moduleName |> fromNonempty |> String.join "."
    in
    { message =
        [ "Module `"
        , moduleNameString
        , "` doesn't exist yet"
        ]
            |> String.concat
    , details =
        [ [ "Create a new module `"
          , moduleNameString
          , ".elm` that will contain every generated "
          , oneDeclarationKind
          , "."
          ]
            |> String.concat
        , "Everything in there can then be imported from every other module."
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
    , errorMessage : String
    }
    -> ErrorInfo
errorInArguments { description, errorMessage } =
    { message =
        [ "Invalid arguments given to "
        , description
        , " generator"
        ]
            |> String.concat
    , details = [ errorMessage ]
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
     , exposed : Maybe Exposing
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
                case moduleName |> Node.value |> toNonempty of
                    Just nonemptyModuleName ->
                        visitor
                            { moduleName = nonemptyModuleName
                            , alias =
                                moduleAlias
                                    |> Maybe.andThen
                                        (List.head << Node.value)
                            , exposed =
                                exposingList |> Maybe.map Node.value
                            , range = range
                            }

                    Nothing ->
                        -- never
                        \context -> ( [], context )
            )


withTopLevelDeclarationListVisitor :
    (Dict String (Node Declaration)
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
                            FunctionDeclaration { signature } ->
                                signature
                                    |> Maybe.map
                                        (\(Node _ { typeAnnotation }) ->
                                            [ typeAnnotation ]
                                        )
                                    |> Maybe.withDefault []

                            AliasDeclaration { typeAnnotation } ->
                                [ typeAnnotation ]

                            CustomTypeDeclaration { constructors } ->
                                constructors
                                    |> List.concatMap
                                        (\(Node _ { arguments }) ->
                                            arguments
                                        )

                            PortDeclaration { typeAnnotation } ->
                                [ typeAnnotation ]

                            InfixDeclaration _ ->
                                []

                            Destructuring _ _ ->
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


moduleNameFromMetadata : Rule.Metadata -> ModuleName
moduleNameFromMetadata metadata =
    Rule.moduleNameFromMetadata metadata
        |> toNonempty
        -- always succeeds
        |> Maybe.withDefault ( "", [] )
