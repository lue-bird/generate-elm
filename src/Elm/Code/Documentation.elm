module Elm.Code.Documentation exposing (..)

{-|


## documentation

@docs DocumentationBlock, DocumentationComment, addMarkdown, addCustomCodeDoc

-}


documentationCommentToCodeGen :
    DocumentationComment
    -> CodeGen.Comment CodeGen.DocComment
documentationCommentToCodeGen documentationComment =
    documentationComment
        |> List.foldl
            (\block ->
                case block of
                    MarkdownDocumentationBlock md ->
                        CodeGen.markdown (md |> String.join "\n")

                    ElmCodeDocumentationBlock elm ->
                        [ case elm.moduleHeader of
                            Just header ->
                                header
                                    |> moduleHeaderToSyntax
                                    |> CodeGen.prettyModule

                            Nothing ->
                                Pretty.string ""
                        ]
                            |> Debug.todo ""
            )
            CodeGen.emptyDocComment


{-| Mark imported modules as "implicit".

They will then not be printed but the code will behave as if they were there.

-}
addImplicit :
    List Import
    -> Generalizable DocumentationCodeBlock DocumentationBlock
    -> Generalizable DocumentationCodeBlock DocumentationBlock
addImplicit additionalImplicitlyImportedModules documentationBlock =
    documentationBlock
        |> Generalizable.alter
            (\r ->
                { r
                    | implicitlyImportedModules =
                        joinAndSortImports
                            ([ r.implicitlyImportedModules |> toImportList
                             , additionalImplicitlyImportedModules
                             ]
                                |> List.concat
                            )
                }
            )


{-| Add markdown to the [`DocumentationComment`](#DocumentationComment).
-}
addMarkdown :
    List String
    -> DocumentationComment
    -> DocumentationComment
addMarkdown addedMarkdownBlock documentationComment =
    documentationComment
        ++ [ MarkdownDocumentationBlock addedMarkdownBlock ]


{-| Add a custom elm code block string to the [`DocumentationBlock`](#DocumentationBlock).
Prefer

  - [`addDeclarationDoc`](#addDeclarationDoc)
  - [`addExpressionDoc`](#addExpressionDoc)

where possible and use this for special stuff like comments.

-}
addCustomCodeDoc :
    String
    -> Generalizable DocumentationCodeBlock DocumentationBlock
    -> Generalizable DocumentationCodeBlock DocumentationBlock
addCustomCodeDoc elmCodeString documentationBlock =
    documentationBlock
        |> addPrintableDoc (CustomPrintable elmCodeString)


{-| Add an expression to a code block in a documentation comment.
-}
addExpressionDoc :
    Expression
        (SpecificExpressionNotComparable specific_ pattern_ possiblyOrNever_)
    -> Generalizable DocumentationCodeBlock DocumentationBlock
    -> Generalizable DocumentationCodeBlock DocumentationBlock
addExpressionDoc expression documentationBlock =
    documentationBlock
        |> addPrintableDoc
            (expression
                |> expressionAny
                |> ExpressionPrintable
            )


addPrintableDoc :
    DocumentationCodeSnippet
    -> Generalizable DocumentationCodeBlock DocumentationBlock
    -> Generalizable DocumentationCodeBlock DocumentationBlock
addPrintableDoc docPrintable documentationBlock =
    documentationBlock
        |> Generalizable.alter
            (\r ->
                { r
                    | codeBottomToTop =
                        r.codeBottomToTop
                            |> (::) docPrintable
                }
            )
