import ts from "typescript";
// Automatically converted by <Ctrl-C><Ctrl-V> + regexp:
//  s/export function is\([^(]*\)(node: Node): node is \([^ ]*\).*/export const as\1Impl = (node: ts.Node): ts.\2 | null => ts.is\1(node)?node:null;/
export const asNumericLiteralImpl = (node) => ts.isNumericLiteral(node) ? node : null;
export const asBigIntLiteralImpl = (node) => ts.isBigIntLiteral(node) ? node : null;
export const asStringLiteralImpl = (node) => ts.isStringLiteral(node) ? node : null;
export const asJsxTextImpl = (node) => ts.isJsxText(node) ? node : null;
export const asRegularExpressionLiteralImpl = (node) => ts.isRegularExpressionLiteral(node) ? node : null;
export const asNoSubstitutionTemplateLiteralImpl = (node) => ts.isNoSubstitutionTemplateLiteral(node) ? node : null;
// Pseudo-literals
export const asTemplateHeadImpl = (node) => ts.isTemplateHead(node) ? node : null;
export const asTemplateMiddleImpl = (node) => ts.isTemplateMiddle(node) ? node : null;
export const asTemplateTailImpl = (node) => ts.isTemplateTail(node) ? node : null;
// Punctuation
export const asDotDotDotTokenImpl = (node) => ts.isDotDotDotToken(node) ? node : null;
// /*@internal*/
// export const asCommaTokenImpl = (node: ts.Node): ts.Token<ts.SyntaxKind.CommaToken> | null => ts.isCommaToken(node)?node:null;
export const asPlusTokenImpl = (node) => ts.isPlusToken(node) ? node : null;
export const asMinusTokenImpl = (node) => ts.isMinusToken(node) ? node : null;
export const asAsteriskTokenImpl = (node) => ts.isAsteriskToken(node) ? node : null;
// /*@internal*/
// export const asExclamationTokenImpl = (node: ts.Node): ts.ExclamationToken | null => ts.isExclamationToken(node)?node:null;
// 
// /*@internal*/
// export const asQuestionTokenImpl = (node: ts.Node): ts.QuestionToken | null => ts.isQuestionToken(node)?node:null;
// 
// /*@internal*/
// export const asColonTokenImpl = (node: ts.Node): ts.ColonToken | null => ts.isColonToken(node)?node:null;
// 
// /*@internal*/
// export const asQuestionDotTokenImpl = (node: ts.Node): ts.QuestionDotToken | null => ts.isQuestionDotToken(node)?node:null;
// 
// /*@internal*/
// export const asEqualsGreaterThanTokenImpl = (node: ts.Node): ts.EqualsGreaterThanToken | null => ts.isEqualsGreaterThanToken(node)?node:null;
// Identifiers
export const asIdentifierImpl = (node) => ts.isIdentifier(node) ? node : null;
export const asPrivateIdentifierImpl = (node) => ts.isPrivateIdentifier(node) ? node : null;
// Reserved Words
// /* @internal */
// export const asExportModifierImpl = (node: ts.Node): ts.ExportKeyword | null => ts.isExportModifier(node)?node:null;
// 
// /* @internal */
// export const asAsyncModifierImpl = (node: ts.Node): ts.AsyncKeyword | null => ts.isAsyncModifier(node)?node:null;
// 
// /* @internal */
// export const asAssertsKeywordImpl = (node: ts.Node): ts.AssertsKeyword | null => ts.isAssertsKeyword(node)?node:null;
// 
// /* @internal */
// export const asAwaitKeywordImpl = (node: ts.Node): ts.AwaitKeyword | null => ts.isAwaitKeyword(node)?node:null;
// 
// /* @internal */
// export const asReadonlyKeywordImpl = (node: ts.Node): ts.ReadonlyKeyword | null => ts.isReadonlyKeyword(node)?node:null;
// 
// /* @internal */
// export const asStaticModifierImpl = (node: ts.Node): ts.StaticKeyword | null => ts.isStaticModifier(node)?node:null;
// 
// /* @internal */
// export const asAbstractModifierImpl = (node: ts.Node): ts.AbstractKeyword | null => ts.isAbstractModifier(node)?node:null;
// 
// /*@internal*/
// export const asSuperKeywordImpl = (node: ts.Node): ts.SuperExpression | null => ts.isSuperKeyword(node)?node:null;
// 
// /*@internal*/
// export const asImportKeywordImpl = (node: ts.Node): ts.ImportExpression | null => ts.isImportKeyword(node)?node:null;
// Names
export const asQualifiedNameImpl = (node) => ts.isQualifiedName(node) ? node : null;
export const asComputedPropertyNameImpl = (node) => ts.isComputedPropertyName(node) ? node : null;
// Signature elements
export const asTypeParameterDeclarationImpl = (node) => ts.isTypeParameterDeclaration(node) ? node : null;
// TODO(rbuckton): Rename to 'isParameterDeclaration'
export const asParameterImpl = (node) => ts.isParameter(node) ? node : null;
export const asDecoratorImpl = (node) => ts.isDecorator(node) ? node : null;
// TypeMember
export const asPropertySignatureImpl = (node) => ts.isPropertySignature(node) ? node : null;
export const asPropertyDeclarationImpl = (node) => ts.isPropertyDeclaration(node) ? node : null;
export const asMethodSignatureImpl = (node) => ts.isMethodSignature(node) ? node : null;
export const asMethodDeclarationImpl = (node) => ts.isMethodDeclaration(node) ? node : null;
export const asClassStaticBlockDeclarationImpl = (node) => ts.isClassStaticBlockDeclaration(node) ? node : null;
export const asConstructorDeclarationImpl = (node) => ts.isConstructorDeclaration(node) ? node : null;
export const asGetAccessorDeclarationImpl = (node) => ts.isGetAccessorDeclaration(node) ? node : null;
export const asSetAccessorDeclarationImpl = (node) => ts.isSetAccessorDeclaration(node) ? node : null;
export const asCallSignatureDeclarationImpl = (node) => ts.isCallSignatureDeclaration(node) ? node : null;
export const asConstructSignatureDeclarationImpl = (node) => ts.isConstructSignatureDeclaration(node) ? node : null;
export const asIndexSignatureDeclarationImpl = (node) => ts.isIndexSignatureDeclaration(node) ? node : null;
// Type
export const asTypePredicateNodeImpl = (node) => ts.isTypePredicateNode(node) ? node : null;
export const asTypeReferenceNodeImpl = (node) => ts.isTypeReferenceNode(node) ? node : null;
export const asFunctionTypeNodeImpl = (node) => ts.isFunctionTypeNode(node) ? node : null;
export const asConstructorTypeNodeImpl = (node) => ts.isConstructorTypeNode(node) ? node : null;
export const asTypeQueryNodeImpl = (node) => ts.isTypeQueryNode(node) ? node : null;
export const asTypeLiteralNodeImpl = (node) => ts.isTypeLiteralNode(node) ? node : null;
export const asArrayTypeNodeImpl = (node) => ts.isArrayTypeNode(node) ? node : null;
export const asTupleTypeNodeImpl = (node) => ts.isTupleTypeNode(node) ? node : null;
export const asNamedTupleMemberImpl = (node) => ts.isNamedTupleMember(node) ? node : null;
export const asOptionalTypeNodeImpl = (node) => ts.isOptionalTypeNode(node) ? node : null;
export const asRestTypeNodeImpl = (node) => ts.isRestTypeNode(node) ? node : null;
export const asUnionTypeNodeImpl = (node) => ts.isUnionTypeNode(node) ? node : null;
export const asIntersectionTypeNodeImpl = (node) => ts.isIntersectionTypeNode(node) ? node : null;
export const asConditionalTypeNodeImpl = (node) => ts.isConditionalTypeNode(node) ? node : null;
export const asInferTypeNodeImpl = (node) => ts.isInferTypeNode(node) ? node : null;
export const asParenthesizedTypeNodeImpl = (node) => ts.isParenthesizedTypeNode(node) ? node : null;
export const asThisTypeNodeImpl = (node) => ts.isThisTypeNode(node) ? node : null;
export const asTypeOperatorNodeImpl = (node) => ts.isTypeOperatorNode(node) ? node : null;
export const asIndexedAccessTypeNodeImpl = (node) => ts.isIndexedAccessTypeNode(node) ? node : null;
export const asMappedTypeNodeImpl = (node) => ts.isMappedTypeNode(node) ? node : null;
export const asLiteralTypeNodeImpl = (node) => ts.isLiteralTypeNode(node) ? node : null;
export const asImportTypeNodeImpl = (node) => ts.isImportTypeNode(node) ? node : null;
export const asTemplateLiteralTypeSpanImpl = (node) => ts.isTemplateLiteralTypeSpan(node) ? node : null;
export const asTemplateLiteralTypeNodeImpl = (node) => ts.isTemplateLiteralTypeNode(node) ? node : null;
// Binding patterns
export const asObjectBindingPatternImpl = (node) => ts.isObjectBindingPattern(node) ? node : null;
export const asArrayBindingPatternImpl = (node) => ts.isArrayBindingPattern(node) ? node : null;
export const asBindingElementImpl = (node) => ts.isBindingElement(node) ? node : null;
// Expression
export const asArrayLiteralExpressionImpl = (node) => ts.isArrayLiteralExpression(node) ? node : null;
export const asObjectLiteralExpressionImpl = (node) => ts.isObjectLiteralExpression(node) ? node : null;
export const asPropertyAccessExpressionImpl = (node) => ts.isPropertyAccessExpression(node) ? node : null;
export const asElementAccessExpressionImpl = (node) => ts.isElementAccessExpression(node) ? node : null;
export const asCallExpressionImpl = (node) => ts.isCallExpression(node) ? node : null;
export const asNewExpressionImpl = (node) => ts.isNewExpression(node) ? node : null;
export const asTaggedTemplateExpressionImpl = (node) => ts.isTaggedTemplateExpression(node) ? node : null;
export const asTypeAssertionExpressionImpl = (node) => ts.isTypeAssertionExpression(node) ? node : null;
export const asParenthesizedExpressionImpl = (node) => ts.isParenthesizedExpression(node) ? node : null;
export const asFunctionExpressionImpl = (node) => ts.isFunctionExpression(node) ? node : null;
export const asArrowFunctionImpl = (node) => ts.isArrowFunction(node) ? node : null;
export const asDeleteExpressionImpl = (node) => ts.isDeleteExpression(node) ? node : null;
export const asTypeOfExpressionImpl = (node) => ts.isTypeOfExpression(node) ? node : null;
export const asVoidExpressionImpl = (node) => ts.isVoidExpression(node) ? node : null;
export const asAwaitExpressionImpl = (node) => ts.isAwaitExpression(node) ? node : null;
export const asPrefixUnaryExpressionImpl = (node) => ts.isPrefixUnaryExpression(node) ? node : null;
export const asPostfixUnaryExpressionImpl = (node) => ts.isPostfixUnaryExpression(node) ? node : null;
export const asBinaryExpressionImpl = (node) => ts.isBinaryExpression(node) ? node : null;
export const asConditionalExpressionImpl = (node) => ts.isConditionalExpression(node) ? node : null;
export const asTemplateExpressionImpl = (node) => ts.isTemplateExpression(node) ? node : null;
export const asYieldExpressionImpl = (node) => ts.isYieldExpression(node) ? node : null;
export const asSpreadElementImpl = (node) => ts.isSpreadElement(node) ? node : null;
export const asClassExpressionImpl = (node) => ts.isClassExpression(node) ? node : null;
export const asOmittedExpressionImpl = (node) => ts.isOmittedExpression(node) ? node : null;
export const asExpressionWithTypeArgumentsImpl = (node) => ts.isExpressionWithTypeArguments(node) ? node : null;
export const asAsExpressionImpl = (node) => ts.isAsExpression(node) ? node : null;
export const asNonNullExpressionImpl = (node) => ts.isNonNullExpression(node) ? node : null;
export const asMetaPropertyImpl = (node) => ts.isMetaProperty(node) ? node : null;
export const asSyntheticExpressionImpl = (node) => ts.isSyntheticExpression(node) ? node : null;
export const asPartiallyEmittedExpressionImpl = (node) => ts.isPartiallyEmittedExpression(node) ? node : null;
export const asCommaListExpressionImpl = (node) => ts.isCommaListExpression(node) ? node : null;
// Misc
export const asTemplateSpanImpl = (node) => ts.isTemplateSpan(node) ? node : null;
export const asSemicolonClassElementImpl = (node) => ts.isSemicolonClassElement(node) ? node : null;
// Elements
export const asBlockImpl = (node) => ts.isBlock(node) ? node : null;
export const asVariableStatementImpl = (node) => ts.isVariableStatement(node) ? node : null;
export const asEmptyStatementImpl = (node) => ts.isEmptyStatement(node) ? node : null;
export const asExpressionStatementImpl = (node) => ts.isExpressionStatement(node) ? node : null;
export const asIfStatementImpl = (node) => ts.isIfStatement(node) ? node : null;
export const asDoStatementImpl = (node) => ts.isDoStatement(node) ? node : null;
export const asWhileStatementImpl = (node) => ts.isWhileStatement(node) ? node : null;
export const asForStatementImpl = (node) => ts.isForStatement(node) ? node : null;
export const asForInStatementImpl = (node) => ts.isForInStatement(node) ? node : null;
export const asForOfStatementImpl = (node) => ts.isForOfStatement(node) ? node : null;
export const asContinueStatementImpl = (node) => ts.isContinueStatement(node) ? node : null;
export const asBreakStatementImpl = (node) => ts.isBreakStatement(node) ? node : null;
export const asReturnStatementImpl = (node) => ts.isReturnStatement(node) ? node : null;
export const asWithStatementImpl = (node) => ts.isWithStatement(node) ? node : null;
export const asSwitchStatementImpl = (node) => ts.isSwitchStatement(node) ? node : null;
export const asLabeledStatementImpl = (node) => ts.isLabeledStatement(node) ? node : null;
export const asThrowStatementImpl = (node) => ts.isThrowStatement(node) ? node : null;
export const asTryStatementImpl = (node) => ts.isTryStatement(node) ? node : null;
export const asDebuggerStatementImpl = (node) => ts.isDebuggerStatement(node) ? node : null;
export const asVariableDeclarationImpl = (node) => ts.isVariableDeclaration(node) ? node : null;
export const asVariableDeclarationListImpl = (node) => ts.isVariableDeclarationList(node) ? node : null;
export const asFunctionDeclarationImpl = (node) => ts.isFunctionDeclaration(node) ? node : null;
export const asClassDeclarationImpl = (node) => ts.isClassDeclaration(node) ? node : null;
export const asInterfaceDeclarationImpl = (node) => ts.isInterfaceDeclaration(node) ? node : null;
export const asTypeAliasDeclarationImpl = (node) => ts.isTypeAliasDeclaration(node) ? node : null;
export const asEnumDeclarationImpl = (node) => ts.isEnumDeclaration(node) ? node : null;
export const asModuleDeclarationImpl = (node) => ts.isModuleDeclaration(node) ? node : null;
export const asModuleBlockImpl = (node) => ts.isModuleBlock(node) ? node : null;
export const asCaseBlockImpl = (node) => ts.isCaseBlock(node) ? node : null;
export const asNamespaceExportDeclarationImpl = (node) => ts.isNamespaceExportDeclaration(node) ? node : null;
export const asImportEqualsDeclarationImpl = (node) => ts.isImportEqualsDeclaration(node) ? node : null;
export const asImportDeclarationImpl = (node) => ts.isImportDeclaration(node) ? node : null;
export const asImportClauseImpl = (node) => ts.isImportClause(node) ? node : null;
export const asAssertClauseImpl = (node) => ts.isAssertClause(node) ? node : null;
export const asAssertEntryImpl = (node) => ts.isAssertEntry(node) ? node : null;
export const asNamespaceImportImpl = (node) => ts.isNamespaceImport(node) ? node : null;
export const asNamespaceExportImpl = (node) => ts.isNamespaceExport(node) ? node : null;
export const asNamedImportsImpl = (node) => ts.isNamedImports(node) ? node : null;
export const asImportSpecifierImpl = (node) => ts.isImportSpecifier(node) ? node : null;
export const asExportAssignmentImpl = (node) => ts.isExportAssignment(node) ? node : null;
export const asExportDeclarationImpl = (node) => ts.isExportDeclaration(node) ? node : null;
export const asNamedExportsImpl = (node) => ts.isNamedExports(node) ? node : null;
export const asExportSpecifierImpl = (node) => ts.isExportSpecifier(node) ? node : null;
export const asMissingDeclarationImpl = (node) => ts.isMissingDeclaration(node) ? node : null;
export const asNotEmittedStatementImpl = (node) => ts.isNotEmittedStatement(node) ? node : null;
// /* @internal */
// export const asSyntheticReferenceImpl = (node: ts.Node): ts.SyntheticReferenceExpression | null => ts.isSyntheticReference(node)?node:null;
// 
// /* @internal */
// export const asMergeDeclarationMarkerImpl = (node: ts.Node): ts.MergeDeclarationMarker | null => ts.isMergeDeclarationMarker(node)?node:null;
// 
// /* @internal */
// export const asEndOfDeclarationMarkerImpl = (node: ts.Node): ts.EndOfDeclarationMarker | null => ts.isEndOfDeclarationMarker(node)?node:null;
// Module References
export const asExternalModuleReferenceImpl = (node) => ts.isExternalModuleReference(node) ? node : null;
// JSX
export const asJsxElementImpl = (node) => ts.isJsxElement(node) ? node : null;
export const asJsxSelfClosingElementImpl = (node) => ts.isJsxSelfClosingElement(node) ? node : null;
export const asJsxOpeningElementImpl = (node) => ts.isJsxOpeningElement(node) ? node : null;
export const asJsxClosingElementImpl = (node) => ts.isJsxClosingElement(node) ? node : null;
export const asJsxFragmentImpl = (node) => ts.isJsxFragment(node) ? node : null;
export const asJsxOpeningFragmentImpl = (node) => ts.isJsxOpeningFragment(node) ? node : null;
export const asJsxClosingFragmentImpl = (node) => ts.isJsxClosingFragment(node) ? node : null;
export const asJsxAttributeImpl = (node) => ts.isJsxAttribute(node) ? node : null;
export const asJsxAttributesImpl = (node) => ts.isJsxAttributes(node) ? node : null;
export const asJsxSpreadAttributeImpl = (node) => ts.isJsxSpreadAttribute(node) ? node : null;
export const asJsxExpressionImpl = (node) => ts.isJsxExpression(node) ? node : null;
// Clauses
export const asCaseClauseImpl = (node) => ts.isCaseClause(node) ? node : null;
export const asDefaultClauseImpl = (node) => ts.isDefaultClause(node) ? node : null;
export const asHeritageClauseImpl = (node) => ts.isHeritageClause(node) ? node : null;
export const asCatchClauseImpl = (node) => ts.isCatchClause(node) ? node : null;
// Property assignments
export const asPropertyAssignmentImpl = (node) => ts.isPropertyAssignment(node) ? node : null;
export const asShorthandPropertyAssignmentImpl = (node) => ts.isShorthandPropertyAssignment(node) ? node : null;
export const asSpreadAssignmentImpl = (node) => ts.isSpreadAssignment(node) ? node : null;
// Enum
export const asEnumMemberImpl = (node) => ts.isEnumMember(node) ? node : null;
// Unparsed
// TODO(rbuckton): isUnparsedPrologue
export const asUnparsedPrependImpl = (node) => ts.isUnparsedPrepend(node) ? node : null;
// TODO(rbuckton): isUnparsedText
// TODO(rbuckton): isUnparsedInternalText
// TODO(rbuckton): isUnparsedSyntheticReference
// Top-level nodes
export const asSourceFileImpl = (node) => ts.isSourceFile(node) ? node : null;
export const asBundleImpl = (node) => ts.isBundle(node) ? node : null;
export const asUnparsedSourceImpl = (node) => ts.isUnparsedSource(node) ? node : null;
// TODO(rbuckton): isInputFiles
// JSDoc Elements
export const asJSDocTypeExpressionImpl = (node) => ts.isJSDocTypeExpression(node) ? node : null;
export const asJSDocNameReferenceImpl = (node) => ts.isJSDocNameReference(node) ? node : null;
export const asJSDocMemberNameImpl = (node) => ts.isJSDocMemberName(node) ? node : null;
export const asJSDocLinkImpl = (node) => ts.isJSDocLink(node) ? node : null;
export const asJSDocLinkCodeImpl = (node) => ts.isJSDocLinkCode(node) ? node : null;
export const asJSDocLinkPlainImpl = (node) => ts.isJSDocLinkPlain(node) ? node : null;
export const asJSDocAllTypeImpl = (node) => ts.isJSDocAllType(node) ? node : null;
export const asJSDocUnknownTypeImpl = (node) => ts.isJSDocUnknownType(node) ? node : null;
export const asJSDocNullableTypeImpl = (node) => ts.isJSDocNullableType(node) ? node : null;
export const asJSDocNonNullableTypeImpl = (node) => ts.isJSDocNonNullableType(node) ? node : null;
export const asJSDocOptionalTypeImpl = (node) => ts.isJSDocOptionalType(node) ? node : null;
export const asJSDocFunctionTypeImpl = (node) => ts.isJSDocFunctionType(node) ? node : null;
export const asJSDocVariadicTypeImpl = (node) => ts.isJSDocVariadicType(node) ? node : null;
export const asJSDocNamepathTypeImpl = (node) => ts.isJSDocNamepathType(node) ? node : null;
export const asJSDocImpl = (node) => ts.isJSDoc(node) ? node : null;
export const asJSDocTypeLiteralImpl = (node) => ts.isJSDocTypeLiteral(node) ? node : null;
export const asJSDocSignatureImpl = (node) => ts.isJSDocSignature(node) ? node : null;
export const asSyntaxListImpl = (node) => {
    let isSyntaxList = function (n) {
        return n.kind === ts.SyntaxKind.SyntaxList;
    };
    // `ts.isSyntaxList` is not working here correctly. Why?
    return isSyntaxList(node) ? node : null;
};
//# sourceMappingURL=NodeTests.js.map