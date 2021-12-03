import * as ts from "typescript";

// Automatically converted by <Ctrl-C><Ctrl-V> + regexp:
//  s/export function is\([^(]*\)(node: Node): node is \([^ ]*\).*/export const as\1Impl = (node: ts.Node): ts.\2 | null => ts.is\1(node)?node:null;/

export const asNumericLiteralImpl = (node: ts.Node): ts.NumericLiteral | null => ts.isNumericLiteral(node)?node:null;

export const asBigIntLiteralImpl = (node: ts.Node): ts.BigIntLiteral | null => ts.isBigIntLiteral(node)?node:null;

export const asStringLiteralImpl = (node: ts.Node): ts.StringLiteral | null => ts.isStringLiteral(node)?node:null;

export const asJsxTextImpl = (node: ts.Node): ts.JsxText | null => ts.isJsxText(node)?node:null;

export const asRegularExpressionLiteralImpl = (node: ts.Node): ts.RegularExpressionLiteral | null => ts.isRegularExpressionLiteral(node)?node:null;

export const asNoSubstitutionTemplateLiteralImpl = (node: ts.Node): ts.NoSubstitutionTemplateLiteral | null => ts.isNoSubstitutionTemplateLiteral(node)?node:null;

// Pseudo-literals

export const asTemplateHeadImpl = (node: ts.Node): ts.TemplateHead | null => ts.isTemplateHead(node)?node:null;

export const asTemplateMiddleImpl = (node: ts.Node): ts.TemplateMiddle | null => ts.isTemplateMiddle(node)?node:null;

export const asTemplateTailImpl = (node: ts.Node): ts.TemplateTail | null => ts.isTemplateTail(node)?node:null;

// Punctuation

export const asDotDotDotTokenImpl = (node: ts.Node): ts.DotDotDotToken | null => ts.isDotDotDotToken(node)?node:null;

// /*@internal*/
// export const asCommaTokenImpl = (node: ts.Node): ts.Token<ts.SyntaxKind.CommaToken> | null => ts.isCommaToken(node)?node:null;

export const asPlusTokenImpl = (node: ts.Node): ts.PlusToken | null => ts.isPlusToken(node)?node:null;

export const asMinusTokenImpl = (node: ts.Node): ts.MinusToken | null => ts.isMinusToken(node)?node:null;

export const asAsteriskTokenImpl = (node: ts.Node): ts.AsteriskToken | null => ts.isAsteriskToken(node)?node:null;

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

export const asIdentifierImpl = (node: ts.Node): ts.Identifier | null => ts.isIdentifier(node)?node:null;

export const asPrivateIdentifierImpl = (node: ts.Node): ts.PrivateIdentifier | null => ts.isPrivateIdentifier(node)?node:null;

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

export const asQualifiedNameImpl = (node: ts.Node): ts.QualifiedName | null => ts.isQualifiedName(node)?node:null;

export const asComputedPropertyNameImpl = (node: ts.Node): ts.ComputedPropertyName | null => ts.isComputedPropertyName(node)?node:null;

// Signature elements

export const asTypeParameterDeclarationImpl = (node: ts.Node): ts.TypeParameterDeclaration | null => ts.isTypeParameterDeclaration(node)?node:null;

// TODO(rbuckton): Rename to 'isParameterDeclaration'
export const asParameterImpl = (node: ts.Node): ts.ParameterDeclaration | null => ts.isParameter(node)?node:null;

export const asDecoratorImpl = (node: ts.Node): ts.Decorator | null => ts.isDecorator(node)?node:null;

// TypeMember

export const asPropertySignatureImpl = (node: ts.Node): ts.PropertySignature | null => ts.isPropertySignature(node)?node:null;

export const asPropertyDeclarationImpl = (node: ts.Node): ts.PropertyDeclaration | null => ts.isPropertyDeclaration(node)?node:null;

export const asMethodSignatureImpl = (node: ts.Node): ts.MethodSignature | null => ts.isMethodSignature(node)?node:null;

export const asMethodDeclarationImpl = (node: ts.Node): ts.MethodDeclaration | null => ts.isMethodDeclaration(node)?node:null;

export const asClassStaticBlockDeclarationImpl = (node: ts.Node): ts.ClassStaticBlockDeclaration | null => ts.isClassStaticBlockDeclaration(node)?node:null;

export const asConstructorDeclarationImpl = (node: ts.Node): ts.ConstructorDeclaration | null => ts.isConstructorDeclaration(node)?node:null;

export const asGetAccessorDeclarationImpl = (node: ts.Node): ts.GetAccessorDeclaration | null => ts.isGetAccessorDeclaration(node)?node:null;

export const asSetAccessorDeclarationImpl = (node: ts.Node): ts.SetAccessorDeclaration | null => ts.isSetAccessorDeclaration(node)?node:null;

export const asCallSignatureDeclarationImpl = (node: ts.Node): ts.CallSignatureDeclaration | null => ts.isCallSignatureDeclaration(node)?node:null;

export const asConstructSignatureDeclarationImpl = (node: ts.Node): ts.ConstructSignatureDeclaration | null => ts.isConstructSignatureDeclaration(node)?node:null;

export const asIndexSignatureDeclarationImpl = (node: ts.Node): ts.IndexSignatureDeclaration | null => ts.isIndexSignatureDeclaration(node)?node:null;

// Type

export const asTypePredicateNodeImpl = (node: ts.Node): ts.TypePredicateNode | null => ts.isTypePredicateNode(node)?node:null;

export const asTypeReferenceNodeImpl = (node: ts.Node): ts.TypeReferenceNode | null => ts.isTypeReferenceNode(node)?node:null;

export const asFunctionTypeNodeImpl = (node: ts.Node): ts.FunctionTypeNode | null => ts.isFunctionTypeNode(node)?node:null;

export const asConstructorTypeNodeImpl = (node: ts.Node): ts.ConstructorTypeNode | null => ts.isConstructorTypeNode(node)?node:null;

export const asTypeQueryNodeImpl = (node: ts.Node): ts.TypeQueryNode | null => ts.isTypeQueryNode(node)?node:null;

export const asTypeLiteralNodeImpl = (node: ts.Node): ts.TypeLiteralNode | null => ts.isTypeLiteralNode(node)?node:null;

export const asArrayTypeNodeImpl = (node: ts.Node): ts.ArrayTypeNode | null => ts.isArrayTypeNode(node)?node:null;

export const asTupleTypeNodeImpl = (node: ts.Node): ts.TupleTypeNode | null => ts.isTupleTypeNode(node)?node:null;

export const asNamedTupleMemberImpl = (node: ts.Node): ts.NamedTupleMember | null => ts.isNamedTupleMember(node)?node:null;

export const asOptionalTypeNodeImpl = (node: ts.Node): ts.OptionalTypeNode | null => ts.isOptionalTypeNode(node)?node:null;

export const asRestTypeNodeImpl = (node: ts.Node): ts.RestTypeNode | null => ts.isRestTypeNode(node)?node:null;

export const asUnionTypeNodeImpl = (node: ts.Node): ts.UnionTypeNode | null => ts.isUnionTypeNode(node)?node:null;

export const asIntersectionTypeNodeImpl = (node: ts.Node): ts.IntersectionTypeNode | null => ts.isIntersectionTypeNode(node)?node:null;

export const asConditionalTypeNodeImpl = (node: ts.Node): ts.ConditionalTypeNode | null => ts.isConditionalTypeNode(node)?node:null;

export const asInferTypeNodeImpl = (node: ts.Node): ts.InferTypeNode | null => ts.isInferTypeNode(node)?node:null;

export const asParenthesizedTypeNodeImpl = (node: ts.Node): ts.ParenthesizedTypeNode | null => ts.isParenthesizedTypeNode(node)?node:null;

export const asThisTypeNodeImpl = (node: ts.Node): ts.ThisTypeNode | null => ts.isThisTypeNode(node)?node:null;

export const asTypeOperatorNodeImpl = (node: ts.Node): ts.TypeOperatorNode | null => ts.isTypeOperatorNode(node)?node:null;

export const asIndexedAccessTypeNodeImpl = (node: ts.Node): ts.IndexedAccessTypeNode | null => ts.isIndexedAccessTypeNode(node)?node:null;

export const asMappedTypeNodeImpl = (node: ts.Node): ts.MappedTypeNode | null => ts.isMappedTypeNode(node)?node:null;

export const asLiteralTypeNodeImpl = (node: ts.Node): ts.LiteralTypeNode | null => ts.isLiteralTypeNode(node)?node:null;

export const asImportTypeNodeImpl = (node: ts.Node): ts.ImportTypeNode | null => ts.isImportTypeNode(node)?node:null;

export const asTemplateLiteralTypeSpanImpl = (node: ts.Node): ts.TemplateLiteralTypeSpan | null => ts.isTemplateLiteralTypeSpan(node)?node:null;

export const asTemplateLiteralTypeNodeImpl = (node: ts.Node): ts.TemplateLiteralTypeNode | null => ts.isTemplateLiteralTypeNode(node)?node:null;

// Binding patterns

export const asObjectBindingPatternImpl = (node: ts.Node): ts.ObjectBindingPattern | null => ts.isObjectBindingPattern(node)?node:null;

export const asArrayBindingPatternImpl = (node: ts.Node): ts.ArrayBindingPattern | null => ts.isArrayBindingPattern(node)?node:null;

export const asBindingElementImpl = (node: ts.Node): ts.BindingElement | null => ts.isBindingElement(node)?node:null;

// Expression

export const asArrayLiteralExpressionImpl = (node: ts.Node): ts.ArrayLiteralExpression | null => ts.isArrayLiteralExpression(node)?node:null;

export const asObjectLiteralExpressionImpl = (node: ts.Node): ts.ObjectLiteralExpression | null => ts.isObjectLiteralExpression(node)?node:null;

export const asPropertyAccessExpressionImpl = (node: ts.Node): ts.PropertyAccessExpression | null => ts.isPropertyAccessExpression(node)?node:null;

export const asElementAccessExpressionImpl = (node: ts.Node): ts.ElementAccessExpression | null => ts.isElementAccessExpression(node)?node:null;

export const asCallExpressionImpl = (node: ts.Node): ts.CallExpression | null => ts.isCallExpression(node)?node:null;

export const asNewExpressionImpl = (node: ts.Node): ts.NewExpression | null => ts.isNewExpression(node)?node:null;

export const asTaggedTemplateExpressionImpl = (node: ts.Node): ts.TaggedTemplateExpression | null => ts.isTaggedTemplateExpression(node)?node:null;

export const asTypeAssertionExpressionImpl = (node: ts.Node): ts.TypeAssertion | null => ts.isTypeAssertionExpression(node)?node:null;

export const asParenthesizedExpressionImpl = (node: ts.Node): ts.ParenthesizedExpression | null => ts.isParenthesizedExpression(node)?node:null;

export const asFunctionExpressionImpl = (node: ts.Node): ts.FunctionExpression | null => ts.isFunctionExpression(node)?node:null;

export const asArrowFunctionImpl = (node: ts.Node): ts.ArrowFunction | null => ts.isArrowFunction(node)?node:null;

export const asDeleteExpressionImpl = (node: ts.Node): ts.DeleteExpression | null => ts.isDeleteExpression(node)?node:null;

export const asTypeOfExpressionImpl = (node: ts.Node): ts.TypeOfExpression | null => ts.isTypeOfExpression(node)?node:null;

export const asVoidExpressionImpl = (node: ts.Node): ts.VoidExpression | null => ts.isVoidExpression(node)?node:null;

export const asAwaitExpressionImpl = (node: ts.Node): ts.AwaitExpression | null => ts.isAwaitExpression(node)?node:null;

export const asPrefixUnaryExpressionImpl = (node: ts.Node): ts.PrefixUnaryExpression | null => ts.isPrefixUnaryExpression(node)?node:null;

export const asPostfixUnaryExpressionImpl = (node: ts.Node): ts.PostfixUnaryExpression | null => ts.isPostfixUnaryExpression(node)?node:null;

export const asBinaryExpressionImpl = (node: ts.Node): ts.BinaryExpression | null => ts.isBinaryExpression(node)?node:null;

export const asConditionalExpressionImpl = (node: ts.Node): ts.ConditionalExpression | null => ts.isConditionalExpression(node)?node:null;

export const asTemplateExpressionImpl = (node: ts.Node): ts.TemplateExpression | null => ts.isTemplateExpression(node)?node:null;

export const asYieldExpressionImpl = (node: ts.Node): ts.YieldExpression | null => ts.isYieldExpression(node)?node:null;

export const asSpreadElementImpl = (node: ts.Node): ts.SpreadElement | null => ts.isSpreadElement(node)?node:null;

export const asClassExpressionImpl = (node: ts.Node): ts.ClassExpression | null => ts.isClassExpression(node)?node:null;

export const asOmittedExpressionImpl = (node: ts.Node): ts.OmittedExpression | null => ts.isOmittedExpression(node)?node:null;

export const asExpressionWithTypeArgumentsImpl = (node: ts.Node): ts.ExpressionWithTypeArguments | null => ts.isExpressionWithTypeArguments(node)?node:null;

export const asAsExpressionImpl = (node: ts.Node): ts.AsExpression | null => ts.isAsExpression(node)?node:null;

export const asNonNullExpressionImpl = (node: ts.Node): ts.NonNullExpression | null => ts.isNonNullExpression(node)?node:null;

export const asMetaPropertyImpl = (node: ts.Node): ts.MetaProperty | null => ts.isMetaProperty(node)?node:null;

export const asSyntheticExpressionImpl = (node: ts.Node): ts.SyntheticExpression | null => ts.isSyntheticExpression(node)?node:null;

export const asPartiallyEmittedExpressionImpl = (node: ts.Node): ts.PartiallyEmittedExpression | null => ts.isPartiallyEmittedExpression(node)?node:null;

export const asCommaListExpressionImpl = (node: ts.Node): ts.CommaListExpression | null => ts.isCommaListExpression(node)?node:null;

// Misc

export const asTemplateSpanImpl = (node: ts.Node): ts.TemplateSpan | null => ts.isTemplateSpan(node)?node:null;

export const asSemicolonClassElementImpl = (node: ts.Node): ts.SemicolonClassElement | null => ts.isSemicolonClassElement(node)?node:null;

// Elements

export const asBlockImpl = (node: ts.Node): ts.Block | null => ts.isBlock(node)?node:null;

export const asVariableStatementImpl = (node: ts.Node): ts.VariableStatement | null => ts.isVariableStatement(node)?node:null;

export const asEmptyStatementImpl = (node: ts.Node): ts.EmptyStatement | null => ts.isEmptyStatement(node)?node:null;

export const asExpressionStatementImpl = (node: ts.Node): ts.ExpressionStatement | null => ts.isExpressionStatement(node)?node:null;

export const asIfStatementImpl = (node: ts.Node): ts.IfStatement | null => ts.isIfStatement(node)?node:null;

export const asDoStatementImpl = (node: ts.Node): ts.DoStatement | null => ts.isDoStatement(node)?node:null;

export const asWhileStatementImpl = (node: ts.Node): ts.WhileStatement | null => ts.isWhileStatement(node)?node:null;

export const asForStatementImpl = (node: ts.Node): ts.ForStatement | null => ts.isForStatement(node)?node:null;

export const asForInStatementImpl = (node: ts.Node): ts.ForInStatement | null => ts.isForInStatement(node)?node:null;

export const asForOfStatementImpl = (node: ts.Node): ts.ForOfStatement | null => ts.isForOfStatement(node)?node:null;

export const asContinueStatementImpl = (node: ts.Node): ts.ContinueStatement | null => ts.isContinueStatement(node)?node:null;

export const asBreakStatementImpl = (node: ts.Node): ts.BreakStatement | null => ts.isBreakStatement(node)?node:null;

export const asReturnStatementImpl = (node: ts.Node): ts.ReturnStatement | null => ts.isReturnStatement(node)?node:null;

export const asWithStatementImpl = (node: ts.Node): ts.WithStatement | null => ts.isWithStatement(node)?node:null;

export const asSwitchStatementImpl = (node: ts.Node): ts.SwitchStatement | null => ts.isSwitchStatement(node)?node:null;

export const asLabeledStatementImpl = (node: ts.Node): ts.LabeledStatement | null => ts.isLabeledStatement(node)?node:null;

export const asThrowStatementImpl = (node: ts.Node): ts.ThrowStatement | null => ts.isThrowStatement(node)?node:null;

export const asTryStatementImpl = (node: ts.Node): ts.TryStatement | null => ts.isTryStatement(node)?node:null;

export const asDebuggerStatementImpl = (node: ts.Node): ts.DebuggerStatement | null => ts.isDebuggerStatement(node)?node:null;

export const asVariableDeclarationImpl = (node: ts.Node): ts.VariableDeclaration | null => ts.isVariableDeclaration(node)?node:null;

export const asVariableDeclarationListImpl = (node: ts.Node): ts.VariableDeclarationList | null => ts.isVariableDeclarationList(node)?node:null;

export const asFunctionDeclarationImpl = (node: ts.Node): ts.FunctionDeclaration | null => ts.isFunctionDeclaration(node)?node:null;

export const asClassDeclarationImpl = (node: ts.Node): ts.ClassDeclaration | null => ts.isClassDeclaration(node)?node:null;

export const asInterfaceDeclarationImpl = (node: ts.Node): ts.InterfaceDeclaration | null => ts.isInterfaceDeclaration(node)?node:null;

export const asTypeAliasDeclarationImpl = (node: ts.Node): ts.TypeAliasDeclaration | null => ts.isTypeAliasDeclaration(node)?node:null;

export const asEnumDeclarationImpl = (node: ts.Node): ts.EnumDeclaration | null => ts.isEnumDeclaration(node)?node:null;

export const asModuleDeclarationImpl = (node: ts.Node): ts.ModuleDeclaration | null => ts.isModuleDeclaration(node)?node:null;

export const asModuleBlockImpl = (node: ts.Node): ts.ModuleBlock | null => ts.isModuleBlock(node)?node:null;

export const asCaseBlockImpl = (node: ts.Node): ts.CaseBlock | null => ts.isCaseBlock(node)?node:null;

export const asNamespaceExportDeclarationImpl = (node: ts.Node): ts.NamespaceExportDeclaration | null => ts.isNamespaceExportDeclaration(node)?node:null;

export const asImportEqualsDeclarationImpl = (node: ts.Node): ts.ImportEqualsDeclaration | null => ts.isImportEqualsDeclaration(node)?node:null;

export const asImportDeclarationImpl = (node: ts.Node): ts.ImportDeclaration | null => ts.isImportDeclaration(node)?node:null;

export const asImportClauseImpl = (node: ts.Node): ts.ImportClause | null => ts.isImportClause(node)?node:null;

export const asAssertClauseImpl = (node: ts.Node): ts.AssertClause | null => ts.isAssertClause(node)?node:null;

export const asAssertEntryImpl = (node: ts.Node): ts.AssertEntry | null => ts.isAssertEntry(node)?node:null;

export const asNamespaceImportImpl = (node: ts.Node): ts.NamespaceImport | null => ts.isNamespaceImport(node)?node:null;

export const asNamespaceExportImpl = (node: ts.Node): ts.NamespaceExport | null => ts.isNamespaceExport(node)?node:null;

export const asNamedImportsImpl = (node: ts.Node): ts.NamedImports | null => ts.isNamedImports(node)?node:null;

export const asImportSpecifierImpl = (node: ts.Node): ts.ImportSpecifier | null => ts.isImportSpecifier(node)?node:null;

export const asExportAssignmentImpl = (node: ts.Node): ts.ExportAssignment | null => ts.isExportAssignment(node)?node:null;

export const asExportDeclarationImpl = (node: ts.Node): ts.ExportDeclaration | null => ts.isExportDeclaration(node)?node:null;

export const asNamedExportsImpl = (node: ts.Node): ts.NamedExports | null => ts.isNamedExports(node)?node:null;

export const asExportSpecifierImpl = (node: ts.Node): ts.ExportSpecifier | null => ts.isExportSpecifier(node)?node:null;

export const asMissingDeclarationImpl = (node: ts.Node): ts.MissingDeclaration | null => ts.isMissingDeclaration(node)?node:null;

export const asNotEmittedStatementImpl = (node: ts.Node): ts.NotEmittedStatement | null => ts.isNotEmittedStatement(node)?node:null;

// /* @internal */
// export const asSyntheticReferenceImpl = (node: ts.Node): ts.SyntheticReferenceExpression | null => ts.isSyntheticReference(node)?node:null;
// 
// /* @internal */
// export const asMergeDeclarationMarkerImpl = (node: ts.Node): ts.MergeDeclarationMarker | null => ts.isMergeDeclarationMarker(node)?node:null;
// 
// /* @internal */
// export const asEndOfDeclarationMarkerImpl = (node: ts.Node): ts.EndOfDeclarationMarker | null => ts.isEndOfDeclarationMarker(node)?node:null;

// Module References

export const asExternalModuleReferenceImpl = (node: ts.Node): ts.ExternalModuleReference | null => ts.isExternalModuleReference(node)?node:null;

// JSX

export const asJsxElementImpl = (node: ts.Node): ts.JsxElement | null => ts.isJsxElement(node)?node:null;

export const asJsxSelfClosingElementImpl = (node: ts.Node): ts.JsxSelfClosingElement | null => ts.isJsxSelfClosingElement(node)?node:null;

export const asJsxOpeningElementImpl = (node: ts.Node): ts.JsxOpeningElement | null => ts.isJsxOpeningElement(node)?node:null;

export const asJsxClosingElementImpl = (node: ts.Node): ts.JsxClosingElement | null => ts.isJsxClosingElement(node)?node:null;

export const asJsxFragmentImpl = (node: ts.Node): ts.JsxFragment | null => ts.isJsxFragment(node)?node:null;

export const asJsxOpeningFragmentImpl = (node: ts.Node): ts.JsxOpeningFragment | null => ts.isJsxOpeningFragment(node)?node:null;

export const asJsxClosingFragmentImpl = (node: ts.Node): ts.JsxClosingFragment | null => ts.isJsxClosingFragment(node)?node:null;

export const asJsxAttributeImpl = (node: ts.Node): ts.JsxAttribute | null => ts.isJsxAttribute(node)?node:null;

export const asJsxAttributesImpl = (node: ts.Node): ts.JsxAttributes | null => ts.isJsxAttributes(node)?node:null;

export const asJsxSpreadAttributeImpl = (node: ts.Node): ts.JsxSpreadAttribute | null => ts.isJsxSpreadAttribute(node)?node:null;

export const asJsxExpressionImpl = (node: ts.Node): ts.JsxExpression | null => ts.isJsxExpression(node)?node:null;

// Clauses

export const asCaseClauseImpl = (node: ts.Node): ts.CaseClause | null => ts.isCaseClause(node)?node:null;

export const asDefaultClauseImpl = (node: ts.Node): ts.DefaultClause | null => ts.isDefaultClause(node)?node:null;

export const asHeritageClauseImpl = (node: ts.Node): ts.HeritageClause | null => ts.isHeritageClause(node)?node:null;

export const asCatchClauseImpl = (node: ts.Node): ts.CatchClause | null => ts.isCatchClause(node)?node:null;

// Property assignments

export const asPropertyAssignmentImpl = (node: ts.Node): ts.PropertyAssignment | null => ts.isPropertyAssignment(node)?node:null;

export const asShorthandPropertyAssignmentImpl = (node: ts.Node): ts.ShorthandPropertyAssignment | null => ts.isShorthandPropertyAssignment(node)?node:null;

export const asSpreadAssignmentImpl = (node: ts.Node): ts.SpreadAssignment | null => ts.isSpreadAssignment(node)?node:null;

// Enum

export const asEnumMemberImpl = (node: ts.Node): ts.EnumMember | null => ts.isEnumMember(node)?node:null;

// Unparsed

// TODO(rbuckton): isUnparsedPrologue

export const asUnparsedPrependImpl = (node: ts.Node): ts.UnparsedPrepend | null => ts.isUnparsedPrepend(node)?node:null;

// TODO(rbuckton): isUnparsedText
// TODO(rbuckton): isUnparsedInternalText
// TODO(rbuckton): isUnparsedSyntheticReference

// Top-level nodes
export const asSourceFileImpl = (node: ts.Node): ts.SourceFile | null => ts.isSourceFile(node)?node:null;

export const asBundleImpl = (node: ts.Node): ts.Bundle | null => ts.isBundle(node)?node:null;

export const asUnparsedSourceImpl = (node: ts.Node): ts.UnparsedSource | null => ts.isUnparsedSource(node)?node:null;

// TODO(rbuckton): isInputFiles

// JSDoc Elements

export const asJSDocTypeExpressionImpl = (node: ts.Node): ts.JSDocTypeExpression | null => ts.isJSDocTypeExpression(node)?node:null;

export const asJSDocNameReferenceImpl = (node: ts.Node): ts.JSDocNameReference | null => ts.isJSDocNameReference(node)?node:null;

export const asJSDocMemberNameImpl = (node: ts.Node): ts.JSDocMemberName | null => ts.isJSDocMemberName(node)?node:null;

export const asJSDocLinkImpl = (node: ts.Node): ts.JSDocLink | null => ts.isJSDocLink(node)?node:null;

export const asJSDocLinkCodeImpl = (node: ts.Node): ts.JSDocLinkCode | null => ts.isJSDocLinkCode(node)?node:null;

export const asJSDocLinkPlainImpl = (node: ts.Node): ts.JSDocLinkPlain | null => ts.isJSDocLinkPlain(node)?node:null;

export const asJSDocAllTypeImpl = (node: ts.Node): ts.JSDocAllType | null => ts.isJSDocAllType(node)?node:null;

export const asJSDocUnknownTypeImpl = (node: ts.Node): ts.JSDocUnknownType | null => ts.isJSDocUnknownType(node)?node:null;

export const asJSDocNullableTypeImpl = (node: ts.Node): ts.JSDocNullableType | null => ts.isJSDocNullableType(node)?node:null;

export const asJSDocNonNullableTypeImpl = (node: ts.Node): ts.JSDocNonNullableType | null => ts.isJSDocNonNullableType(node)?node:null;

export const asJSDocOptionalTypeImpl = (node: ts.Node): ts.JSDocOptionalType | null => ts.isJSDocOptionalType(node)?node:null;

export const asJSDocFunctionTypeImpl = (node: ts.Node): ts.JSDocFunctionType | null => ts.isJSDocFunctionType(node)?node:null;

export const asJSDocVariadicTypeImpl = (node: ts.Node): ts.JSDocVariadicType | null => ts.isJSDocVariadicType(node)?node:null;

export const asJSDocNamepathTypeImpl = (node: ts.Node): ts.JSDocNamepathType | null => ts.isJSDocNamepathType(node)?node:null;

export const asJSDocImpl = (node: ts.Node): ts.JSDoc | null => ts.isJSDoc(node)?node:null;

export const asJSDocTypeLiteralImpl = (node: ts.Node): ts.JSDocTypeLiteral | null => ts.isJSDocTypeLiteral(node)?node:null;

export const asJSDocSignatureImpl = (node: ts.Node): ts.JSDocSignature | null => ts.isJSDocSignature(node)?node:null;

export const asSyntaxListImpl = (node: ts.Node) : ts.SyntaxList | null => {
  let isSyntaxList = function(n: ts.Node): n is ts.SyntaxList {
    return n.kind === ts.SyntaxKind.SyntaxList;
  }
  // `ts.isSyntaxList` is not working here correctly. Why?
  return isSyntaxList(node)?node:null;
}



