"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.asIndexedAccessTypeNodeImpl = exports.asTypeOperatorNodeImpl = exports.asThisTypeNodeImpl = exports.asParenthesizedTypeNodeImpl = exports.asInferTypeNodeImpl = exports.asConditionalTypeNodeImpl = exports.asIntersectionTypeNodeImpl = exports.asUnionTypeNodeImpl = exports.asRestTypeNodeImpl = exports.asOptionalTypeNodeImpl = exports.asNamedTupleMemberImpl = exports.asTupleTypeNodeImpl = exports.asArrayTypeNodeImpl = exports.asTypeLiteralNodeImpl = exports.asTypeQueryNodeImpl = exports.asConstructorTypeNodeImpl = exports.asFunctionTypeNodeImpl = exports.asTypeReferenceNodeImpl = exports.asTypePredicateNodeImpl = exports.asIndexSignatureDeclarationImpl = exports.asConstructSignatureDeclarationImpl = exports.asCallSignatureDeclarationImpl = exports.asSetAccessorDeclarationImpl = exports.asGetAccessorDeclarationImpl = exports.asConstructorDeclarationImpl = exports.asClassStaticBlockDeclarationImpl = exports.asMethodDeclarationImpl = exports.asMethodSignatureImpl = exports.asPropertyDeclarationImpl = exports.asPropertySignatureImpl = exports.asDecoratorImpl = exports.asParameterImpl = exports.asTypeParameterDeclarationImpl = exports.asComputedPropertyNameImpl = exports.asQualifiedNameImpl = exports.asPrivateIdentifierImpl = exports.asIdentifierImpl = exports.asAsteriskTokenImpl = exports.asMinusTokenImpl = exports.asPlusTokenImpl = exports.asDotDotDotTokenImpl = exports.asTemplateTailImpl = exports.asTemplateMiddleImpl = exports.asTemplateHeadImpl = exports.asNoSubstitutionTemplateLiteralImpl = exports.asRegularExpressionLiteralImpl = exports.asJsxTextImpl = exports.asStringLiteralImpl = exports.asBigIntLiteralImpl = exports.asNumericLiteralImpl = void 0;
exports.asForInStatementImpl = exports.asForStatementImpl = exports.asWhileStatementImpl = exports.asDoStatementImpl = exports.asIfStatementImpl = exports.asExpressionStatementImpl = exports.asEmptyStatementImpl = exports.asVariableStatementImpl = exports.asBlockImpl = exports.asSemicolonClassElementImpl = exports.asTemplateSpanImpl = exports.asCommaListExpressionImpl = exports.asPartiallyEmittedExpressionImpl = exports.asSyntheticExpressionImpl = exports.asMetaPropertyImpl = exports.asNonNullExpressionImpl = exports.asAsExpressionImpl = exports.asExpressionWithTypeArgumentsImpl = exports.asOmittedExpressionImpl = exports.asClassExpressionImpl = exports.asSpreadElementImpl = exports.asYieldExpressionImpl = exports.asTemplateExpressionImpl = exports.asConditionalExpressionImpl = exports.asBinaryExpressionImpl = exports.asPostfixUnaryExpressionImpl = exports.asPrefixUnaryExpressionImpl = exports.asAwaitExpressionImpl = exports.asVoidExpressionImpl = exports.asTypeOfExpressionImpl = exports.asDeleteExpressionImpl = exports.asArrowFunctionImpl = exports.asFunctionExpressionImpl = exports.asParenthesizedExpressionImpl = exports.asTypeAssertionExpressionImpl = exports.asTaggedTemplateExpressionImpl = exports.asNewExpressionImpl = exports.asCallExpressionImpl = exports.asElementAccessExpressionImpl = exports.asPropertyAccessExpressionImpl = exports.asObjectLiteralExpressionImpl = exports.asArrayLiteralExpressionImpl = exports.asBindingElementImpl = exports.asArrayBindingPatternImpl = exports.asObjectBindingPatternImpl = exports.asTemplateLiteralTypeNodeImpl = exports.asTemplateLiteralTypeSpanImpl = exports.asImportTypeNodeImpl = exports.asLiteralTypeNodeImpl = exports.asMappedTypeNodeImpl = void 0;
exports.asDefaultClauseImpl = exports.asCaseClauseImpl = exports.asJsxExpressionImpl = exports.asJsxSpreadAttributeImpl = exports.asJsxAttributesImpl = exports.asJsxAttributeImpl = exports.asJsxClosingFragmentImpl = exports.asJsxOpeningFragmentImpl = exports.asJsxFragmentImpl = exports.asJsxClosingElementImpl = exports.asJsxOpeningElementImpl = exports.asJsxSelfClosingElementImpl = exports.asJsxElementImpl = exports.asExternalModuleReferenceImpl = exports.asNotEmittedStatementImpl = exports.asMissingDeclarationImpl = exports.asExportSpecifierImpl = exports.asNamedExportsImpl = exports.asExportDeclarationImpl = exports.asExportAssignmentImpl = exports.asImportSpecifierImpl = exports.asNamedImportsImpl = exports.asNamespaceExportImpl = exports.asNamespaceImportImpl = exports.asAssertEntryImpl = exports.asAssertClauseImpl = exports.asImportClauseImpl = exports.asImportDeclarationImpl = exports.asImportEqualsDeclarationImpl = exports.asNamespaceExportDeclarationImpl = exports.asCaseBlockImpl = exports.asModuleBlockImpl = exports.asModuleDeclarationImpl = exports.asEnumDeclarationImpl = exports.asTypeAliasDeclarationImpl = exports.asInterfaceDeclarationImpl = exports.asClassDeclarationImpl = exports.asFunctionDeclarationImpl = exports.asVariableDeclarationListImpl = exports.asVariableDeclarationImpl = exports.asDebuggerStatementImpl = exports.asTryStatementImpl = exports.asThrowStatementImpl = exports.asLabeledStatementImpl = exports.asSwitchStatementImpl = exports.asWithStatementImpl = exports.asReturnStatementImpl = exports.asBreakStatementImpl = exports.asContinueStatementImpl = exports.asForOfStatementImpl = void 0;
exports.asSyntaxListImpl = exports.asJSDocSignatureImpl = exports.asJSDocTypeLiteralImpl = exports.asJSDocImpl = exports.asJSDocNamepathTypeImpl = exports.asJSDocVariadicTypeImpl = exports.asJSDocFunctionTypeImpl = exports.asJSDocOptionalTypeImpl = exports.asJSDocNonNullableTypeImpl = exports.asJSDocNullableTypeImpl = exports.asJSDocUnknownTypeImpl = exports.asJSDocAllTypeImpl = exports.asJSDocLinkPlainImpl = exports.asJSDocLinkCodeImpl = exports.asJSDocLinkImpl = exports.asJSDocMemberNameImpl = exports.asJSDocNameReferenceImpl = exports.asJSDocTypeExpressionImpl = exports.asUnparsedSourceImpl = exports.asBundleImpl = exports.asSourceFileImpl = exports.asUnparsedPrependImpl = exports.asEnumMemberImpl = exports.asSpreadAssignmentImpl = exports.asShorthandPropertyAssignmentImpl = exports.asPropertyAssignmentImpl = exports.asCatchClauseImpl = exports.asHeritageClauseImpl = void 0;
var ts = __importStar(require("typescript"));
// Automatically converted by <Ctrl-C><Ctrl-V> + regexp:
//  s/export function is\([^(]*\)(node: Node): node is \([^ ]*\).*/export const as\1Impl = (node: ts.Node): ts.\2 | null => ts.is\1(node)?node:null;/
var asNumericLiteralImpl = function (node) { return ts.isNumericLiteral(node) ? node : null; };
exports.asNumericLiteralImpl = asNumericLiteralImpl;
var asBigIntLiteralImpl = function (node) { return ts.isBigIntLiteral(node) ? node : null; };
exports.asBigIntLiteralImpl = asBigIntLiteralImpl;
var asStringLiteralImpl = function (node) { return ts.isStringLiteral(node) ? node : null; };
exports.asStringLiteralImpl = asStringLiteralImpl;
var asJsxTextImpl = function (node) { return ts.isJsxText(node) ? node : null; };
exports.asJsxTextImpl = asJsxTextImpl;
var asRegularExpressionLiteralImpl = function (node) { return ts.isRegularExpressionLiteral(node) ? node : null; };
exports.asRegularExpressionLiteralImpl = asRegularExpressionLiteralImpl;
var asNoSubstitutionTemplateLiteralImpl = function (node) { return ts.isNoSubstitutionTemplateLiteral(node) ? node : null; };
exports.asNoSubstitutionTemplateLiteralImpl = asNoSubstitutionTemplateLiteralImpl;
// Pseudo-literals
var asTemplateHeadImpl = function (node) { return ts.isTemplateHead(node) ? node : null; };
exports.asTemplateHeadImpl = asTemplateHeadImpl;
var asTemplateMiddleImpl = function (node) { return ts.isTemplateMiddle(node) ? node : null; };
exports.asTemplateMiddleImpl = asTemplateMiddleImpl;
var asTemplateTailImpl = function (node) { return ts.isTemplateTail(node) ? node : null; };
exports.asTemplateTailImpl = asTemplateTailImpl;
// Punctuation
var asDotDotDotTokenImpl = function (node) { return ts.isDotDotDotToken(node) ? node : null; };
exports.asDotDotDotTokenImpl = asDotDotDotTokenImpl;
// /*@internal*/
// export const asCommaTokenImpl = (node: ts.Node): ts.Token<ts.SyntaxKind.CommaToken> | null => ts.isCommaToken(node)?node:null;
var asPlusTokenImpl = function (node) { return ts.isPlusToken(node) ? node : null; };
exports.asPlusTokenImpl = asPlusTokenImpl;
var asMinusTokenImpl = function (node) { return ts.isMinusToken(node) ? node : null; };
exports.asMinusTokenImpl = asMinusTokenImpl;
var asAsteriskTokenImpl = function (node) { return ts.isAsteriskToken(node) ? node : null; };
exports.asAsteriskTokenImpl = asAsteriskTokenImpl;
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
var asIdentifierImpl = function (node) { return ts.isIdentifier(node) ? node : null; };
exports.asIdentifierImpl = asIdentifierImpl;
var asPrivateIdentifierImpl = function (node) { return ts.isPrivateIdentifier(node) ? node : null; };
exports.asPrivateIdentifierImpl = asPrivateIdentifierImpl;
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
var asQualifiedNameImpl = function (node) { return ts.isQualifiedName(node) ? node : null; };
exports.asQualifiedNameImpl = asQualifiedNameImpl;
var asComputedPropertyNameImpl = function (node) { return ts.isComputedPropertyName(node) ? node : null; };
exports.asComputedPropertyNameImpl = asComputedPropertyNameImpl;
// Signature elements
var asTypeParameterDeclarationImpl = function (node) { return ts.isTypeParameterDeclaration(node) ? node : null; };
exports.asTypeParameterDeclarationImpl = asTypeParameterDeclarationImpl;
// TODO(rbuckton): Rename to 'isParameterDeclaration'
var asParameterImpl = function (node) { return ts.isParameter(node) ? node : null; };
exports.asParameterImpl = asParameterImpl;
var asDecoratorImpl = function (node) { return ts.isDecorator(node) ? node : null; };
exports.asDecoratorImpl = asDecoratorImpl;
// TypeMember
var asPropertySignatureImpl = function (node) { return ts.isPropertySignature(node) ? node : null; };
exports.asPropertySignatureImpl = asPropertySignatureImpl;
var asPropertyDeclarationImpl = function (node) { return ts.isPropertyDeclaration(node) ? node : null; };
exports.asPropertyDeclarationImpl = asPropertyDeclarationImpl;
var asMethodSignatureImpl = function (node) { return ts.isMethodSignature(node) ? node : null; };
exports.asMethodSignatureImpl = asMethodSignatureImpl;
var asMethodDeclarationImpl = function (node) { return ts.isMethodDeclaration(node) ? node : null; };
exports.asMethodDeclarationImpl = asMethodDeclarationImpl;
var asClassStaticBlockDeclarationImpl = function (node) { return ts.isClassStaticBlockDeclaration(node) ? node : null; };
exports.asClassStaticBlockDeclarationImpl = asClassStaticBlockDeclarationImpl;
var asConstructorDeclarationImpl = function (node) { return ts.isConstructorDeclaration(node) ? node : null; };
exports.asConstructorDeclarationImpl = asConstructorDeclarationImpl;
var asGetAccessorDeclarationImpl = function (node) { return ts.isGetAccessorDeclaration(node) ? node : null; };
exports.asGetAccessorDeclarationImpl = asGetAccessorDeclarationImpl;
var asSetAccessorDeclarationImpl = function (node) { return ts.isSetAccessorDeclaration(node) ? node : null; };
exports.asSetAccessorDeclarationImpl = asSetAccessorDeclarationImpl;
var asCallSignatureDeclarationImpl = function (node) { return ts.isCallSignatureDeclaration(node) ? node : null; };
exports.asCallSignatureDeclarationImpl = asCallSignatureDeclarationImpl;
var asConstructSignatureDeclarationImpl = function (node) { return ts.isConstructSignatureDeclaration(node) ? node : null; };
exports.asConstructSignatureDeclarationImpl = asConstructSignatureDeclarationImpl;
var asIndexSignatureDeclarationImpl = function (node) { return ts.isIndexSignatureDeclaration(node) ? node : null; };
exports.asIndexSignatureDeclarationImpl = asIndexSignatureDeclarationImpl;
// Type
var asTypePredicateNodeImpl = function (node) { return ts.isTypePredicateNode(node) ? node : null; };
exports.asTypePredicateNodeImpl = asTypePredicateNodeImpl;
var asTypeReferenceNodeImpl = function (node) { return ts.isTypeReferenceNode(node) ? node : null; };
exports.asTypeReferenceNodeImpl = asTypeReferenceNodeImpl;
var asFunctionTypeNodeImpl = function (node) { return ts.isFunctionTypeNode(node) ? node : null; };
exports.asFunctionTypeNodeImpl = asFunctionTypeNodeImpl;
var asConstructorTypeNodeImpl = function (node) { return ts.isConstructorTypeNode(node) ? node : null; };
exports.asConstructorTypeNodeImpl = asConstructorTypeNodeImpl;
var asTypeQueryNodeImpl = function (node) { return ts.isTypeQueryNode(node) ? node : null; };
exports.asTypeQueryNodeImpl = asTypeQueryNodeImpl;
var asTypeLiteralNodeImpl = function (node) { return ts.isTypeLiteralNode(node) ? node : null; };
exports.asTypeLiteralNodeImpl = asTypeLiteralNodeImpl;
var asArrayTypeNodeImpl = function (node) { return ts.isArrayTypeNode(node) ? node : null; };
exports.asArrayTypeNodeImpl = asArrayTypeNodeImpl;
var asTupleTypeNodeImpl = function (node) { return ts.isTupleTypeNode(node) ? node : null; };
exports.asTupleTypeNodeImpl = asTupleTypeNodeImpl;
var asNamedTupleMemberImpl = function (node) { return ts.isNamedTupleMember(node) ? node : null; };
exports.asNamedTupleMemberImpl = asNamedTupleMemberImpl;
var asOptionalTypeNodeImpl = function (node) { return ts.isOptionalTypeNode(node) ? node : null; };
exports.asOptionalTypeNodeImpl = asOptionalTypeNodeImpl;
var asRestTypeNodeImpl = function (node) { return ts.isRestTypeNode(node) ? node : null; };
exports.asRestTypeNodeImpl = asRestTypeNodeImpl;
var asUnionTypeNodeImpl = function (node) { return ts.isUnionTypeNode(node) ? node : null; };
exports.asUnionTypeNodeImpl = asUnionTypeNodeImpl;
var asIntersectionTypeNodeImpl = function (node) { return ts.isIntersectionTypeNode(node) ? node : null; };
exports.asIntersectionTypeNodeImpl = asIntersectionTypeNodeImpl;
var asConditionalTypeNodeImpl = function (node) { return ts.isConditionalTypeNode(node) ? node : null; };
exports.asConditionalTypeNodeImpl = asConditionalTypeNodeImpl;
var asInferTypeNodeImpl = function (node) { return ts.isInferTypeNode(node) ? node : null; };
exports.asInferTypeNodeImpl = asInferTypeNodeImpl;
var asParenthesizedTypeNodeImpl = function (node) { return ts.isParenthesizedTypeNode(node) ? node : null; };
exports.asParenthesizedTypeNodeImpl = asParenthesizedTypeNodeImpl;
var asThisTypeNodeImpl = function (node) { return ts.isThisTypeNode(node) ? node : null; };
exports.asThisTypeNodeImpl = asThisTypeNodeImpl;
var asTypeOperatorNodeImpl = function (node) { return ts.isTypeOperatorNode(node) ? node : null; };
exports.asTypeOperatorNodeImpl = asTypeOperatorNodeImpl;
var asIndexedAccessTypeNodeImpl = function (node) { return ts.isIndexedAccessTypeNode(node) ? node : null; };
exports.asIndexedAccessTypeNodeImpl = asIndexedAccessTypeNodeImpl;
var asMappedTypeNodeImpl = function (node) { return ts.isMappedTypeNode(node) ? node : null; };
exports.asMappedTypeNodeImpl = asMappedTypeNodeImpl;
var asLiteralTypeNodeImpl = function (node) { return ts.isLiteralTypeNode(node) ? node : null; };
exports.asLiteralTypeNodeImpl = asLiteralTypeNodeImpl;
var asImportTypeNodeImpl = function (node) { return ts.isImportTypeNode(node) ? node : null; };
exports.asImportTypeNodeImpl = asImportTypeNodeImpl;
var asTemplateLiteralTypeSpanImpl = function (node) { return ts.isTemplateLiteralTypeSpan(node) ? node : null; };
exports.asTemplateLiteralTypeSpanImpl = asTemplateLiteralTypeSpanImpl;
var asTemplateLiteralTypeNodeImpl = function (node) { return ts.isTemplateLiteralTypeNode(node) ? node : null; };
exports.asTemplateLiteralTypeNodeImpl = asTemplateLiteralTypeNodeImpl;
// Binding patterns
var asObjectBindingPatternImpl = function (node) { return ts.isObjectBindingPattern(node) ? node : null; };
exports.asObjectBindingPatternImpl = asObjectBindingPatternImpl;
var asArrayBindingPatternImpl = function (node) { return ts.isArrayBindingPattern(node) ? node : null; };
exports.asArrayBindingPatternImpl = asArrayBindingPatternImpl;
var asBindingElementImpl = function (node) { return ts.isBindingElement(node) ? node : null; };
exports.asBindingElementImpl = asBindingElementImpl;
// Expression
var asArrayLiteralExpressionImpl = function (node) { return ts.isArrayLiteralExpression(node) ? node : null; };
exports.asArrayLiteralExpressionImpl = asArrayLiteralExpressionImpl;
var asObjectLiteralExpressionImpl = function (node) { return ts.isObjectLiteralExpression(node) ? node : null; };
exports.asObjectLiteralExpressionImpl = asObjectLiteralExpressionImpl;
var asPropertyAccessExpressionImpl = function (node) { return ts.isPropertyAccessExpression(node) ? node : null; };
exports.asPropertyAccessExpressionImpl = asPropertyAccessExpressionImpl;
var asElementAccessExpressionImpl = function (node) { return ts.isElementAccessExpression(node) ? node : null; };
exports.asElementAccessExpressionImpl = asElementAccessExpressionImpl;
var asCallExpressionImpl = function (node) { return ts.isCallExpression(node) ? node : null; };
exports.asCallExpressionImpl = asCallExpressionImpl;
var asNewExpressionImpl = function (node) { return ts.isNewExpression(node) ? node : null; };
exports.asNewExpressionImpl = asNewExpressionImpl;
var asTaggedTemplateExpressionImpl = function (node) { return ts.isTaggedTemplateExpression(node) ? node : null; };
exports.asTaggedTemplateExpressionImpl = asTaggedTemplateExpressionImpl;
var asTypeAssertionExpressionImpl = function (node) { return ts.isTypeAssertionExpression(node) ? node : null; };
exports.asTypeAssertionExpressionImpl = asTypeAssertionExpressionImpl;
var asParenthesizedExpressionImpl = function (node) { return ts.isParenthesizedExpression(node) ? node : null; };
exports.asParenthesizedExpressionImpl = asParenthesizedExpressionImpl;
var asFunctionExpressionImpl = function (node) { return ts.isFunctionExpression(node) ? node : null; };
exports.asFunctionExpressionImpl = asFunctionExpressionImpl;
var asArrowFunctionImpl = function (node) { return ts.isArrowFunction(node) ? node : null; };
exports.asArrowFunctionImpl = asArrowFunctionImpl;
var asDeleteExpressionImpl = function (node) { return ts.isDeleteExpression(node) ? node : null; };
exports.asDeleteExpressionImpl = asDeleteExpressionImpl;
var asTypeOfExpressionImpl = function (node) { return ts.isTypeOfExpression(node) ? node : null; };
exports.asTypeOfExpressionImpl = asTypeOfExpressionImpl;
var asVoidExpressionImpl = function (node) { return ts.isVoidExpression(node) ? node : null; };
exports.asVoidExpressionImpl = asVoidExpressionImpl;
var asAwaitExpressionImpl = function (node) { return ts.isAwaitExpression(node) ? node : null; };
exports.asAwaitExpressionImpl = asAwaitExpressionImpl;
var asPrefixUnaryExpressionImpl = function (node) { return ts.isPrefixUnaryExpression(node) ? node : null; };
exports.asPrefixUnaryExpressionImpl = asPrefixUnaryExpressionImpl;
var asPostfixUnaryExpressionImpl = function (node) { return ts.isPostfixUnaryExpression(node) ? node : null; };
exports.asPostfixUnaryExpressionImpl = asPostfixUnaryExpressionImpl;
var asBinaryExpressionImpl = function (node) { return ts.isBinaryExpression(node) ? node : null; };
exports.asBinaryExpressionImpl = asBinaryExpressionImpl;
var asConditionalExpressionImpl = function (node) { return ts.isConditionalExpression(node) ? node : null; };
exports.asConditionalExpressionImpl = asConditionalExpressionImpl;
var asTemplateExpressionImpl = function (node) { return ts.isTemplateExpression(node) ? node : null; };
exports.asTemplateExpressionImpl = asTemplateExpressionImpl;
var asYieldExpressionImpl = function (node) { return ts.isYieldExpression(node) ? node : null; };
exports.asYieldExpressionImpl = asYieldExpressionImpl;
var asSpreadElementImpl = function (node) { return ts.isSpreadElement(node) ? node : null; };
exports.asSpreadElementImpl = asSpreadElementImpl;
var asClassExpressionImpl = function (node) { return ts.isClassExpression(node) ? node : null; };
exports.asClassExpressionImpl = asClassExpressionImpl;
var asOmittedExpressionImpl = function (node) { return ts.isOmittedExpression(node) ? node : null; };
exports.asOmittedExpressionImpl = asOmittedExpressionImpl;
var asExpressionWithTypeArgumentsImpl = function (node) { return ts.isExpressionWithTypeArguments(node) ? node : null; };
exports.asExpressionWithTypeArgumentsImpl = asExpressionWithTypeArgumentsImpl;
var asAsExpressionImpl = function (node) { return ts.isAsExpression(node) ? node : null; };
exports.asAsExpressionImpl = asAsExpressionImpl;
var asNonNullExpressionImpl = function (node) { return ts.isNonNullExpression(node) ? node : null; };
exports.asNonNullExpressionImpl = asNonNullExpressionImpl;
var asMetaPropertyImpl = function (node) { return ts.isMetaProperty(node) ? node : null; };
exports.asMetaPropertyImpl = asMetaPropertyImpl;
var asSyntheticExpressionImpl = function (node) { return ts.isSyntheticExpression(node) ? node : null; };
exports.asSyntheticExpressionImpl = asSyntheticExpressionImpl;
var asPartiallyEmittedExpressionImpl = function (node) { return ts.isPartiallyEmittedExpression(node) ? node : null; };
exports.asPartiallyEmittedExpressionImpl = asPartiallyEmittedExpressionImpl;
var asCommaListExpressionImpl = function (node) { return ts.isCommaListExpression(node) ? node : null; };
exports.asCommaListExpressionImpl = asCommaListExpressionImpl;
// Misc
var asTemplateSpanImpl = function (node) { return ts.isTemplateSpan(node) ? node : null; };
exports.asTemplateSpanImpl = asTemplateSpanImpl;
var asSemicolonClassElementImpl = function (node) { return ts.isSemicolonClassElement(node) ? node : null; };
exports.asSemicolonClassElementImpl = asSemicolonClassElementImpl;
// Elements
var asBlockImpl = function (node) { return ts.isBlock(node) ? node : null; };
exports.asBlockImpl = asBlockImpl;
var asVariableStatementImpl = function (node) { return ts.isVariableStatement(node) ? node : null; };
exports.asVariableStatementImpl = asVariableStatementImpl;
var asEmptyStatementImpl = function (node) { return ts.isEmptyStatement(node) ? node : null; };
exports.asEmptyStatementImpl = asEmptyStatementImpl;
var asExpressionStatementImpl = function (node) { return ts.isExpressionStatement(node) ? node : null; };
exports.asExpressionStatementImpl = asExpressionStatementImpl;
var asIfStatementImpl = function (node) { return ts.isIfStatement(node) ? node : null; };
exports.asIfStatementImpl = asIfStatementImpl;
var asDoStatementImpl = function (node) { return ts.isDoStatement(node) ? node : null; };
exports.asDoStatementImpl = asDoStatementImpl;
var asWhileStatementImpl = function (node) { return ts.isWhileStatement(node) ? node : null; };
exports.asWhileStatementImpl = asWhileStatementImpl;
var asForStatementImpl = function (node) { return ts.isForStatement(node) ? node : null; };
exports.asForStatementImpl = asForStatementImpl;
var asForInStatementImpl = function (node) { return ts.isForInStatement(node) ? node : null; };
exports.asForInStatementImpl = asForInStatementImpl;
var asForOfStatementImpl = function (node) { return ts.isForOfStatement(node) ? node : null; };
exports.asForOfStatementImpl = asForOfStatementImpl;
var asContinueStatementImpl = function (node) { return ts.isContinueStatement(node) ? node : null; };
exports.asContinueStatementImpl = asContinueStatementImpl;
var asBreakStatementImpl = function (node) { return ts.isBreakStatement(node) ? node : null; };
exports.asBreakStatementImpl = asBreakStatementImpl;
var asReturnStatementImpl = function (node) { return ts.isReturnStatement(node) ? node : null; };
exports.asReturnStatementImpl = asReturnStatementImpl;
var asWithStatementImpl = function (node) { return ts.isWithStatement(node) ? node : null; };
exports.asWithStatementImpl = asWithStatementImpl;
var asSwitchStatementImpl = function (node) { return ts.isSwitchStatement(node) ? node : null; };
exports.asSwitchStatementImpl = asSwitchStatementImpl;
var asLabeledStatementImpl = function (node) { return ts.isLabeledStatement(node) ? node : null; };
exports.asLabeledStatementImpl = asLabeledStatementImpl;
var asThrowStatementImpl = function (node) { return ts.isThrowStatement(node) ? node : null; };
exports.asThrowStatementImpl = asThrowStatementImpl;
var asTryStatementImpl = function (node) { return ts.isTryStatement(node) ? node : null; };
exports.asTryStatementImpl = asTryStatementImpl;
var asDebuggerStatementImpl = function (node) { return ts.isDebuggerStatement(node) ? node : null; };
exports.asDebuggerStatementImpl = asDebuggerStatementImpl;
var asVariableDeclarationImpl = function (node) { return ts.isVariableDeclaration(node) ? node : null; };
exports.asVariableDeclarationImpl = asVariableDeclarationImpl;
var asVariableDeclarationListImpl = function (node) { return ts.isVariableDeclarationList(node) ? node : null; };
exports.asVariableDeclarationListImpl = asVariableDeclarationListImpl;
var asFunctionDeclarationImpl = function (node) { return ts.isFunctionDeclaration(node) ? node : null; };
exports.asFunctionDeclarationImpl = asFunctionDeclarationImpl;
var asClassDeclarationImpl = function (node) { return ts.isClassDeclaration(node) ? node : null; };
exports.asClassDeclarationImpl = asClassDeclarationImpl;
var asInterfaceDeclarationImpl = function (node) { return ts.isInterfaceDeclaration(node) ? node : null; };
exports.asInterfaceDeclarationImpl = asInterfaceDeclarationImpl;
var asTypeAliasDeclarationImpl = function (node) { return ts.isTypeAliasDeclaration(node) ? node : null; };
exports.asTypeAliasDeclarationImpl = asTypeAliasDeclarationImpl;
var asEnumDeclarationImpl = function (node) { return ts.isEnumDeclaration(node) ? node : null; };
exports.asEnumDeclarationImpl = asEnumDeclarationImpl;
var asModuleDeclarationImpl = function (node) { return ts.isModuleDeclaration(node) ? node : null; };
exports.asModuleDeclarationImpl = asModuleDeclarationImpl;
var asModuleBlockImpl = function (node) { return ts.isModuleBlock(node) ? node : null; };
exports.asModuleBlockImpl = asModuleBlockImpl;
var asCaseBlockImpl = function (node) { return ts.isCaseBlock(node) ? node : null; };
exports.asCaseBlockImpl = asCaseBlockImpl;
var asNamespaceExportDeclarationImpl = function (node) { return ts.isNamespaceExportDeclaration(node) ? node : null; };
exports.asNamespaceExportDeclarationImpl = asNamespaceExportDeclarationImpl;
var asImportEqualsDeclarationImpl = function (node) { return ts.isImportEqualsDeclaration(node) ? node : null; };
exports.asImportEqualsDeclarationImpl = asImportEqualsDeclarationImpl;
var asImportDeclarationImpl = function (node) { return ts.isImportDeclaration(node) ? node : null; };
exports.asImportDeclarationImpl = asImportDeclarationImpl;
var asImportClauseImpl = function (node) { return ts.isImportClause(node) ? node : null; };
exports.asImportClauseImpl = asImportClauseImpl;
var asAssertClauseImpl = function (node) { return ts.isAssertClause(node) ? node : null; };
exports.asAssertClauseImpl = asAssertClauseImpl;
var asAssertEntryImpl = function (node) { return ts.isAssertEntry(node) ? node : null; };
exports.asAssertEntryImpl = asAssertEntryImpl;
var asNamespaceImportImpl = function (node) { return ts.isNamespaceImport(node) ? node : null; };
exports.asNamespaceImportImpl = asNamespaceImportImpl;
var asNamespaceExportImpl = function (node) { return ts.isNamespaceExport(node) ? node : null; };
exports.asNamespaceExportImpl = asNamespaceExportImpl;
var asNamedImportsImpl = function (node) { return ts.isNamedImports(node) ? node : null; };
exports.asNamedImportsImpl = asNamedImportsImpl;
var asImportSpecifierImpl = function (node) { return ts.isImportSpecifier(node) ? node : null; };
exports.asImportSpecifierImpl = asImportSpecifierImpl;
var asExportAssignmentImpl = function (node) { return ts.isExportAssignment(node) ? node : null; };
exports.asExportAssignmentImpl = asExportAssignmentImpl;
var asExportDeclarationImpl = function (node) { return ts.isExportDeclaration(node) ? node : null; };
exports.asExportDeclarationImpl = asExportDeclarationImpl;
var asNamedExportsImpl = function (node) { return ts.isNamedExports(node) ? node : null; };
exports.asNamedExportsImpl = asNamedExportsImpl;
var asExportSpecifierImpl = function (node) { return ts.isExportSpecifier(node) ? node : null; };
exports.asExportSpecifierImpl = asExportSpecifierImpl;
var asMissingDeclarationImpl = function (node) { return ts.isMissingDeclaration(node) ? node : null; };
exports.asMissingDeclarationImpl = asMissingDeclarationImpl;
var asNotEmittedStatementImpl = function (node) { return ts.isNotEmittedStatement(node) ? node : null; };
exports.asNotEmittedStatementImpl = asNotEmittedStatementImpl;
// /* @internal */
// export const asSyntheticReferenceImpl = (node: ts.Node): ts.SyntheticReferenceExpression | null => ts.isSyntheticReference(node)?node:null;
// 
// /* @internal */
// export const asMergeDeclarationMarkerImpl = (node: ts.Node): ts.MergeDeclarationMarker | null => ts.isMergeDeclarationMarker(node)?node:null;
// 
// /* @internal */
// export const asEndOfDeclarationMarkerImpl = (node: ts.Node): ts.EndOfDeclarationMarker | null => ts.isEndOfDeclarationMarker(node)?node:null;
// Module References
var asExternalModuleReferenceImpl = function (node) { return ts.isExternalModuleReference(node) ? node : null; };
exports.asExternalModuleReferenceImpl = asExternalModuleReferenceImpl;
// JSX
var asJsxElementImpl = function (node) { return ts.isJsxElement(node) ? node : null; };
exports.asJsxElementImpl = asJsxElementImpl;
var asJsxSelfClosingElementImpl = function (node) { return ts.isJsxSelfClosingElement(node) ? node : null; };
exports.asJsxSelfClosingElementImpl = asJsxSelfClosingElementImpl;
var asJsxOpeningElementImpl = function (node) { return ts.isJsxOpeningElement(node) ? node : null; };
exports.asJsxOpeningElementImpl = asJsxOpeningElementImpl;
var asJsxClosingElementImpl = function (node) { return ts.isJsxClosingElement(node) ? node : null; };
exports.asJsxClosingElementImpl = asJsxClosingElementImpl;
var asJsxFragmentImpl = function (node) { return ts.isJsxFragment(node) ? node : null; };
exports.asJsxFragmentImpl = asJsxFragmentImpl;
var asJsxOpeningFragmentImpl = function (node) { return ts.isJsxOpeningFragment(node) ? node : null; };
exports.asJsxOpeningFragmentImpl = asJsxOpeningFragmentImpl;
var asJsxClosingFragmentImpl = function (node) { return ts.isJsxClosingFragment(node) ? node : null; };
exports.asJsxClosingFragmentImpl = asJsxClosingFragmentImpl;
var asJsxAttributeImpl = function (node) { return ts.isJsxAttribute(node) ? node : null; };
exports.asJsxAttributeImpl = asJsxAttributeImpl;
var asJsxAttributesImpl = function (node) { return ts.isJsxAttributes(node) ? node : null; };
exports.asJsxAttributesImpl = asJsxAttributesImpl;
var asJsxSpreadAttributeImpl = function (node) { return ts.isJsxSpreadAttribute(node) ? node : null; };
exports.asJsxSpreadAttributeImpl = asJsxSpreadAttributeImpl;
var asJsxExpressionImpl = function (node) { return ts.isJsxExpression(node) ? node : null; };
exports.asJsxExpressionImpl = asJsxExpressionImpl;
// Clauses
var asCaseClauseImpl = function (node) { return ts.isCaseClause(node) ? node : null; };
exports.asCaseClauseImpl = asCaseClauseImpl;
var asDefaultClauseImpl = function (node) { return ts.isDefaultClause(node) ? node : null; };
exports.asDefaultClauseImpl = asDefaultClauseImpl;
var asHeritageClauseImpl = function (node) { return ts.isHeritageClause(node) ? node : null; };
exports.asHeritageClauseImpl = asHeritageClauseImpl;
var asCatchClauseImpl = function (node) { return ts.isCatchClause(node) ? node : null; };
exports.asCatchClauseImpl = asCatchClauseImpl;
// Property assignments
var asPropertyAssignmentImpl = function (node) { return ts.isPropertyAssignment(node) ? node : null; };
exports.asPropertyAssignmentImpl = asPropertyAssignmentImpl;
var asShorthandPropertyAssignmentImpl = function (node) { return ts.isShorthandPropertyAssignment(node) ? node : null; };
exports.asShorthandPropertyAssignmentImpl = asShorthandPropertyAssignmentImpl;
var asSpreadAssignmentImpl = function (node) { return ts.isSpreadAssignment(node) ? node : null; };
exports.asSpreadAssignmentImpl = asSpreadAssignmentImpl;
// Enum
var asEnumMemberImpl = function (node) { return ts.isEnumMember(node) ? node : null; };
exports.asEnumMemberImpl = asEnumMemberImpl;
// Unparsed
// TODO(rbuckton): isUnparsedPrologue
var asUnparsedPrependImpl = function (node) { return ts.isUnparsedPrepend(node) ? node : null; };
exports.asUnparsedPrependImpl = asUnparsedPrependImpl;
// TODO(rbuckton): isUnparsedText
// TODO(rbuckton): isUnparsedInternalText
// TODO(rbuckton): isUnparsedSyntheticReference
// Top-level nodes
var asSourceFileImpl = function (node) { return ts.isSourceFile(node) ? node : null; };
exports.asSourceFileImpl = asSourceFileImpl;
var asBundleImpl = function (node) { return ts.isBundle(node) ? node : null; };
exports.asBundleImpl = asBundleImpl;
var asUnparsedSourceImpl = function (node) { return ts.isUnparsedSource(node) ? node : null; };
exports.asUnparsedSourceImpl = asUnparsedSourceImpl;
// TODO(rbuckton): isInputFiles
// JSDoc Elements
var asJSDocTypeExpressionImpl = function (node) { return ts.isJSDocTypeExpression(node) ? node : null; };
exports.asJSDocTypeExpressionImpl = asJSDocTypeExpressionImpl;
var asJSDocNameReferenceImpl = function (node) { return ts.isJSDocNameReference(node) ? node : null; };
exports.asJSDocNameReferenceImpl = asJSDocNameReferenceImpl;
var asJSDocMemberNameImpl = function (node) { return ts.isJSDocMemberName(node) ? node : null; };
exports.asJSDocMemberNameImpl = asJSDocMemberNameImpl;
var asJSDocLinkImpl = function (node) { return ts.isJSDocLink(node) ? node : null; };
exports.asJSDocLinkImpl = asJSDocLinkImpl;
var asJSDocLinkCodeImpl = function (node) { return ts.isJSDocLinkCode(node) ? node : null; };
exports.asJSDocLinkCodeImpl = asJSDocLinkCodeImpl;
var asJSDocLinkPlainImpl = function (node) { return ts.isJSDocLinkPlain(node) ? node : null; };
exports.asJSDocLinkPlainImpl = asJSDocLinkPlainImpl;
var asJSDocAllTypeImpl = function (node) { return ts.isJSDocAllType(node) ? node : null; };
exports.asJSDocAllTypeImpl = asJSDocAllTypeImpl;
var asJSDocUnknownTypeImpl = function (node) { return ts.isJSDocUnknownType(node) ? node : null; };
exports.asJSDocUnknownTypeImpl = asJSDocUnknownTypeImpl;
var asJSDocNullableTypeImpl = function (node) { return ts.isJSDocNullableType(node) ? node : null; };
exports.asJSDocNullableTypeImpl = asJSDocNullableTypeImpl;
var asJSDocNonNullableTypeImpl = function (node) { return ts.isJSDocNonNullableType(node) ? node : null; };
exports.asJSDocNonNullableTypeImpl = asJSDocNonNullableTypeImpl;
var asJSDocOptionalTypeImpl = function (node) { return ts.isJSDocOptionalType(node) ? node : null; };
exports.asJSDocOptionalTypeImpl = asJSDocOptionalTypeImpl;
var asJSDocFunctionTypeImpl = function (node) { return ts.isJSDocFunctionType(node) ? node : null; };
exports.asJSDocFunctionTypeImpl = asJSDocFunctionTypeImpl;
var asJSDocVariadicTypeImpl = function (node) { return ts.isJSDocVariadicType(node) ? node : null; };
exports.asJSDocVariadicTypeImpl = asJSDocVariadicTypeImpl;
var asJSDocNamepathTypeImpl = function (node) { return ts.isJSDocNamepathType(node) ? node : null; };
exports.asJSDocNamepathTypeImpl = asJSDocNamepathTypeImpl;
var asJSDocImpl = function (node) { return ts.isJSDoc(node) ? node : null; };
exports.asJSDocImpl = asJSDocImpl;
var asJSDocTypeLiteralImpl = function (node) { return ts.isJSDocTypeLiteral(node) ? node : null; };
exports.asJSDocTypeLiteralImpl = asJSDocTypeLiteralImpl;
var asJSDocSignatureImpl = function (node) { return ts.isJSDocSignature(node) ? node : null; };
exports.asJSDocSignatureImpl = asJSDocSignatureImpl;
var asSyntaxListImpl = function (node) {
    var isSyntaxList = function (n) {
        return n.kind === ts.SyntaxKind.SyntaxList;
    };
    // `ts.isSyntaxList` is not working here correctly. Why?
    return isSyntaxList(node) ? node : null;
};
exports.asSyntaxListImpl = asSyntaxListImpl;
//# sourceMappingURL=NodeTests.js.map