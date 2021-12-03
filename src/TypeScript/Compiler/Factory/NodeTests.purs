module TypeScript.Compiler.Factory.NodeTests where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import TypeScript.Compiler.Types (Node)
import TypeScript.Compiler.Types.Nodes as Nodes

-- Automatically converted by <Ctrl-C><Ctrl-V> of the NodeTests.ts content and "nearly" this regexp:
-- s/export const \([^ ]*\)Impl.*): ts.\([^ ]*\).*/\1 = toMaybe <<< runFn1 \1Impl\r\rforeign import \1Impl :: forall r. Fn1 (Node r) (Nullable Nodes.\2/)
--
-- Order of definitions in this file follows the original typescript module.
-- All extra node testing funcitons are added at the end of the file.

asNumericLiteral :: forall r. Node r -> Maybe Nodes.NumericLiteral
asNumericLiteral = toMaybe <<< runFn1 asNumericLiteralImpl

foreign import asNumericLiteralImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NumericLiteral)

asBigIntLiteral :: forall r. Node r -> Maybe Nodes.BigIntLiteral
asBigIntLiteral = toMaybe <<< runFn1 asBigIntLiteralImpl

foreign import asBigIntLiteralImpl :: forall r. Fn1 (Node r) (Nullable Nodes.BigIntLiteral)

asStringLiteral :: forall r. Node r -> Maybe Nodes.StringLiteral
asStringLiteral = toMaybe <<< runFn1 asStringLiteralImpl

foreign import asStringLiteralImpl :: forall r. Fn1 (Node r) (Nullable Nodes.StringLiteral)

asJsxText :: forall r. Node r -> Maybe Nodes.JsxText
asJsxText = toMaybe <<< runFn1 asJsxTextImpl

foreign import asJsxTextImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxText)

asRegularExpressionLiteral :: forall r. Node r -> Maybe Nodes.RegularExpressionLiteral
asRegularExpressionLiteral = toMaybe <<< runFn1 asRegularExpressionLiteralImpl

foreign import asRegularExpressionLiteralImpl :: forall r. Fn1 (Node r) (Nullable Nodes.RegularExpressionLiteral)

asNoSubstitutionTemplateLiteral :: forall r. Node r -> Maybe Nodes.NoSubstitutionTemplateLiteral
asNoSubstitutionTemplateLiteral = toMaybe <<< runFn1 asNoSubstitutionTemplateLiteralImpl

foreign import asNoSubstitutionTemplateLiteralImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NoSubstitutionTemplateLiteral)

-- Pseudo-literals

asTemplateHead :: forall r. Node r -> Maybe Nodes.TemplateHead
asTemplateHead = toMaybe <<< runFn1 asTemplateHeadImpl

foreign import asTemplateHeadImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TemplateHead)

asTemplateMiddle :: forall r. Node r -> Maybe Nodes.TemplateMiddle
asTemplateMiddle = toMaybe <<< runFn1 asTemplateMiddleImpl

foreign import asTemplateMiddleImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TemplateMiddle)

asTemplateTail :: forall r. Node r -> Maybe Nodes.TemplateTail
asTemplateTail = toMaybe <<< runFn1 asTemplateTailImpl

foreign import asTemplateTailImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TemplateTail)

-- Punctuation

asDotDotDotToken :: forall r. Node r -> Maybe Nodes.DotDotDotToken
asDotDotDotToken = toMaybe <<< runFn1 asDotDotDotTokenImpl

foreign import asDotDotDotTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.DotDotDotToken)

-- /*@internal*/
-- asCommaToken = toMaybe <<< runFn1 asCommaTokenImpl
-- foreign import asCommaTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.Token<ts.SyntaxKind.CommaToken>)

asPlusToken :: forall r. Node r -> Maybe Nodes.PlusToken
asPlusToken = toMaybe <<< runFn1 asPlusTokenImpl

foreign import asPlusTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PlusToken)

asMinusToken :: forall r. Node r -> Maybe Nodes.MinusToken
asMinusToken = toMaybe <<< runFn1 asMinusTokenImpl

foreign import asMinusTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.MinusToken)

asAsteriskToken :: forall r. Node r -> Maybe Nodes.AsteriskToken
asAsteriskToken = toMaybe <<< runFn1 asAsteriskTokenImpl

foreign import asAsteriskTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AsteriskToken)

-- /*@internal*/
-- asExclamationToken = toMaybe <<< runFn1 asExclamationTokenImpl
-- foreign import asExclamationTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ExclamationToken)
-- 
-- /*@internal*/
-- asQuestionToken = toMaybe <<< runFn1 asQuestionTokenImpl
-- foreign import asQuestionTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.QuestionToken)
-- 
-- /*@internal*/
-- asColonToken = toMaybe <<< runFn1 asColonTokenImpl
-- foreign import asColonTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ColonToken)
-- 
-- /*@internal*/
-- asQuestionDotToken = toMaybe <<< runFn1 asQuestionDotTokenImpl
-- foreign import asQuestionDotTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.QuestionDotToken)
-- 
-- /*@internal*/
-- asEqualsGreaterThanToken = toMaybe <<< runFn1 asEqualsGreaterThanTokenImpl
-- foreign import asEqualsGreaterThanTokenImpl :: forall r. Fn1 (Node r) (Nullable Nodes.EqualsGreaterThanToken)

-- Identifiers

asIdentifier :: forall r. Node r -> Maybe Nodes.Identifier
asIdentifier = toMaybe <<< runFn1 asIdentifierImpl

foreign import asIdentifierImpl :: forall r. Fn1 (Node r) (Nullable Nodes.Identifier)

asPrivateIdentifier :: forall r. Node r -> Maybe Nodes.PrivateIdentifier
asPrivateIdentifier = toMaybe <<< runFn1 asPrivateIdentifierImpl

foreign import asPrivateIdentifierImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PrivateIdentifier)

-- Reserved Words

-- /* @internal */
-- asExportModifier = toMaybe <<< runFn1 asExportModifierImpl
-- foreign import asExportModifierImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ExportKeyword)
-- 
-- /* @internal */
-- asAsyncModifier = toMaybe <<< runFn1 asAsyncModifierImpl
-- foreign import asAsyncModifierImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AsyncKeyword)
-- 
-- /* @internal */
-- asAssertsKeyword = toMaybe <<< runFn1 asAssertsKeywordImpl
-- foreign import asAssertsKeywordImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AssertsKeyword)
-- 
-- /* @internal */
-- asAwaitKeyword = toMaybe <<< runFn1 asAwaitKeywordImpl
-- foreign import asAwaitKeywordImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AwaitKeyword)
-- 
-- /* @internal */
-- asReadonlyKeyword = toMaybe <<< runFn1 asReadonlyKeywordImpl
-- foreign import asReadonlyKeywordImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ReadonlyKeyword)
-- 
-- /* @internal */
-- asStaticModifier = toMaybe <<< runFn1 asStaticModifierImpl
-- foreign import asStaticModifierImpl :: forall r. Fn1 (Node r) (Nullable Nodes.StaticKeyword)
-- 
-- /* @internal */
-- asAbstractModifier = toMaybe <<< runFn1 asAbstractModifierImpl
-- foreign import asAbstractModifierImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AbstractKeyword)
-- 
-- /*@internal*/
-- asSuperKeyword = toMaybe <<< runFn1 asSuperKeywordImpl
-- 
-- foreign import asSuperKeywordImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SuperExpression)
-- 
-- /*@internal*/
-- asImportKeyword = toMaybe <<< runFn1 asImportKeywordImpl
-- 
-- foreign import asImportKeywordImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ImportExpression)

-- Names

asQualifiedName :: forall r. Node r -> Maybe Nodes.QualifiedName
asQualifiedName = toMaybe <<< runFn1 asQualifiedNameImpl

foreign import asQualifiedNameImpl :: forall r. Fn1 (Node r) (Nullable Nodes.QualifiedName)

asComputedPropertyName :: forall r. Node r -> Maybe Nodes.ComputedPropertyName
asComputedPropertyName = toMaybe <<< runFn1 asComputedPropertyNameImpl

foreign import asComputedPropertyNameImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ComputedPropertyName)

-- Signature elements

asTypeParameterDeclaration :: forall r. Node r -> Maybe Nodes.TypeParameterDeclaration
asTypeParameterDeclaration = toMaybe <<< runFn1 asTypeParameterDeclarationImpl

foreign import asTypeParameterDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypeParameterDeclaration)

asParameterDeclaration :: forall r. Node r -> Maybe Nodes.ParameterDeclaration
asParameterDeclaration = toMaybe <<< runFn1 asParameterImpl

foreign import asParameterImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ParameterDeclaration)

asDecorator :: forall r. Node r -> Maybe Nodes.Decorator
asDecorator = toMaybe <<< runFn1 asDecoratorImpl

foreign import asDecoratorImpl :: forall r. Fn1 (Node r) (Nullable Nodes.Decorator)

-- TypeMember

asPropertySignature :: forall r. Node r -> Maybe Nodes.PropertySignature
asPropertySignature = toMaybe <<< runFn1 asPropertySignatureImpl

foreign import asPropertySignatureImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PropertySignature)

asPropertyDeclaration :: forall r. Node r -> Maybe Nodes.PropertyDeclaration
asPropertyDeclaration = toMaybe <<< runFn1 asPropertyDeclarationImpl

foreign import asPropertyDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PropertyDeclaration)

asMethodSignature :: forall r. Node r -> Maybe Nodes.MethodSignature
asMethodSignature = toMaybe <<< runFn1 asMethodSignatureImpl

foreign import asMethodSignatureImpl :: forall r. Fn1 (Node r) (Nullable Nodes.MethodSignature)

asMethodDeclaration :: forall r. Node r -> Maybe Nodes.MethodDeclaration
asMethodDeclaration = toMaybe <<< runFn1 asMethodDeclarationImpl

foreign import asMethodDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.MethodDeclaration)

asClassStaticBlockDeclaration :: forall r. Node r -> Maybe Nodes.ClassStaticBlockDeclaration
asClassStaticBlockDeclaration = toMaybe <<< runFn1 asClassStaticBlockDeclarationImpl

foreign import asClassStaticBlockDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ClassStaticBlockDeclaration)

asConstructorDeclaration :: forall r. Node r -> Maybe Nodes.ConstructorDeclaration
asConstructorDeclaration = toMaybe <<< runFn1 asConstructorDeclarationImpl

foreign import asConstructorDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ConstructorDeclaration)

asGetAccessorDeclaration :: forall r. Node r -> Maybe Nodes.GetAccessorDeclaration
asGetAccessorDeclaration = toMaybe <<< runFn1 asGetAccessorDeclarationImpl

foreign import asGetAccessorDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.GetAccessorDeclaration)

asSetAccessorDeclaration :: forall r. Node r -> Maybe Nodes.SetAccessorDeclaration
asSetAccessorDeclaration = toMaybe <<< runFn1 asSetAccessorDeclarationImpl

foreign import asSetAccessorDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SetAccessorDeclaration)

asCallSignatureDeclaration :: forall r. Node r -> Maybe Nodes.CallSignatureDeclaration
asCallSignatureDeclaration = toMaybe <<< runFn1 asCallSignatureDeclarationImpl

foreign import asCallSignatureDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.CallSignatureDeclaration)

asConstructSignatureDeclaration :: forall r. Node r -> Maybe Nodes.ConstructSignatureDeclaration
asConstructSignatureDeclaration = toMaybe <<< runFn1 asConstructSignatureDeclarationImpl

foreign import asConstructSignatureDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ConstructSignatureDeclaration)

asIndexSignatureDeclaration :: forall r. Node r -> Maybe Nodes.IndexSignatureDeclaration
asIndexSignatureDeclaration = toMaybe <<< runFn1 asIndexSignatureDeclarationImpl

foreign import asIndexSignatureDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.IndexSignatureDeclaration)

-- Type

asTypePredicateNode :: forall r. Node r -> Maybe Nodes.TypePredicateNode
asTypePredicateNode = toMaybe <<< runFn1 asTypePredicateNodeImpl

foreign import asTypePredicateNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypePredicateNode)

asTypeReferenceNode :: forall r. Node r -> Maybe Nodes.TypeReferenceNode
asTypeReferenceNode = toMaybe <<< runFn1 asTypeReferenceNodeImpl

foreign import asTypeReferenceNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypeReferenceNode)

asFunctionTypeNode :: forall r. Node r -> Maybe Nodes.FunctionTypeNode
asFunctionTypeNode = toMaybe <<< runFn1 asFunctionTypeNodeImpl

foreign import asFunctionTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.FunctionTypeNode)

asConstructorTypeNode :: forall r. Node r -> Maybe Nodes.ConstructorTypeNode
asConstructorTypeNode = toMaybe <<< runFn1 asConstructorTypeNodeImpl

foreign import asConstructorTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ConstructorTypeNode)

asTypeQueryNode :: forall r. Node r -> Maybe Nodes.TypeQueryNode
asTypeQueryNode = toMaybe <<< runFn1 asTypeQueryNodeImpl

foreign import asTypeQueryNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypeQueryNode)

asTypeLiteralNode :: forall r. Node r -> Maybe Nodes.TypeLiteralNode
asTypeLiteralNode = toMaybe <<< runFn1 asTypeLiteralNodeImpl

foreign import asTypeLiteralNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypeLiteralNode)

asArrayTypeNode :: forall r. Node r -> Maybe Nodes.ArrayTypeNode
asArrayTypeNode = toMaybe <<< runFn1 asArrayTypeNodeImpl

foreign import asArrayTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ArrayTypeNode)

asTupleTypeNode :: forall r. Node r -> Maybe Nodes.TupleTypeNode
asTupleTypeNode = toMaybe <<< runFn1 asTupleTypeNodeImpl

foreign import asTupleTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TupleTypeNode)

asNamedTupleMember :: forall r. Node r -> Maybe Nodes.NamedTupleMember
asNamedTupleMember = toMaybe <<< runFn1 asNamedTupleMemberImpl

foreign import asNamedTupleMemberImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NamedTupleMember)

asOptionalTypeNode :: forall r. Node r -> Maybe Nodes.OptionalTypeNode
asOptionalTypeNode = toMaybe <<< runFn1 asOptionalTypeNodeImpl

foreign import asOptionalTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.OptionalTypeNode)

asRestTypeNode :: forall r. Node r -> Maybe Nodes.RestTypeNode
asRestTypeNode = toMaybe <<< runFn1 asRestTypeNodeImpl

foreign import asRestTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.RestTypeNode)

asUnionTypeNode :: forall r. Node r -> Maybe Nodes.UnionTypeNode
asUnionTypeNode = toMaybe <<< runFn1 asUnionTypeNodeImpl

foreign import asUnionTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.UnionTypeNode)

asIntersectionTypeNode :: forall r. Node r -> Maybe Nodes.IntersectionTypeNode
asIntersectionTypeNode = toMaybe <<< runFn1 asIntersectionTypeNodeImpl

foreign import asIntersectionTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.IntersectionTypeNode)

asConditionalTypeNode :: forall r. Node r -> Maybe Nodes.ConditionalTypeNode
asConditionalTypeNode = toMaybe <<< runFn1 asConditionalTypeNodeImpl

foreign import asConditionalTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ConditionalTypeNode)

asInferTypeNode :: forall r. Node r -> Maybe Nodes.InferTypeNode
asInferTypeNode = toMaybe <<< runFn1 asInferTypeNodeImpl

foreign import asInferTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.InferTypeNode)

asParenthesizedTypeNode :: forall r. Node r -> Maybe Nodes.ParenthesizedTypeNode
asParenthesizedTypeNode = toMaybe <<< runFn1 asParenthesizedTypeNodeImpl

foreign import asParenthesizedTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ParenthesizedTypeNode)

asThisTypeNode :: forall r. Node r -> Maybe Nodes.ThisTypeNode
asThisTypeNode = toMaybe <<< runFn1 asThisTypeNodeImpl

foreign import asThisTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ThisTypeNode)

asTypeOperatorNode :: forall r. Node r -> Maybe Nodes.TypeOperatorNode
asTypeOperatorNode = toMaybe <<< runFn1 asTypeOperatorNodeImpl

foreign import asTypeOperatorNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypeOperatorNode)

asIndexedAccessTypeNode :: forall r. Node r -> Maybe Nodes.IndexedAccessTypeNode
asIndexedAccessTypeNode = toMaybe <<< runFn1 asIndexedAccessTypeNodeImpl

foreign import asIndexedAccessTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.IndexedAccessTypeNode)

asMappedTypeNode :: forall r. Node r -> Maybe Nodes.MappedTypeNode
asMappedTypeNode = toMaybe <<< runFn1 asMappedTypeNodeImpl

foreign import asMappedTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.MappedTypeNode)

asLiteralTypeNode :: forall r. Node r -> Maybe Nodes.LiteralTypeNode
asLiteralTypeNode = toMaybe <<< runFn1 asLiteralTypeNodeImpl

foreign import asLiteralTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.LiteralTypeNode)

asImportTypeNode :: forall r. Node r -> Maybe Nodes.ImportTypeNode
asImportTypeNode = toMaybe <<< runFn1 asImportTypeNodeImpl

foreign import asImportTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ImportTypeNode)

asTemplateLiteralTypeSpan :: forall r. Node r -> Maybe Nodes.TemplateLiteralTypeSpan
asTemplateLiteralTypeSpan = toMaybe <<< runFn1 asTemplateLiteralTypeSpanImpl

foreign import asTemplateLiteralTypeSpanImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TemplateLiteralTypeSpan)

asTemplateLiteralTypeNode :: forall r. Node r -> Maybe Nodes.TemplateLiteralTypeNode
asTemplateLiteralTypeNode = toMaybe <<< runFn1 asTemplateLiteralTypeNodeImpl

foreign import asTemplateLiteralTypeNodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TemplateLiteralTypeNode)

-- Binding patterns

asObjectBindingPattern :: forall r. Node r -> Maybe Nodes.ObjectBindingPattern
asObjectBindingPattern = toMaybe <<< runFn1 asObjectBindingPatternImpl

foreign import asObjectBindingPatternImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ObjectBindingPattern)

asArrayBindingPattern :: forall r. Node r -> Maybe Nodes.ArrayBindingPattern
asArrayBindingPattern = toMaybe <<< runFn1 asArrayBindingPatternImpl

foreign import asArrayBindingPatternImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ArrayBindingPattern)

asBindingElement :: forall r. Node r -> Maybe Nodes.BindingElement
asBindingElement = toMaybe <<< runFn1 asBindingElementImpl

foreign import asBindingElementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.BindingElement)

-- Expression

asArrayLiteralExpression :: forall r. Node r -> Maybe Nodes.ArrayLiteralExpression
asArrayLiteralExpression = toMaybe <<< runFn1 asArrayLiteralExpressionImpl

foreign import asArrayLiteralExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ArrayLiteralExpression)

asObjectLiteralExpression :: forall r. Node r -> Maybe Nodes.ObjectLiteralExpression
asObjectLiteralExpression = toMaybe <<< runFn1 asObjectLiteralExpressionImpl

foreign import asObjectLiteralExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ObjectLiteralExpression)

asPropertyAccessExpression :: forall r. Node r -> Maybe Nodes.PropertyAccessExpression
asPropertyAccessExpression = toMaybe <<< runFn1 asPropertyAccessExpressionImpl

foreign import asPropertyAccessExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PropertyAccessExpression)

asElementAccessExpression :: forall r. Node r -> Maybe Nodes.ElementAccessExpression
asElementAccessExpression = toMaybe <<< runFn1 asElementAccessExpressionImpl

foreign import asElementAccessExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ElementAccessExpression)

asCallExpression :: forall r. Node r -> Maybe Nodes.CallExpression
asCallExpression = toMaybe <<< runFn1 asCallExpressionImpl

foreign import asCallExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.CallExpression)

asNewExpression :: forall r. Node r -> Maybe Nodes.NewExpression
asNewExpression = toMaybe <<< runFn1 asNewExpressionImpl

foreign import asNewExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NewExpression)

asTaggedTemplateExpression :: forall r. Node r -> Maybe Nodes.TaggedTemplateExpression
asTaggedTemplateExpression = toMaybe <<< runFn1 asTaggedTemplateExpressionImpl

foreign import asTaggedTemplateExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TaggedTemplateExpression)

asTypeAssertionExpression :: forall r. Node r -> Maybe Nodes.TypeAssertion
asTypeAssertionExpression = toMaybe <<< runFn1 asTypeAssertionExpressionImpl

foreign import asTypeAssertionExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypeAssertion)

asParenthesizedExpression :: forall r. Node r -> Maybe Nodes.ParenthesizedExpression
asParenthesizedExpression = toMaybe <<< runFn1 asParenthesizedExpressionImpl

foreign import asParenthesizedExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ParenthesizedExpression)

asFunctionExpression :: forall r. Node r -> Maybe Nodes.FunctionExpression
asFunctionExpression = toMaybe <<< runFn1 asFunctionExpressionImpl

foreign import asFunctionExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.FunctionExpression)

asArrowFunction :: forall r. Node r -> Maybe Nodes.ArrowFunction
asArrowFunction = toMaybe <<< runFn1 asArrowFunctionImpl

foreign import asArrowFunctionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ArrowFunction)

asDeleteExpression :: forall r. Node r -> Maybe Nodes.DeleteExpression
asDeleteExpression = toMaybe <<< runFn1 asDeleteExpressionImpl

foreign import asDeleteExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.DeleteExpression)

asTypeOfExpression :: forall r. Node r -> Maybe Nodes.TypeOfExpression
asTypeOfExpression = toMaybe <<< runFn1 asTypeOfExpressionImpl

foreign import asTypeOfExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypeOfExpression)

asVoidExpression :: forall r. Node r -> Maybe Nodes.VoidExpression
asVoidExpression = toMaybe <<< runFn1 asVoidExpressionImpl

foreign import asVoidExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.VoidExpression)

asAwaitExpression :: forall r. Node r -> Maybe Nodes.AwaitExpression
asAwaitExpression = toMaybe <<< runFn1 asAwaitExpressionImpl

foreign import asAwaitExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AwaitExpression)

asPrefixUnaryExpression :: forall r. Node r -> Maybe Nodes.PrefixUnaryExpression
asPrefixUnaryExpression = toMaybe <<< runFn1 asPrefixUnaryExpressionImpl

foreign import asPrefixUnaryExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PrefixUnaryExpression)

asPostfixUnaryExpression :: forall r. Node r -> Maybe Nodes.PostfixUnaryExpression
asPostfixUnaryExpression = toMaybe <<< runFn1 asPostfixUnaryExpressionImpl

foreign import asPostfixUnaryExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PostfixUnaryExpression)

asBinaryExpression :: forall r. Node r -> Maybe Nodes.BinaryExpression
asBinaryExpression = toMaybe <<< runFn1 asBinaryExpressionImpl

foreign import asBinaryExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.BinaryExpression)

asConditionalExpression :: forall r. Node r -> Maybe Nodes.ConditionalExpression
asConditionalExpression = toMaybe <<< runFn1 asConditionalExpressionImpl

foreign import asConditionalExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ConditionalExpression)

asTemplateExpression :: forall r. Node r -> Maybe Nodes.TemplateExpression
asTemplateExpression = toMaybe <<< runFn1 asTemplateExpressionImpl

foreign import asTemplateExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TemplateExpression)

asYieldExpression :: forall r. Node r -> Maybe Nodes.YieldExpression
asYieldExpression = toMaybe <<< runFn1 asYieldExpressionImpl

foreign import asYieldExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.YieldExpression)

asSpreadElement :: forall r. Node r -> Maybe Nodes.SpreadElement
asSpreadElement = toMaybe <<< runFn1 asSpreadElementImpl

foreign import asSpreadElementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SpreadElement)

asClassExpression :: forall r. Node r -> Maybe Nodes.ClassExpression
asClassExpression = toMaybe <<< runFn1 asClassExpressionImpl

foreign import asClassExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ClassExpression)

asOmittedExpression :: forall r. Node r -> Maybe Nodes.OmittedExpression
asOmittedExpression = toMaybe <<< runFn1 asOmittedExpressionImpl

foreign import asOmittedExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.OmittedExpression)

asExpressionWithTypeArguments :: forall r. Node r -> Maybe Nodes.ExpressionWithTypeArguments
asExpressionWithTypeArguments = toMaybe <<< runFn1 asExpressionWithTypeArgumentsImpl

foreign import asExpressionWithTypeArgumentsImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ExpressionWithTypeArguments)

asAsExpression :: forall r. Node r -> Maybe Nodes.AsExpression
asAsExpression = toMaybe <<< runFn1 asAsExpressionImpl

foreign import asAsExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AsExpression)

asNonNullExpression :: forall r. Node r -> Maybe Nodes.NonNullExpression
asNonNullExpression = toMaybe <<< runFn1 asNonNullExpressionImpl

foreign import asNonNullExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NonNullExpression)

asMetaProperty :: forall r. Node r -> Maybe Nodes.MetaProperty
asMetaProperty = toMaybe <<< runFn1 asMetaPropertyImpl

foreign import asMetaPropertyImpl :: forall r. Fn1 (Node r) (Nullable Nodes.MetaProperty)

asSyntheticExpression :: forall r. Node r -> Maybe Nodes.SyntheticExpression
asSyntheticExpression = toMaybe <<< runFn1 asSyntheticExpressionImpl

foreign import asSyntheticExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SyntheticExpression)

asPartiallyEmittedExpression :: forall r. Node r -> Maybe Nodes.PartiallyEmittedExpression
asPartiallyEmittedExpression = toMaybe <<< runFn1 asPartiallyEmittedExpressionImpl

foreign import asPartiallyEmittedExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PartiallyEmittedExpression)

asCommaListExpression :: forall r. Node r -> Maybe Nodes.CommaListExpression
asCommaListExpression = toMaybe <<< runFn1 asCommaListExpressionImpl

foreign import asCommaListExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.CommaListExpression)

-- Misc

asTemplateSpan :: forall r. Node r -> Maybe Nodes.TemplateSpan
asTemplateSpan = toMaybe <<< runFn1 asTemplateSpanImpl

foreign import asTemplateSpanImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TemplateSpan)

asSemicolonClassElement :: forall r. Node r -> Maybe Nodes.SemicolonClassElement
asSemicolonClassElement = toMaybe <<< runFn1 asSemicolonClassElementImpl

foreign import asSemicolonClassElementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SemicolonClassElement)

-- Elements

asBlock :: forall r. Node r -> Maybe Nodes.Block
asBlock = toMaybe <<< runFn1 asBlockImpl

foreign import asBlockImpl :: forall r. Fn1 (Node r) (Nullable Nodes.Block)

asVariableStatement :: forall r. Node r -> Maybe Nodes.VariableStatement
asVariableStatement = toMaybe <<< runFn1 asVariableStatementImpl

foreign import asVariableStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.VariableStatement)

asEmptyStatement :: forall r. Node r -> Maybe Nodes.EmptyStatement
asEmptyStatement = toMaybe <<< runFn1 asEmptyStatementImpl

foreign import asEmptyStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.EmptyStatement)

asExpressionStatement :: forall r. Node r -> Maybe Nodes.ExpressionStatement
asExpressionStatement = toMaybe <<< runFn1 asExpressionStatementImpl

foreign import asExpressionStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ExpressionStatement)

asIfStatement :: forall r. Node r -> Maybe Nodes.IfStatement
asIfStatement = toMaybe <<< runFn1 asIfStatementImpl

foreign import asIfStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.IfStatement)

asDoStatement :: forall r. Node r -> Maybe Nodes.DoStatement
asDoStatement = toMaybe <<< runFn1 asDoStatementImpl

foreign import asDoStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.DoStatement)

asWhileStatement :: forall r. Node r -> Maybe Nodes.WhileStatement
asWhileStatement = toMaybe <<< runFn1 asWhileStatementImpl

foreign import asWhileStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.WhileStatement)

asForStatement :: forall r. Node r -> Maybe Nodes.ForStatement
asForStatement = toMaybe <<< runFn1 asForStatementImpl

foreign import asForStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ForStatement)

asForInStatement :: forall r. Node r -> Maybe Nodes.ForInStatement
asForInStatement = toMaybe <<< runFn1 asForInStatementImpl

foreign import asForInStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ForInStatement)

asForOfStatement :: forall r. Node r -> Maybe Nodes.ForOfStatement
asForOfStatement = toMaybe <<< runFn1 asForOfStatementImpl

foreign import asForOfStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ForOfStatement)

asContinueStatement :: forall r. Node r -> Maybe Nodes.ContinueStatement
asContinueStatement = toMaybe <<< runFn1 asContinueStatementImpl

foreign import asContinueStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ContinueStatement)

asBreakStatement :: forall r. Node r -> Maybe Nodes.BreakStatement
asBreakStatement = toMaybe <<< runFn1 asBreakStatementImpl

foreign import asBreakStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.BreakStatement)

asReturnStatement :: forall r. Node r -> Maybe Nodes.ReturnStatement
asReturnStatement = toMaybe <<< runFn1 asReturnStatementImpl

foreign import asReturnStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ReturnStatement)

asWithStatement :: forall r. Node r -> Maybe Nodes.WithStatement
asWithStatement = toMaybe <<< runFn1 asWithStatementImpl

foreign import asWithStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.WithStatement)

asSwitchStatement :: forall r. Node r -> Maybe Nodes.SwitchStatement
asSwitchStatement = toMaybe <<< runFn1 asSwitchStatementImpl

foreign import asSwitchStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SwitchStatement)

asLabeledStatement :: forall r. Node r -> Maybe Nodes.LabeledStatement
asLabeledStatement = toMaybe <<< runFn1 asLabeledStatementImpl

foreign import asLabeledStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.LabeledStatement)

asThrowStatement :: forall r. Node r -> Maybe Nodes.ThrowStatement
asThrowStatement = toMaybe <<< runFn1 asThrowStatementImpl

foreign import asThrowStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ThrowStatement)

asTryStatement :: forall r. Node r -> Maybe Nodes.TryStatement
asTryStatement = toMaybe <<< runFn1 asTryStatementImpl

foreign import asTryStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TryStatement)

asDebuggerStatement :: forall r. Node r -> Maybe Nodes.DebuggerStatement
asDebuggerStatement = toMaybe <<< runFn1 asDebuggerStatementImpl

foreign import asDebuggerStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.DebuggerStatement)

asVariableDeclaration :: forall r. Node r -> Maybe Nodes.VariableDeclaration
asVariableDeclaration = toMaybe <<< runFn1 asVariableDeclarationImpl

foreign import asVariableDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.VariableDeclaration)

asVariableDeclarationList :: forall r. Node r -> Maybe Nodes.VariableDeclarationList
asVariableDeclarationList = toMaybe <<< runFn1 asVariableDeclarationListImpl

foreign import asVariableDeclarationListImpl :: forall r. Fn1 (Node r) (Nullable Nodes.VariableDeclarationList)

asFunctionDeclaration :: forall r. Node r -> Maybe Nodes.FunctionDeclaration
asFunctionDeclaration = toMaybe <<< runFn1 asFunctionDeclarationImpl

foreign import asFunctionDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.FunctionDeclaration)

asClassDeclaration :: forall r. Node r -> Maybe Nodes.ClassLikeDeclaration
asClassDeclaration = toMaybe <<< runFn1 asClassDeclarationImpl

foreign import asClassDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ClassDeclaration)

asInterfaceDeclaration :: forall r. Node r -> Maybe Nodes.InterfaceDeclaration
asInterfaceDeclaration = toMaybe <<< runFn1 asInterfaceDeclarationImpl

foreign import asInterfaceDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.InterfaceDeclaration)

asTypeAliasDeclaration :: forall r. Node r -> Maybe Nodes.TypeAliasDeclaration
asTypeAliasDeclaration = toMaybe <<< runFn1 asTypeAliasDeclarationImpl

foreign import asTypeAliasDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.TypeAliasDeclaration)

asEnumDeclaration :: forall r. Node r -> Maybe Nodes.EnumDeclaration
asEnumDeclaration = toMaybe <<< runFn1 asEnumDeclarationImpl

foreign import asEnumDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.EnumDeclaration)

asModuleDeclaration :: forall r. Node r -> Maybe Nodes.ModuleDeclaration
asModuleDeclaration = toMaybe <<< runFn1 asModuleDeclarationImpl

foreign import asModuleDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ModuleDeclaration)

asModuleBlock :: forall r. Node r -> Maybe Nodes.ModuleBlock
asModuleBlock = toMaybe <<< runFn1 asModuleBlockImpl

foreign import asModuleBlockImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ModuleBlock)

asCaseBlock :: forall r. Node r -> Maybe Nodes.CaseBlock
asCaseBlock = toMaybe <<< runFn1 asCaseBlockImpl

foreign import asCaseBlockImpl :: forall r. Fn1 (Node r) (Nullable Nodes.CaseBlock)

asNamespaceExportDeclaration :: forall r. Node r -> Maybe Nodes.NamespaceExportDeclaration
asNamespaceExportDeclaration = toMaybe <<< runFn1 asNamespaceExportDeclarationImpl

foreign import asNamespaceExportDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NamespaceExportDeclaration)

asImportEqualsDeclaration :: forall r. Node r -> Maybe Nodes.ImportEqualsDeclaration
asImportEqualsDeclaration = toMaybe <<< runFn1 asImportEqualsDeclarationImpl

foreign import asImportEqualsDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ImportEqualsDeclaration)

asImportDeclaration :: forall r. Node r -> Maybe Nodes.ImportDeclaration
asImportDeclaration = toMaybe <<< runFn1 asImportDeclarationImpl

foreign import asImportDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ImportDeclaration)

asImportClause :: forall r. Node r -> Maybe Nodes.ImportClause
asImportClause = toMaybe <<< runFn1 asImportClauseImpl

foreign import asImportClauseImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ImportClause)

asAssertClause :: forall r. Node r -> Maybe Nodes.AssertClause
asAssertClause = toMaybe <<< runFn1 asAssertClauseImpl

foreign import asAssertClauseImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AssertClause)

asAssertEntry :: forall r. Node r -> Maybe Nodes.AssertEntry
asAssertEntry = toMaybe <<< runFn1 asAssertEntryImpl

foreign import asAssertEntryImpl :: forall r. Fn1 (Node r) (Nullable Nodes.AssertEntry)

asNamespaceImport :: forall r. Node r -> Maybe Nodes.NamespaceImport
asNamespaceImport = toMaybe <<< runFn1 asNamespaceImportImpl

foreign import asNamespaceImportImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NamespaceImport)

asNamespaceExport :: forall r. Node r -> Maybe Nodes.NamespaceExport
asNamespaceExport = toMaybe <<< runFn1 asNamespaceExportImpl

foreign import asNamespaceExportImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NamespaceExport)

asNamedImports :: forall r. Node r -> Maybe Nodes.NamedImports
asNamedImports = toMaybe <<< runFn1 asNamedImportsImpl

foreign import asNamedImportsImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NamedImports)

asImportSpecifier :: forall r. Node r -> Maybe Nodes.ImportSpecifier
asImportSpecifier = toMaybe <<< runFn1 asImportSpecifierImpl

foreign import asImportSpecifierImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ImportSpecifier)

asExportAssignment :: forall r. Node r -> Maybe Nodes.ExportAssignment
asExportAssignment = toMaybe <<< runFn1 asExportAssignmentImpl

foreign import asExportAssignmentImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ExportAssignment)

asExportDeclaration :: forall r. Node r -> Maybe Nodes.ExportDeclaration
asExportDeclaration = toMaybe <<< runFn1 asExportDeclarationImpl

foreign import asExportDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ExportDeclaration)

asNamedExports :: forall r. Node r -> Maybe Nodes.NamedExports
asNamedExports = toMaybe <<< runFn1 asNamedExportsImpl

foreign import asNamedExportsImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NamedExports)

asExportSpecifier :: forall r. Node r -> Maybe Nodes.ExportSpecifier
asExportSpecifier = toMaybe <<< runFn1 asExportSpecifierImpl

foreign import asExportSpecifierImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ExportSpecifier)

asMissingDeclaration :: forall r. Node r -> Maybe Nodes.MissingDeclaration
asMissingDeclaration = toMaybe <<< runFn1 asMissingDeclarationImpl

foreign import asMissingDeclarationImpl :: forall r. Fn1 (Node r) (Nullable Nodes.MissingDeclaration)

asNotEmittedStatement :: forall r. Node r -> Maybe Nodes.NotEmittedStatement
asNotEmittedStatement = toMaybe <<< runFn1 asNotEmittedStatementImpl

foreign import asNotEmittedStatementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.NotEmittedStatement)

-- /* @internal */
-- asSyntheticReference = toMaybe <<< runFn1 asSyntheticReferenceImpl
-- foreign import asSyntheticReferenceImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SyntheticReferenceExpression)
-- 
-- /* @internal */
-- asMergeDeclarationMarker = toMaybe <<< runFn1 asMergeDeclarationMarkerImpl
-- foreign import asMergeDeclarationMarkerImpl :: forall r. Fn1 (Node r) (Nullable Nodes.MergeDeclarationMarker)
-- 
-- /* @internal */
-- asEndOfDeclarationMarker = toMaybe <<< runFn1 asEndOfDeclarationMarkerImpl
-- foreign import asEndOfDeclarationMarkerImpl :: forall r. Fn1 (Node r) (Nullable Nodes.EndOfDeclarationMarker)

-- Module References

asExternalModuleReference :: forall r. Node r -> Maybe Nodes.ExternalModuleReference
asExternalModuleReference = toMaybe <<< runFn1 asExternalModuleReferenceImpl

foreign import asExternalModuleReferenceImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ExternalModuleReference)

-- JSX

asJsxElement :: forall r. Node r -> Maybe Nodes.JsxElement
asJsxElement = toMaybe <<< runFn1 asJsxElementImpl

foreign import asJsxElementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxElement)

asJsxSelfClosingElement :: forall r. Node r -> Maybe Nodes.JsxSelfClosingElement
asJsxSelfClosingElement = toMaybe <<< runFn1 asJsxSelfClosingElementImpl

foreign import asJsxSelfClosingElementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxSelfClosingElement)

asJsxOpeningElement :: forall r. Node r -> Maybe Nodes.JsxOpeningElement
asJsxOpeningElement = toMaybe <<< runFn1 asJsxOpeningElementImpl

foreign import asJsxOpeningElementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxOpeningElement)

asJsxClosingElement :: forall r. Node r -> Maybe Nodes.JsxClosingElement
asJsxClosingElement = toMaybe <<< runFn1 asJsxClosingElementImpl

foreign import asJsxClosingElementImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxClosingElement)

asJsxFragment :: forall r. Node r -> Maybe Nodes.JsxFragment
asJsxFragment = toMaybe <<< runFn1 asJsxFragmentImpl

foreign import asJsxFragmentImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxFragment)

asJsxOpeningFragment :: forall r. Node r -> Maybe Nodes.JsxOpeningFragment
asJsxOpeningFragment = toMaybe <<< runFn1 asJsxOpeningFragmentImpl

foreign import asJsxOpeningFragmentImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxOpeningFragment)

asJsxClosingFragment :: forall r. Node r -> Maybe Nodes.JsxClosingFragment
asJsxClosingFragment = toMaybe <<< runFn1 asJsxClosingFragmentImpl

foreign import asJsxClosingFragmentImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxClosingFragment)

asJsxAttribute :: forall r. Node r -> Maybe Nodes.JsxAttribute
asJsxAttribute = toMaybe <<< runFn1 asJsxAttributeImpl

foreign import asJsxAttributeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxAttribute)

asJsxAttributes :: forall r. Node r -> Maybe Nodes.JsxAttributes
asJsxAttributes = toMaybe <<< runFn1 asJsxAttributesImpl

foreign import asJsxAttributesImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxAttributes)

asJsxSpreadAttribute :: forall r. Node r -> Maybe Nodes.JsxSpreadAttribute
asJsxSpreadAttribute = toMaybe <<< runFn1 asJsxSpreadAttributeImpl

foreign import asJsxSpreadAttributeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxSpreadAttribute)

asJsxExpression :: forall r. Node r -> Maybe Nodes.JsxExpression
asJsxExpression = toMaybe <<< runFn1 asJsxExpressionImpl

foreign import asJsxExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JsxExpression)

-- Clauses

asCaseClause :: forall r. Node r -> Maybe Nodes.CaseClause
asCaseClause = toMaybe <<< runFn1 asCaseClauseImpl

foreign import asCaseClauseImpl :: forall r. Fn1 (Node r) (Nullable Nodes.CaseClause)

asDefaultClause :: forall r. Node r -> Maybe Nodes.DefaultClause
asDefaultClause = toMaybe <<< runFn1 asDefaultClauseImpl

foreign import asDefaultClauseImpl :: forall r. Fn1 (Node r) (Nullable Nodes.DefaultClause)

asHeritageClause :: forall r. Node r -> Maybe Nodes.HeritageClause
asHeritageClause = toMaybe <<< runFn1 asHeritageClauseImpl

foreign import asHeritageClauseImpl :: forall r. Fn1 (Node r) (Nullable Nodes.HeritageClause)

asCatchClause :: forall r. Node r -> Maybe Nodes.CatchClause
asCatchClause = toMaybe <<< runFn1 asCatchClauseImpl

foreign import asCatchClauseImpl :: forall r. Fn1 (Node r) (Nullable Nodes.CatchClause)

-- Property assignments

asPropertyAssignment :: forall r. Node r -> Maybe Nodes.PropertyAssignment
asPropertyAssignment = toMaybe <<< runFn1 asPropertyAssignmentImpl

foreign import asPropertyAssignmentImpl :: forall r. Fn1 (Node r) (Nullable Nodes.PropertyAssignment)

asShorthandPropertyAssignment :: forall r. Node r -> Maybe Nodes.ShorthandPropertyAssignment
asShorthandPropertyAssignment = toMaybe <<< runFn1 asShorthandPropertyAssignmentImpl

foreign import asShorthandPropertyAssignmentImpl :: forall r. Fn1 (Node r) (Nullable Nodes.ShorthandPropertyAssignment)

asSpreadAssignment :: forall r. Node r -> Maybe Nodes.SpreadAssignment
asSpreadAssignment = toMaybe <<< runFn1 asSpreadAssignmentImpl

foreign import asSpreadAssignmentImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SpreadAssignment)

-- Enum

asEnumMember :: forall r. Node r -> Maybe Nodes.EnumMember
asEnumMember = toMaybe <<< runFn1 asEnumMemberImpl

foreign import asEnumMemberImpl :: forall r. Fn1 (Node r) (Nullable Nodes.EnumMember)

-- Unparsed

-- TODO(rbuckton): isUnparsedPrologue

asUnparsedPrepend :: forall r. Node r -> Maybe Nodes.UnparsedPrepend
asUnparsedPrepend = toMaybe <<< runFn1 asUnparsedPrependImpl

foreign import asUnparsedPrependImpl :: forall r. Fn1 (Node r) (Nullable Nodes.UnparsedPrepend)

-- TODO(rbuckton): isUnparsedText
-- TODO(rbuckton): isUnparsedInternalText
-- TODO(rbuckton): isUnparsedSyntheticReference

-- Top-level nodes
asSourceFile :: forall r. Node r -> Maybe Nodes.SourceFile
asSourceFile = toMaybe <<< runFn1 asSourceFileImpl

foreign import asSourceFileImpl :: forall r. Fn1 (Node r) (Nullable Nodes.SourceFile)

asBundle :: forall r. Node r -> Maybe Nodes.Bundle
asBundle = toMaybe <<< runFn1 asBundleImpl

foreign import asBundleImpl :: forall r. Fn1 (Node r) (Nullable Nodes.Bundle)

asUnparsedSource :: forall r. Node r -> Maybe Nodes.UnparsedSource
asUnparsedSource = toMaybe <<< runFn1 asUnparsedSourceImpl

foreign import asUnparsedSourceImpl :: forall r. Fn1 (Node r) (Nullable Nodes.UnparsedSource)

-- TODO(rbuckton): isInputFiles

-- JSDoc Elements

asJSDocTypeExpression :: forall r. Node r -> Maybe Nodes.JSDocTypeExpression
asJSDocTypeExpression = toMaybe <<< runFn1 asJSDocTypeExpressionImpl

foreign import asJSDocTypeExpressionImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocTypeExpression)

asJSDocNameReference :: forall r. Node r -> Maybe Nodes.JSDocNameReference
asJSDocNameReference = toMaybe <<< runFn1 asJSDocNameReferenceImpl

foreign import asJSDocNameReferenceImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocNameReference)

asJSDocMemberName :: forall r. Node r -> Maybe Nodes.JSDocMemberName
asJSDocMemberName = toMaybe <<< runFn1 asJSDocMemberNameImpl

foreign import asJSDocMemberNameImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocMemberName)

asJSDocLink :: forall r. Node r -> Maybe Nodes.JSDocLink
asJSDocLink = toMaybe <<< runFn1 asJSDocLinkImpl

foreign import asJSDocLinkImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocLink)

asJSDocLinkCode :: forall r. Node r -> Maybe Nodes.JSDocLinkCode
asJSDocLinkCode = toMaybe <<< runFn1 asJSDocLinkCodeImpl

foreign import asJSDocLinkCodeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocLinkCode)

asJSDocLinkPlain :: forall r. Node r -> Maybe Nodes.JSDocLinkPlain
asJSDocLinkPlain = toMaybe <<< runFn1 asJSDocLinkPlainImpl

foreign import asJSDocLinkPlainImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocLinkPlain)

asJSDocAllType :: forall r. Node r -> Maybe Nodes.JSDocAllType
asJSDocAllType = toMaybe <<< runFn1 asJSDocAllTypeImpl

foreign import asJSDocAllTypeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocAllType)

asJSDocUnknownType :: forall r. Node r -> Maybe Nodes.JSDocUnknownType
asJSDocUnknownType = toMaybe <<< runFn1 asJSDocUnknownTypeImpl

foreign import asJSDocUnknownTypeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocUnknownType)

asJSDocNullableType :: forall r. Node r -> Maybe Nodes.JSDocNullableType
asJSDocNullableType = toMaybe <<< runFn1 asJSDocNullableTypeImpl

foreign import asJSDocNullableTypeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocNullableType)

asJSDocNonNullableType :: forall r. Node r -> Maybe Nodes.JSDocNonNullableType
asJSDocNonNullableType = toMaybe <<< runFn1 asJSDocNonNullableTypeImpl

foreign import asJSDocNonNullableTypeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocNonNullableType)

asJSDocOptionalType :: forall r. Node r -> Maybe Nodes.JSDocOptionalType
asJSDocOptionalType = toMaybe <<< runFn1 asJSDocOptionalTypeImpl

foreign import asJSDocOptionalTypeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocOptionalType)

asJSDocFunctionType :: forall r. Node r -> Maybe Nodes.JSDocFunctionType
asJSDocFunctionType = toMaybe <<< runFn1 asJSDocFunctionTypeImpl

foreign import asJSDocFunctionTypeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocFunctionType)

asJSDocVariadicType :: forall r. Node r -> Maybe Nodes.JSDocVariadicType
asJSDocVariadicType = toMaybe <<< runFn1 asJSDocVariadicTypeImpl

foreign import asJSDocVariadicTypeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocVariadicType)

asJSDocNamepathType :: forall r. Node r -> Maybe Nodes.JSDocNamepathType
asJSDocNamepathType = toMaybe <<< runFn1 asJSDocNamepathTypeImpl

foreign import asJSDocNamepathTypeImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocNamepathType)

asJSDoc :: forall r. Node r -> Maybe Nodes.JSDoc
asJSDoc = toMaybe <<< runFn1 asJSDocImpl

foreign import asJSDocImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDoc)

asJSDocTypeLiteral :: forall r. Node r -> Maybe Nodes.JSDocTypeLiteral
asJSDocTypeLiteral = toMaybe <<< runFn1 asJSDocTypeLiteralImpl

foreign import asJSDocTypeLiteralImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocTypeLiteral)

asJSDocSignature :: forall r. Node r -> Maybe Nodes.JSDocSignature
asJSDocSignature = toMaybe <<< runFn1 asJSDocSignatureImpl

foreign import asJSDocSignatureImpl :: forall r. Fn1 (Node r) (Nullable Nodes.JSDocSignature)

-- Our extras

asSyntaxList :: forall k. Node k -> Maybe Nodes.SyntaxList
asSyntaxList = toMaybe <<< asSyntaxListImpl

foreign import asSyntaxListImpl :: forall k. Node k -> Nullable Nodes.SyntaxList

