module TypeScript.Compiler.Factory.NodeTests where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import TypeScript.Compiler.Types (Node)
import TypeScript.Compiler.Types.Nodes as Nodes

-- Automatically converted by <Ctrl-C><Ctrl-V> of the NodeTests.ts content and "nearly" this regexp:
-- s/export const \([^ ]*\)Impl.*): ts.\([^ ]*\).*/\1 = toMaybe <<< runFn1 \1Impl\r\rforeign import \1Impl :: forall l r. Fn1 (Node l r) (Nullable Nodes.\2/)
--
-- Order of definitions in this file follows the original typescript module.
-- All extra node testing funcitons are added at the end of the file.

asNumericLiteral :: forall l r. Node l r -> Maybe Nodes.NumericLiteral
asNumericLiteral = toMaybe <<< runFn1 asNumericLiteralImpl

foreign import asNumericLiteralImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NumericLiteral)

asBigIntLiteral :: forall l r. Node l r -> Maybe Nodes.BigIntLiteral
asBigIntLiteral = toMaybe <<< runFn1 asBigIntLiteralImpl

foreign import asBigIntLiteralImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.BigIntLiteral)

asStringLiteral :: forall l r. Node l r -> Maybe Nodes.StringLiteral
asStringLiteral = toMaybe <<< runFn1 asStringLiteralImpl

foreign import asStringLiteralImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.StringLiteral)

asJsxText :: forall l r. Node l r -> Maybe Nodes.JsxText
asJsxText = toMaybe <<< runFn1 asJsxTextImpl

foreign import asJsxTextImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxText)

asRegularExpressionLiteral :: forall l r. Node l r -> Maybe Nodes.RegularExpressionLiteral
asRegularExpressionLiteral = toMaybe <<< runFn1 asRegularExpressionLiteralImpl

foreign import asRegularExpressionLiteralImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.RegularExpressionLiteral)

asNoSubstitutionTemplateLiteral :: forall l r. Node l r -> Maybe Nodes.NoSubstitutionTemplateLiteral
asNoSubstitutionTemplateLiteral = toMaybe <<< runFn1 asNoSubstitutionTemplateLiteralImpl

foreign import asNoSubstitutionTemplateLiteralImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NoSubstitutionTemplateLiteral)

-- Pseudo-literals

asTemplateHead :: forall l r. Node l r -> Maybe Nodes.TemplateHead
asTemplateHead = toMaybe <<< runFn1 asTemplateHeadImpl

foreign import asTemplateHeadImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TemplateHead)

asTemplateMiddle :: forall l r. Node l r -> Maybe Nodes.TemplateMiddle
asTemplateMiddle = toMaybe <<< runFn1 asTemplateMiddleImpl

foreign import asTemplateMiddleImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TemplateMiddle)

asTemplateTail :: forall l r. Node l r -> Maybe Nodes.TemplateTail
asTemplateTail = toMaybe <<< runFn1 asTemplateTailImpl

foreign import asTemplateTailImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TemplateTail)

-- Punctuation

asDotDotDotToken :: forall l r. Node l r -> Maybe Nodes.DotDotDotToken
asDotDotDotToken = toMaybe <<< runFn1 asDotDotDotTokenImpl

foreign import asDotDotDotTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.DotDotDotToken)

-- /*@internal*/
-- asCommaToken = toMaybe <<< runFn1 asCommaTokenImpl
-- foreign import asCommaTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.Token<ts.SyntaxKind.CommaToken>)

asPlusToken :: forall l r. Node l r -> Maybe Nodes.PlusToken
asPlusToken = toMaybe <<< runFn1 asPlusTokenImpl

foreign import asPlusTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PlusToken)

asMinusToken :: forall l r. Node l r -> Maybe Nodes.MinusToken
asMinusToken = toMaybe <<< runFn1 asMinusTokenImpl

foreign import asMinusTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.MinusToken)

asAsteriskToken :: forall l r. Node l r -> Maybe Nodes.AsteriskToken
asAsteriskToken = toMaybe <<< runFn1 asAsteriskTokenImpl

foreign import asAsteriskTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AsteriskToken)

-- /*@internal*/
-- asExclamationToken = toMaybe <<< runFn1 asExclamationTokenImpl
-- foreign import asExclamationTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ExclamationToken)
-- 
-- /*@internal*/
-- asQuestionToken = toMaybe <<< runFn1 asQuestionTokenImpl
-- foreign import asQuestionTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.QuestionToken)
-- 
-- /*@internal*/
-- asColonToken = toMaybe <<< runFn1 asColonTokenImpl
-- foreign import asColonTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ColonToken)
-- 
-- /*@internal*/
-- asQuestionDotToken = toMaybe <<< runFn1 asQuestionDotTokenImpl
-- foreign import asQuestionDotTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.QuestionDotToken)
-- 
-- /*@internal*/
-- asEqualsGreaterThanToken = toMaybe <<< runFn1 asEqualsGreaterThanTokenImpl
-- foreign import asEqualsGreaterThanTokenImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.EqualsGreaterThanToken)

-- Identifiers

asIdentifier :: forall l r. Node l r -> Maybe Nodes.Identifier
asIdentifier = toMaybe <<< runFn1 asIdentifierImpl

foreign import asIdentifierImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.Identifier)

asPrivateIdentifier :: forall l r. Node l r -> Maybe Nodes.PrivateIdentifier
asPrivateIdentifier = toMaybe <<< runFn1 asPrivateIdentifierImpl

foreign import asPrivateIdentifierImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PrivateIdentifier)

-- Reserved Words

-- /* @internal */
-- asExportModifier = toMaybe <<< runFn1 asExportModifierImpl
-- foreign import asExportModifierImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ExportKeyword)
-- 
-- /* @internal */
-- asAsyncModifier = toMaybe <<< runFn1 asAsyncModifierImpl
-- foreign import asAsyncModifierImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AsyncKeyword)
-- 
-- /* @internal */
-- asAssertsKeyword = toMaybe <<< runFn1 asAssertsKeywordImpl
-- foreign import asAssertsKeywordImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AssertsKeyword)
-- 
-- /* @internal */
-- asAwaitKeyword = toMaybe <<< runFn1 asAwaitKeywordImpl
-- foreign import asAwaitKeywordImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AwaitKeyword)
-- 
-- /* @internal */
-- asReadonlyKeyword = toMaybe <<< runFn1 asReadonlyKeywordImpl
-- foreign import asReadonlyKeywordImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ReadonlyKeyword)
-- 
-- /* @internal */
-- asStaticModifier = toMaybe <<< runFn1 asStaticModifierImpl
-- foreign import asStaticModifierImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.StaticKeyword)
-- 
-- /* @internal */
-- asAbstractModifier = toMaybe <<< runFn1 asAbstractModifierImpl
-- foreign import asAbstractModifierImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AbstractKeyword)
-- 
-- /*@internal*/
-- asSuperKeyword = toMaybe <<< runFn1 asSuperKeywordImpl
-- 
-- foreign import asSuperKeywordImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SuperExpression)
-- 
-- /*@internal*/
-- asImportKeyword = toMaybe <<< runFn1 asImportKeywordImpl
-- 
-- foreign import asImportKeywordImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ImportExpression)

-- Names

asQualifiedName :: forall l r. Node l r -> Maybe Nodes.QualifiedName
asQualifiedName = toMaybe <<< runFn1 asQualifiedNameImpl

foreign import asQualifiedNameImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.QualifiedName)

asComputedPropertyName :: forall l r. Node l r -> Maybe Nodes.ComputedPropertyName
asComputedPropertyName = toMaybe <<< runFn1 asComputedPropertyNameImpl

foreign import asComputedPropertyNameImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ComputedPropertyName)

-- Signature elements

asTypeParameterDeclaration :: forall l r. Node l r -> Maybe Nodes.TypeParameterDeclaration
asTypeParameterDeclaration = toMaybe <<< runFn1 asTypeParameterDeclarationImpl

foreign import asTypeParameterDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypeParameterDeclaration)

asParameterDeclaration :: forall l r. Node l r -> Maybe Nodes.ParameterDeclaration
asParameterDeclaration = toMaybe <<< runFn1 asParameterImpl

foreign import asParameterImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ParameterDeclaration)

asDecorator :: forall l r. Node l r -> Maybe Nodes.Decorator
asDecorator = toMaybe <<< runFn1 asDecoratorImpl

foreign import asDecoratorImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.Decorator)

-- TypeMember

asPropertySignature :: forall l r. Node l r -> Maybe Nodes.PropertySignature
asPropertySignature = toMaybe <<< runFn1 asPropertySignatureImpl

foreign import asPropertySignatureImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PropertySignature)

asPropertyDeclaration :: forall l r. Node l r -> Maybe Nodes.PropertyDeclaration
asPropertyDeclaration = toMaybe <<< runFn1 asPropertyDeclarationImpl

foreign import asPropertyDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PropertyDeclaration)

asMethodSignature :: forall l r. Node l r -> Maybe Nodes.MethodSignature
asMethodSignature = toMaybe <<< runFn1 asMethodSignatureImpl

foreign import asMethodSignatureImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.MethodSignature)

asMethodDeclaration :: forall l r. Node l r -> Maybe Nodes.MethodDeclaration
asMethodDeclaration = toMaybe <<< runFn1 asMethodDeclarationImpl

foreign import asMethodDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.MethodDeclaration)

asClassStaticBlockDeclaration :: forall l r. Node l r -> Maybe Nodes.ClassStaticBlockDeclaration
asClassStaticBlockDeclaration = toMaybe <<< runFn1 asClassStaticBlockDeclarationImpl

foreign import asClassStaticBlockDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ClassStaticBlockDeclaration)

asConstructorDeclaration :: forall l r. Node l r -> Maybe Nodes.ConstructorDeclaration
asConstructorDeclaration = toMaybe <<< runFn1 asConstructorDeclarationImpl

foreign import asConstructorDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ConstructorDeclaration)

asGetAccessorDeclaration :: forall l r. Node l r -> Maybe Nodes.GetAccessorDeclaration
asGetAccessorDeclaration = toMaybe <<< runFn1 asGetAccessorDeclarationImpl

foreign import asGetAccessorDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.GetAccessorDeclaration)

asSetAccessorDeclaration :: forall l r. Node l r -> Maybe Nodes.SetAccessorDeclaration
asSetAccessorDeclaration = toMaybe <<< runFn1 asSetAccessorDeclarationImpl

foreign import asSetAccessorDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SetAccessorDeclaration)

asCallSignatureDeclaration :: forall l r. Node l r -> Maybe Nodes.CallSignatureDeclaration
asCallSignatureDeclaration = toMaybe <<< runFn1 asCallSignatureDeclarationImpl

foreign import asCallSignatureDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.CallSignatureDeclaration)

asConstructSignatureDeclaration :: forall l r. Node l r -> Maybe Nodes.ConstructSignatureDeclaration
asConstructSignatureDeclaration = toMaybe <<< runFn1 asConstructSignatureDeclarationImpl

foreign import asConstructSignatureDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ConstructSignatureDeclaration)

asIndexSignatureDeclaration :: forall l r. Node l r -> Maybe Nodes.IndexSignatureDeclaration
asIndexSignatureDeclaration = toMaybe <<< runFn1 asIndexSignatureDeclarationImpl

foreign import asIndexSignatureDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.IndexSignatureDeclaration)

-- Type

asTypePredicateNode :: forall l r. Node l r -> Maybe Nodes.TypePredicateNode
asTypePredicateNode = toMaybe <<< runFn1 asTypePredicateNodeImpl

foreign import asTypePredicateNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypePredicateNode)

asTypeReferenceNode :: forall l r. Node l r -> Maybe Nodes.TypeReferenceNode
asTypeReferenceNode = toMaybe <<< runFn1 asTypeReferenceNodeImpl

foreign import asTypeReferenceNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypeReferenceNode)

asFunctionTypeNode :: forall l r. Node l r -> Maybe Nodes.FunctionTypeNode
asFunctionTypeNode = toMaybe <<< runFn1 asFunctionTypeNodeImpl

foreign import asFunctionTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.FunctionTypeNode)

asConstructorTypeNode :: forall l r. Node l r -> Maybe Nodes.ConstructorTypeNode
asConstructorTypeNode = toMaybe <<< runFn1 asConstructorTypeNodeImpl

foreign import asConstructorTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ConstructorTypeNode)

asTypeQueryNode :: forall l r. Node l r -> Maybe Nodes.TypeQueryNode
asTypeQueryNode = toMaybe <<< runFn1 asTypeQueryNodeImpl

foreign import asTypeQueryNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypeQueryNode)

asTypeLiteralNode :: forall l r. Node l r -> Maybe Nodes.TypeLiteralNode
asTypeLiteralNode = toMaybe <<< runFn1 asTypeLiteralNodeImpl

foreign import asTypeLiteralNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypeLiteralNode)

asArrayTypeNode :: forall l r. Node l r -> Maybe Nodes.ArrayTypeNode
asArrayTypeNode = toMaybe <<< runFn1 asArrayTypeNodeImpl

foreign import asArrayTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ArrayTypeNode)

asTupleTypeNode :: forall l r. Node l r -> Maybe Nodes.TupleTypeNode
asTupleTypeNode = toMaybe <<< runFn1 asTupleTypeNodeImpl

foreign import asTupleTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TupleTypeNode)

asNamedTupleMember :: forall l r. Node l r -> Maybe Nodes.NamedTupleMember
asNamedTupleMember = toMaybe <<< runFn1 asNamedTupleMemberImpl

foreign import asNamedTupleMemberImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NamedTupleMember)

asOptionalTypeNode :: forall l r. Node l r -> Maybe Nodes.OptionalTypeNode
asOptionalTypeNode = toMaybe <<< runFn1 asOptionalTypeNodeImpl

foreign import asOptionalTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.OptionalTypeNode)

asRestTypeNode :: forall l r. Node l r -> Maybe Nodes.RestTypeNode
asRestTypeNode = toMaybe <<< runFn1 asRestTypeNodeImpl

foreign import asRestTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.RestTypeNode)

asUnionTypeNode :: forall l r. Node l r -> Maybe Nodes.UnionTypeNode
asUnionTypeNode = toMaybe <<< runFn1 asUnionTypeNodeImpl

foreign import asUnionTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.UnionTypeNode)

asIntersectionTypeNode :: forall l r. Node l r -> Maybe Nodes.IntersectionTypeNode
asIntersectionTypeNode = toMaybe <<< runFn1 asIntersectionTypeNodeImpl

foreign import asIntersectionTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.IntersectionTypeNode)

asConditionalTypeNode :: forall l r. Node l r -> Maybe Nodes.ConditionalTypeNode
asConditionalTypeNode = toMaybe <<< runFn1 asConditionalTypeNodeImpl

foreign import asConditionalTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ConditionalTypeNode)

asInferTypeNode :: forall l r. Node l r -> Maybe Nodes.InferTypeNode
asInferTypeNode = toMaybe <<< runFn1 asInferTypeNodeImpl

foreign import asInferTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.InferTypeNode)

asParenthesizedTypeNode :: forall l r. Node l r -> Maybe Nodes.ParenthesizedTypeNode
asParenthesizedTypeNode = toMaybe <<< runFn1 asParenthesizedTypeNodeImpl

foreign import asParenthesizedTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ParenthesizedTypeNode)

asThisTypeNode :: forall l r. Node l r -> Maybe Nodes.ThisTypeNode
asThisTypeNode = toMaybe <<< runFn1 asThisTypeNodeImpl

foreign import asThisTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ThisTypeNode)

asTypeOperatorNode :: forall l r. Node l r -> Maybe Nodes.TypeOperatorNode
asTypeOperatorNode = toMaybe <<< runFn1 asTypeOperatorNodeImpl

foreign import asTypeOperatorNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypeOperatorNode)

asIndexedAccessTypeNode :: forall l r. Node l r -> Maybe Nodes.IndexedAccessTypeNode
asIndexedAccessTypeNode = toMaybe <<< runFn1 asIndexedAccessTypeNodeImpl

foreign import asIndexedAccessTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.IndexedAccessTypeNode)

asMappedTypeNode :: forall l r. Node l r -> Maybe Nodes.MappedTypeNode
asMappedTypeNode = toMaybe <<< runFn1 asMappedTypeNodeImpl

foreign import asMappedTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.MappedTypeNode)

asLiteralTypeNode :: forall l r. Node l r -> Maybe Nodes.LiteralTypeNode
asLiteralTypeNode = toMaybe <<< runFn1 asLiteralTypeNodeImpl

foreign import asLiteralTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.LiteralTypeNode)

asImportTypeNode :: forall l r. Node l r -> Maybe Nodes.ImportTypeNode
asImportTypeNode = toMaybe <<< runFn1 asImportTypeNodeImpl

foreign import asImportTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ImportTypeNode)

asTemplateLiteralTypeSpan :: forall l r. Node l r -> Maybe Nodes.TemplateLiteralTypeSpan
asTemplateLiteralTypeSpan = toMaybe <<< runFn1 asTemplateLiteralTypeSpanImpl

foreign import asTemplateLiteralTypeSpanImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TemplateLiteralTypeSpan)

asTemplateLiteralTypeNode :: forall l r. Node l r -> Maybe Nodes.TemplateLiteralTypeNode
asTemplateLiteralTypeNode = toMaybe <<< runFn1 asTemplateLiteralTypeNodeImpl

foreign import asTemplateLiteralTypeNodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TemplateLiteralTypeNode)

-- Binding patterns

asObjectBindingPattern :: forall l r. Node l r -> Maybe Nodes.ObjectBindingPattern
asObjectBindingPattern = toMaybe <<< runFn1 asObjectBindingPatternImpl

foreign import asObjectBindingPatternImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ObjectBindingPattern)

asArrayBindingPattern :: forall l r. Node l r -> Maybe Nodes.ArrayBindingPattern
asArrayBindingPattern = toMaybe <<< runFn1 asArrayBindingPatternImpl

foreign import asArrayBindingPatternImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ArrayBindingPattern)

asBindingElement :: forall l r. Node l r -> Maybe Nodes.BindingElement
asBindingElement = toMaybe <<< runFn1 asBindingElementImpl

foreign import asBindingElementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.BindingElement)

-- Expression

asArrayLiteralExpression :: forall l r. Node l r -> Maybe Nodes.ArrayLiteralExpression
asArrayLiteralExpression = toMaybe <<< runFn1 asArrayLiteralExpressionImpl

foreign import asArrayLiteralExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ArrayLiteralExpression)

asObjectLiteralExpression :: forall l r. Node l r -> Maybe Nodes.ObjectLiteralExpression
asObjectLiteralExpression = toMaybe <<< runFn1 asObjectLiteralExpressionImpl

foreign import asObjectLiteralExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ObjectLiteralExpression)

asPropertyAccessExpression :: forall l r. Node l r -> Maybe Nodes.PropertyAccessExpression
asPropertyAccessExpression = toMaybe <<< runFn1 asPropertyAccessExpressionImpl

foreign import asPropertyAccessExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PropertyAccessExpression)

asElementAccessExpression :: forall l r. Node l r -> Maybe Nodes.ElementAccessExpression
asElementAccessExpression = toMaybe <<< runFn1 asElementAccessExpressionImpl

foreign import asElementAccessExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ElementAccessExpression)

asCallExpression :: forall l r. Node l r -> Maybe Nodes.CallExpression
asCallExpression = toMaybe <<< runFn1 asCallExpressionImpl

foreign import asCallExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.CallExpression)

asNewExpression :: forall l r. Node l r -> Maybe Nodes.NewExpression
asNewExpression = toMaybe <<< runFn1 asNewExpressionImpl

foreign import asNewExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NewExpression)

asTaggedTemplateExpression :: forall l r. Node l r -> Maybe Nodes.TaggedTemplateExpression
asTaggedTemplateExpression = toMaybe <<< runFn1 asTaggedTemplateExpressionImpl

foreign import asTaggedTemplateExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TaggedTemplateExpression)

asTypeAssertionExpression :: forall l r. Node l r -> Maybe Nodes.TypeAssertion
asTypeAssertionExpression = toMaybe <<< runFn1 asTypeAssertionExpressionImpl

foreign import asTypeAssertionExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypeAssertion)

asParenthesizedExpression :: forall l r. Node l r -> Maybe Nodes.ParenthesizedExpression
asParenthesizedExpression = toMaybe <<< runFn1 asParenthesizedExpressionImpl

foreign import asParenthesizedExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ParenthesizedExpression)

asFunctionExpression :: forall l r. Node l r -> Maybe Nodes.FunctionExpression
asFunctionExpression = toMaybe <<< runFn1 asFunctionExpressionImpl

foreign import asFunctionExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.FunctionExpression)

asArrowFunction :: forall l r. Node l r -> Maybe Nodes.ArrowFunction
asArrowFunction = toMaybe <<< runFn1 asArrowFunctionImpl

foreign import asArrowFunctionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ArrowFunction)

asDeleteExpression :: forall l r. Node l r -> Maybe Nodes.DeleteExpression
asDeleteExpression = toMaybe <<< runFn1 asDeleteExpressionImpl

foreign import asDeleteExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.DeleteExpression)

asTypeOfExpression :: forall l r. Node l r -> Maybe Nodes.TypeOfExpression
asTypeOfExpression = toMaybe <<< runFn1 asTypeOfExpressionImpl

foreign import asTypeOfExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypeOfExpression)

asVoidExpression :: forall l r. Node l r -> Maybe Nodes.VoidExpression
asVoidExpression = toMaybe <<< runFn1 asVoidExpressionImpl

foreign import asVoidExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.VoidExpression)

asAwaitExpression :: forall l r. Node l r -> Maybe Nodes.AwaitExpression
asAwaitExpression = toMaybe <<< runFn1 asAwaitExpressionImpl

foreign import asAwaitExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AwaitExpression)

asPrefixUnaryExpression :: forall l r. Node l r -> Maybe Nodes.PrefixUnaryExpression
asPrefixUnaryExpression = toMaybe <<< runFn1 asPrefixUnaryExpressionImpl

foreign import asPrefixUnaryExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PrefixUnaryExpression)

asPostfixUnaryExpression :: forall l r. Node l r -> Maybe Nodes.PostfixUnaryExpression
asPostfixUnaryExpression = toMaybe <<< runFn1 asPostfixUnaryExpressionImpl

foreign import asPostfixUnaryExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PostfixUnaryExpression)

asBinaryExpression :: forall l r. Node l r -> Maybe Nodes.BinaryExpression
asBinaryExpression = toMaybe <<< runFn1 asBinaryExpressionImpl

foreign import asBinaryExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.BinaryExpression)

asConditionalExpression :: forall l r. Node l r -> Maybe Nodes.ConditionalExpression
asConditionalExpression = toMaybe <<< runFn1 asConditionalExpressionImpl

foreign import asConditionalExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ConditionalExpression)

asTemplateExpression :: forall l r. Node l r -> Maybe Nodes.TemplateExpression
asTemplateExpression = toMaybe <<< runFn1 asTemplateExpressionImpl

foreign import asTemplateExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TemplateExpression)

asYieldExpression :: forall l r. Node l r -> Maybe Nodes.YieldExpression
asYieldExpression = toMaybe <<< runFn1 asYieldExpressionImpl

foreign import asYieldExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.YieldExpression)

asSpreadElement :: forall l r. Node l r -> Maybe Nodes.SpreadElement
asSpreadElement = toMaybe <<< runFn1 asSpreadElementImpl

foreign import asSpreadElementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SpreadElement)

asClassExpression :: forall l r. Node l r -> Maybe Nodes.ClassExpression
asClassExpression = toMaybe <<< runFn1 asClassExpressionImpl

foreign import asClassExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ClassExpression)

asOmittedExpression :: forall l r. Node l r -> Maybe Nodes.OmittedExpression
asOmittedExpression = toMaybe <<< runFn1 asOmittedExpressionImpl

foreign import asOmittedExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.OmittedExpression)

asExpressionWithTypeArguments :: forall l r. Node l r -> Maybe Nodes.ExpressionWithTypeArguments
asExpressionWithTypeArguments = toMaybe <<< runFn1 asExpressionWithTypeArgumentsImpl

foreign import asExpressionWithTypeArgumentsImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ExpressionWithTypeArguments)

asAsExpression :: forall l r. Node l r -> Maybe Nodes.AsExpression
asAsExpression = toMaybe <<< runFn1 asAsExpressionImpl

foreign import asAsExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AsExpression)

asNonNullExpression :: forall l r. Node l r -> Maybe Nodes.NonNullExpression
asNonNullExpression = toMaybe <<< runFn1 asNonNullExpressionImpl

foreign import asNonNullExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NonNullExpression)

asMetaProperty :: forall l r. Node l r -> Maybe Nodes.MetaProperty
asMetaProperty = toMaybe <<< runFn1 asMetaPropertyImpl

foreign import asMetaPropertyImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.MetaProperty)

asSyntheticExpression :: forall l r. Node l r -> Maybe Nodes.SyntheticExpression
asSyntheticExpression = toMaybe <<< runFn1 asSyntheticExpressionImpl

foreign import asSyntheticExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SyntheticExpression)

asPartiallyEmittedExpression :: forall l r. Node l r -> Maybe Nodes.PartiallyEmittedExpression
asPartiallyEmittedExpression = toMaybe <<< runFn1 asPartiallyEmittedExpressionImpl

foreign import asPartiallyEmittedExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PartiallyEmittedExpression)

asCommaListExpression :: forall l r. Node l r -> Maybe Nodes.CommaListExpression
asCommaListExpression = toMaybe <<< runFn1 asCommaListExpressionImpl

foreign import asCommaListExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.CommaListExpression)

-- Misc

asTemplateSpan :: forall l r. Node l r -> Maybe Nodes.TemplateSpan
asTemplateSpan = toMaybe <<< runFn1 asTemplateSpanImpl

foreign import asTemplateSpanImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TemplateSpan)

asSemicolonClassElement :: forall l r. Node l r -> Maybe Nodes.SemicolonClassElement
asSemicolonClassElement = toMaybe <<< runFn1 asSemicolonClassElementImpl

foreign import asSemicolonClassElementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SemicolonClassElement)

-- Elements

asBlock :: forall l r. Node l r -> Maybe Nodes.Block
asBlock = toMaybe <<< runFn1 asBlockImpl

foreign import asBlockImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.Block)

asVariableStatement :: forall l r. Node l r -> Maybe Nodes.VariableStatement
asVariableStatement = toMaybe <<< runFn1 asVariableStatementImpl

foreign import asVariableStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.VariableStatement)

asEmptyStatement :: forall l r. Node l r -> Maybe Nodes.EmptyStatement
asEmptyStatement = toMaybe <<< runFn1 asEmptyStatementImpl

foreign import asEmptyStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.EmptyStatement)

asExpressionStatement :: forall l r. Node l r -> Maybe Nodes.ExpressionStatement
asExpressionStatement = toMaybe <<< runFn1 asExpressionStatementImpl

foreign import asExpressionStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ExpressionStatement)

asIfStatement :: forall l r. Node l r -> Maybe Nodes.IfStatement
asIfStatement = toMaybe <<< runFn1 asIfStatementImpl

foreign import asIfStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.IfStatement)

asDoStatement :: forall l r. Node l r -> Maybe Nodes.DoStatement
asDoStatement = toMaybe <<< runFn1 asDoStatementImpl

foreign import asDoStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.DoStatement)

asWhileStatement :: forall l r. Node l r -> Maybe Nodes.WhileStatement
asWhileStatement = toMaybe <<< runFn1 asWhileStatementImpl

foreign import asWhileStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.WhileStatement)

asForStatement :: forall l r. Node l r -> Maybe Nodes.ForStatement
asForStatement = toMaybe <<< runFn1 asForStatementImpl

foreign import asForStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ForStatement)

asForInStatement :: forall l r. Node l r -> Maybe Nodes.ForInStatement
asForInStatement = toMaybe <<< runFn1 asForInStatementImpl

foreign import asForInStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ForInStatement)

asForOfStatement :: forall l r. Node l r -> Maybe Nodes.ForOfStatement
asForOfStatement = toMaybe <<< runFn1 asForOfStatementImpl

foreign import asForOfStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ForOfStatement)

asContinueStatement :: forall l r. Node l r -> Maybe Nodes.ContinueStatement
asContinueStatement = toMaybe <<< runFn1 asContinueStatementImpl

foreign import asContinueStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ContinueStatement)

asBreakStatement :: forall l r. Node l r -> Maybe Nodes.BreakStatement
asBreakStatement = toMaybe <<< runFn1 asBreakStatementImpl

foreign import asBreakStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.BreakStatement)

asReturnStatement :: forall l r. Node l r -> Maybe Nodes.ReturnStatement
asReturnStatement = toMaybe <<< runFn1 asReturnStatementImpl

foreign import asReturnStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ReturnStatement)

asWithStatement :: forall l r. Node l r -> Maybe Nodes.WithStatement
asWithStatement = toMaybe <<< runFn1 asWithStatementImpl

foreign import asWithStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.WithStatement)

asSwitchStatement :: forall l r. Node l r -> Maybe Nodes.SwitchStatement
asSwitchStatement = toMaybe <<< runFn1 asSwitchStatementImpl

foreign import asSwitchStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SwitchStatement)

asLabeledStatement :: forall l r. Node l r -> Maybe Nodes.LabeledStatement
asLabeledStatement = toMaybe <<< runFn1 asLabeledStatementImpl

foreign import asLabeledStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.LabeledStatement)

asThrowStatement :: forall l r. Node l r -> Maybe Nodes.ThrowStatement
asThrowStatement = toMaybe <<< runFn1 asThrowStatementImpl

foreign import asThrowStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ThrowStatement)

asTryStatement :: forall l r. Node l r -> Maybe Nodes.TryStatement
asTryStatement = toMaybe <<< runFn1 asTryStatementImpl

foreign import asTryStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TryStatement)

asDebuggerStatement :: forall l r. Node l r -> Maybe Nodes.DebuggerStatement
asDebuggerStatement = toMaybe <<< runFn1 asDebuggerStatementImpl

foreign import asDebuggerStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.DebuggerStatement)

asVariableDeclaration :: forall l r. Node l r -> Maybe Nodes.VariableDeclaration
asVariableDeclaration = toMaybe <<< runFn1 asVariableDeclarationImpl

foreign import asVariableDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.VariableDeclaration)

asVariableDeclarationList :: forall l r. Node l r -> Maybe Nodes.VariableDeclarationList
asVariableDeclarationList = toMaybe <<< runFn1 asVariableDeclarationListImpl

foreign import asVariableDeclarationListImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.VariableDeclarationList)

asFunctionDeclaration :: forall l r. Node l r -> Maybe Nodes.FunctionDeclaration
asFunctionDeclaration = toMaybe <<< runFn1 asFunctionDeclarationImpl

foreign import asFunctionDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.FunctionDeclaration)

asClassDeclaration :: forall l r. Node l r -> Maybe Nodes.ClassLikeDeclaration
asClassDeclaration = toMaybe <<< runFn1 asClassDeclarationImpl

foreign import asClassDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ClassDeclaration)

asInterfaceDeclaration :: forall l r. Node l r -> Maybe Nodes.InterfaceDeclaration
asInterfaceDeclaration = toMaybe <<< runFn1 asInterfaceDeclarationImpl

foreign import asInterfaceDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.InterfaceDeclaration)

asTypeAliasDeclaration :: forall l r. Node l r -> Maybe Nodes.TypeAliasDeclaration
asTypeAliasDeclaration = toMaybe <<< runFn1 asTypeAliasDeclarationImpl

foreign import asTypeAliasDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.TypeAliasDeclaration)

asEnumDeclaration :: forall l r. Node l r -> Maybe Nodes.EnumDeclaration
asEnumDeclaration = toMaybe <<< runFn1 asEnumDeclarationImpl

foreign import asEnumDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.EnumDeclaration)

asModuleDeclaration :: forall l r. Node l r -> Maybe Nodes.ModuleDeclaration
asModuleDeclaration = toMaybe <<< runFn1 asModuleDeclarationImpl

foreign import asModuleDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ModuleDeclaration)

asModuleBlock :: forall l r. Node l r -> Maybe Nodes.ModuleBlock
asModuleBlock = toMaybe <<< runFn1 asModuleBlockImpl

foreign import asModuleBlockImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ModuleBlock)

asCaseBlock :: forall l r. Node l r -> Maybe Nodes.CaseBlock
asCaseBlock = toMaybe <<< runFn1 asCaseBlockImpl

foreign import asCaseBlockImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.CaseBlock)

asNamespaceExportDeclaration :: forall l r. Node l r -> Maybe Nodes.NamespaceExportDeclaration
asNamespaceExportDeclaration = toMaybe <<< runFn1 asNamespaceExportDeclarationImpl

foreign import asNamespaceExportDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NamespaceExportDeclaration)

asImportEqualsDeclaration :: forall l r. Node l r -> Maybe Nodes.ImportEqualsDeclaration
asImportEqualsDeclaration = toMaybe <<< runFn1 asImportEqualsDeclarationImpl

foreign import asImportEqualsDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ImportEqualsDeclaration)

asImportDeclaration :: forall l r. Node l r -> Maybe Nodes.ImportDeclaration
asImportDeclaration = toMaybe <<< runFn1 asImportDeclarationImpl

foreign import asImportDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ImportDeclaration)

asImportClause :: forall l r. Node l r -> Maybe Nodes.ImportClause
asImportClause = toMaybe <<< runFn1 asImportClauseImpl

foreign import asImportClauseImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ImportClause)

asAssertClause :: forall l r. Node l r -> Maybe Nodes.AssertClause
asAssertClause = toMaybe <<< runFn1 asAssertClauseImpl

foreign import asAssertClauseImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AssertClause)

asAssertEntry :: forall l r. Node l r -> Maybe Nodes.AssertEntry
asAssertEntry = toMaybe <<< runFn1 asAssertEntryImpl

foreign import asAssertEntryImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.AssertEntry)

asNamespaceImport :: forall l r. Node l r -> Maybe Nodes.NamespaceImport
asNamespaceImport = toMaybe <<< runFn1 asNamespaceImportImpl

foreign import asNamespaceImportImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NamespaceImport)

asNamespaceExport :: forall l r. Node l r -> Maybe Nodes.NamespaceExport
asNamespaceExport = toMaybe <<< runFn1 asNamespaceExportImpl

foreign import asNamespaceExportImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NamespaceExport)

asNamedImports :: forall l r. Node l r -> Maybe Nodes.NamedImports
asNamedImports = toMaybe <<< runFn1 asNamedImportsImpl

foreign import asNamedImportsImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NamedImports)

asImportSpecifier :: forall l r. Node l r -> Maybe Nodes.ImportSpecifier
asImportSpecifier = toMaybe <<< runFn1 asImportSpecifierImpl

foreign import asImportSpecifierImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ImportSpecifier)

asExportAssignment :: forall l r. Node l r -> Maybe Nodes.ExportAssignment
asExportAssignment = toMaybe <<< runFn1 asExportAssignmentImpl

foreign import asExportAssignmentImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ExportAssignment)

asExportDeclaration :: forall l r. Node l r -> Maybe Nodes.ExportDeclaration
asExportDeclaration = toMaybe <<< runFn1 asExportDeclarationImpl

foreign import asExportDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ExportDeclaration)

asNamedExports :: forall l r. Node l r -> Maybe Nodes.NamedExports
asNamedExports = toMaybe <<< runFn1 asNamedExportsImpl

foreign import asNamedExportsImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NamedExports)

asExportSpecifier :: forall l r. Node l r -> Maybe Nodes.ExportSpecifier
asExportSpecifier = toMaybe <<< runFn1 asExportSpecifierImpl

foreign import asExportSpecifierImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ExportSpecifier)

asMissingDeclaration :: forall l r. Node l r -> Maybe Nodes.MissingDeclaration
asMissingDeclaration = toMaybe <<< runFn1 asMissingDeclarationImpl

foreign import asMissingDeclarationImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.MissingDeclaration)

asNotEmittedStatement :: forall l r. Node l r -> Maybe Nodes.NotEmittedStatement
asNotEmittedStatement = toMaybe <<< runFn1 asNotEmittedStatementImpl

foreign import asNotEmittedStatementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.NotEmittedStatement)

-- /* @internal */
-- asSyntheticReference = toMaybe <<< runFn1 asSyntheticReferenceImpl
-- foreign import asSyntheticReferenceImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SyntheticReferenceExpression)
-- 
-- /* @internal */
-- asMergeDeclarationMarker = toMaybe <<< runFn1 asMergeDeclarationMarkerImpl
-- foreign import asMergeDeclarationMarkerImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.MergeDeclarationMarker)
-- 
-- /* @internal */
-- asEndOfDeclarationMarker = toMaybe <<< runFn1 asEndOfDeclarationMarkerImpl
-- foreign import asEndOfDeclarationMarkerImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.EndOfDeclarationMarker)

-- Module References

asExternalModuleReference :: forall l r. Node l r -> Maybe Nodes.ExternalModuleReference
asExternalModuleReference = toMaybe <<< runFn1 asExternalModuleReferenceImpl

foreign import asExternalModuleReferenceImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ExternalModuleReference)

-- JSX

asJsxElement :: forall l r. Node l r -> Maybe Nodes.JsxElement
asJsxElement = toMaybe <<< runFn1 asJsxElementImpl

foreign import asJsxElementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxElement)

asJsxSelfClosingElement :: forall l r. Node l r -> Maybe Nodes.JsxSelfClosingElement
asJsxSelfClosingElement = toMaybe <<< runFn1 asJsxSelfClosingElementImpl

foreign import asJsxSelfClosingElementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxSelfClosingElement)

asJsxOpeningElement :: forall l r. Node l r -> Maybe Nodes.JsxOpeningElement
asJsxOpeningElement = toMaybe <<< runFn1 asJsxOpeningElementImpl

foreign import asJsxOpeningElementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxOpeningElement)

asJsxClosingElement :: forall l r. Node l r -> Maybe Nodes.JsxClosingElement
asJsxClosingElement = toMaybe <<< runFn1 asJsxClosingElementImpl

foreign import asJsxClosingElementImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxClosingElement)

asJsxFragment :: forall l r. Node l r -> Maybe Nodes.JsxFragment
asJsxFragment = toMaybe <<< runFn1 asJsxFragmentImpl

foreign import asJsxFragmentImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxFragment)

asJsxOpeningFragment :: forall l r. Node l r -> Maybe Nodes.JsxOpeningFragment
asJsxOpeningFragment = toMaybe <<< runFn1 asJsxOpeningFragmentImpl

foreign import asJsxOpeningFragmentImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxOpeningFragment)

asJsxClosingFragment :: forall l r. Node l r -> Maybe Nodes.JsxClosingFragment
asJsxClosingFragment = toMaybe <<< runFn1 asJsxClosingFragmentImpl

foreign import asJsxClosingFragmentImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxClosingFragment)

asJsxAttribute :: forall l r. Node l r -> Maybe Nodes.JsxAttribute
asJsxAttribute = toMaybe <<< runFn1 asJsxAttributeImpl

foreign import asJsxAttributeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxAttribute)

asJsxAttributes :: forall l r. Node l r -> Maybe Nodes.JsxAttributes
asJsxAttributes = toMaybe <<< runFn1 asJsxAttributesImpl

foreign import asJsxAttributesImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxAttributes)

asJsxSpreadAttribute :: forall l r. Node l r -> Maybe Nodes.JsxSpreadAttribute
asJsxSpreadAttribute = toMaybe <<< runFn1 asJsxSpreadAttributeImpl

foreign import asJsxSpreadAttributeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxSpreadAttribute)

asJsxExpression :: forall l r. Node l r -> Maybe Nodes.JsxExpression
asJsxExpression = toMaybe <<< runFn1 asJsxExpressionImpl

foreign import asJsxExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JsxExpression)

-- Clauses

asCaseClause :: forall l r. Node l r -> Maybe Nodes.CaseClause
asCaseClause = toMaybe <<< runFn1 asCaseClauseImpl

foreign import asCaseClauseImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.CaseClause)

asDefaultClause :: forall l r. Node l r -> Maybe Nodes.DefaultClause
asDefaultClause = toMaybe <<< runFn1 asDefaultClauseImpl

foreign import asDefaultClauseImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.DefaultClause)

asHeritageClause :: forall l r. Node l r -> Maybe Nodes.HeritageClause
asHeritageClause = toMaybe <<< runFn1 asHeritageClauseImpl

foreign import asHeritageClauseImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.HeritageClause)

asCatchClause :: forall l r. Node l r -> Maybe Nodes.CatchClause
asCatchClause = toMaybe <<< runFn1 asCatchClauseImpl

foreign import asCatchClauseImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.CatchClause)

-- Property assignments

asPropertyAssignment :: forall l r. Node l r -> Maybe Nodes.PropertyAssignment
asPropertyAssignment = toMaybe <<< runFn1 asPropertyAssignmentImpl

foreign import asPropertyAssignmentImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.PropertyAssignment)

asShorthandPropertyAssignment :: forall l r. Node l r -> Maybe Nodes.ShorthandPropertyAssignment
asShorthandPropertyAssignment = toMaybe <<< runFn1 asShorthandPropertyAssignmentImpl

foreign import asShorthandPropertyAssignmentImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.ShorthandPropertyAssignment)

asSpreadAssignment :: forall l r. Node l r -> Maybe Nodes.SpreadAssignment
asSpreadAssignment = toMaybe <<< runFn1 asSpreadAssignmentImpl

foreign import asSpreadAssignmentImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SpreadAssignment)

-- Enum

asEnumMember :: forall l r. Node l r -> Maybe Nodes.EnumMember
asEnumMember = toMaybe <<< runFn1 asEnumMemberImpl

foreign import asEnumMemberImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.EnumMember)

-- Unparsed

-- TODO(rbuckton): isUnparsedPrologue

asUnparsedPrepend :: forall l r. Node l r -> Maybe Nodes.UnparsedPrepend
asUnparsedPrepend = toMaybe <<< runFn1 asUnparsedPrependImpl

foreign import asUnparsedPrependImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.UnparsedPrepend)

-- TODO(rbuckton): isUnparsedText
-- TODO(rbuckton): isUnparsedInternalText
-- TODO(rbuckton): isUnparsedSyntheticReference

-- Top-level nodes
asSourceFile :: forall l r. Node l r -> Maybe Nodes.SourceFile
asSourceFile = toMaybe <<< runFn1 asSourceFileImpl

foreign import asSourceFileImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.SourceFile)

asBundle :: forall l r. Node l r -> Maybe Nodes.Bundle
asBundle = toMaybe <<< runFn1 asBundleImpl

foreign import asBundleImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.Bundle)

asUnparsedSource :: forall l r. Node l r -> Maybe Nodes.UnparsedSource
asUnparsedSource = toMaybe <<< runFn1 asUnparsedSourceImpl

foreign import asUnparsedSourceImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.UnparsedSource)

-- TODO(rbuckton): isInputFiles

-- JSDoc Elements

asJSDocTypeExpression :: forall l r. Node l r -> Maybe Nodes.JSDocTypeExpression
asJSDocTypeExpression = toMaybe <<< runFn1 asJSDocTypeExpressionImpl

foreign import asJSDocTypeExpressionImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocTypeExpression)

asJSDocNameReference :: forall l r. Node l r -> Maybe Nodes.JSDocNameReference
asJSDocNameReference = toMaybe <<< runFn1 asJSDocNameReferenceImpl

foreign import asJSDocNameReferenceImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocNameReference)

asJSDocMemberName :: forall l r. Node l r -> Maybe Nodes.JSDocMemberName
asJSDocMemberName = toMaybe <<< runFn1 asJSDocMemberNameImpl

foreign import asJSDocMemberNameImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocMemberName)

asJSDocLink :: forall l r. Node l r -> Maybe Nodes.JSDocLink
asJSDocLink = toMaybe <<< runFn1 asJSDocLinkImpl

foreign import asJSDocLinkImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocLink)

asJSDocLinkCode :: forall l r. Node l r -> Maybe Nodes.JSDocLinkCode
asJSDocLinkCode = toMaybe <<< runFn1 asJSDocLinkCodeImpl

foreign import asJSDocLinkCodeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocLinkCode)

asJSDocLinkPlain :: forall l r. Node l r -> Maybe Nodes.JSDocLinkPlain
asJSDocLinkPlain = toMaybe <<< runFn1 asJSDocLinkPlainImpl

foreign import asJSDocLinkPlainImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocLinkPlain)

asJSDocAllType :: forall l r. Node l r -> Maybe Nodes.JSDocAllType
asJSDocAllType = toMaybe <<< runFn1 asJSDocAllTypeImpl

foreign import asJSDocAllTypeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocAllType)

asJSDocUnknownType :: forall l r. Node l r -> Maybe Nodes.JSDocUnknownType
asJSDocUnknownType = toMaybe <<< runFn1 asJSDocUnknownTypeImpl

foreign import asJSDocUnknownTypeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocUnknownType)

asJSDocNullableType :: forall l r. Node l r -> Maybe Nodes.JSDocNullableType
asJSDocNullableType = toMaybe <<< runFn1 asJSDocNullableTypeImpl

foreign import asJSDocNullableTypeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocNullableType)

asJSDocNonNullableType :: forall l r. Node l r -> Maybe Nodes.JSDocNonNullableType
asJSDocNonNullableType = toMaybe <<< runFn1 asJSDocNonNullableTypeImpl

foreign import asJSDocNonNullableTypeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocNonNullableType)

asJSDocOptionalType :: forall l r. Node l r -> Maybe Nodes.JSDocOptionalType
asJSDocOptionalType = toMaybe <<< runFn1 asJSDocOptionalTypeImpl

foreign import asJSDocOptionalTypeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocOptionalType)

asJSDocFunctionType :: forall l r. Node l r -> Maybe Nodes.JSDocFunctionType
asJSDocFunctionType = toMaybe <<< runFn1 asJSDocFunctionTypeImpl

foreign import asJSDocFunctionTypeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocFunctionType)

asJSDocVariadicType :: forall l r. Node l r -> Maybe Nodes.JSDocVariadicType
asJSDocVariadicType = toMaybe <<< runFn1 asJSDocVariadicTypeImpl

foreign import asJSDocVariadicTypeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocVariadicType)

asJSDocNamepathType :: forall l r. Node l r -> Maybe Nodes.JSDocNamepathType
asJSDocNamepathType = toMaybe <<< runFn1 asJSDocNamepathTypeImpl

foreign import asJSDocNamepathTypeImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocNamepathType)

asJSDoc :: forall l r. Node l r -> Maybe Nodes.JSDoc
asJSDoc = toMaybe <<< runFn1 asJSDocImpl

foreign import asJSDocImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDoc)

asJSDocTypeLiteral :: forall l r. Node l r -> Maybe Nodes.JSDocTypeLiteral
asJSDocTypeLiteral = toMaybe <<< runFn1 asJSDocTypeLiteralImpl

foreign import asJSDocTypeLiteralImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocTypeLiteral)

asJSDocSignature :: forall l r. Node l r -> Maybe Nodes.JSDocSignature
asJSDocSignature = toMaybe <<< runFn1 asJSDocSignatureImpl

foreign import asJSDocSignatureImpl :: forall l r. Fn1 (Node l r) (Nullable Nodes.JSDocSignature)

-- Our contrib

asSyntaxList :: forall l r. Node l r -> Maybe Nodes.SyntaxList
asSyntaxList = toMaybe <<< asSyntaxListImpl

foreign import asSyntaxListImpl :: forall l r. Node l r -> Nullable Nodes.SyntaxList

