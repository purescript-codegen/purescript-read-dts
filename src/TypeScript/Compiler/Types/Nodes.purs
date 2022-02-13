module TypeScript.Compiler.Types.Nodes where

import Data.Undefined.NoProblem (Opt)
import Type.Row (type (+))
import TypeScript.Compiler.Types (Node, ScriptTarget, Typ)
import Unsafe.Coerce (unsafeCoerce)

-- | We can debug print these values using
-- | `TypeScript.Compiler.Debug.formatSyntaxKind`
-- | and `TypeScript.Compiler.Debug.formatNodeFlags`
-- |
-- | We should rather avoid casting on the PS based on flags.
-- | Casting functions are provided by ts compiler itself and we
-- | bind to them (please check `TypeScript.Compiler.Factory.NodeTests`.
foreign import data SyntaxKind :: Type
foreign import data NodeFlags :: Type

type NodeRow r = (nodeFlags :: NodeFlags, syntaxKind :: SyntaxKind | r)

interface :: forall l r. Node l r -> { | NodeRow + r }
interface = unsafeCoerce

foreign import getChildren :: forall l k. Node l k -> Array (Node "" ())

-- | Types and stubs which are required
-- | by `Compiler.Factory.NodeTests`.
-- |
-- | A lot of them are still not correctly filled
-- | and provide only the `NodeRow` now.
type AbstractKeyword = Node "AbstractKeyword" ()
type ArrayBindingPattern = Node "ArrayBindingPattern" ()
type ArrayLiteralExpression = Node "ArrayLiteralExpression" ()
type ArrayTypeNode = Node "ArrayTypeNode" ()
type ArrowFunction = Node "ArrowFunction" ()
type AsExpression = Node "AsExpression" ()
type AssertClause = Node "AssertClause" ()
type AssertEntry = Node "AssertEntry" ()
type AssertsKeyword = Node "AssertsKeyword" ()
type AsteriskToken = Node "AsteriskToken" ()
type AsyncKeyword = Node "AsyncKeyword" ()
type AwaitExpression = Node "AwaitExpression" ()
type AwaitKeyword = Node "AwaitKeyword" ()
type BigIntLiteral = Node "BigIntLiteral" ()
type BinaryExpression = Node "BinaryExpression" ()
type BindingElement = Node "BindingElement" ()
type Block = Node "Block" ()
type BreakStatement = Node "BreakStatement" ()
type Bundle = Node "Bundle" ()
type CallExpression = Node "CallExpression" ()
type CallSignatureDeclaration = Node "CallSignatureDeclaration" ()
type CaseBlock = Node "CaseBlock" ()
type CaseClause = Node "CaseClause" ()
type CatchClause = Node "CatchClause" ()
-- | This approximation is probably enough
-- | given `Compalier.Factory.asClassExpression`
-- | and `Compalier.Factory.asClassDeclaration`
type ClassDeclaration = ClassLikeDeclaration
type ClassElement = Node "ClassElement" (name :: PropertyName | ())
type ClassExpression = ClassLikeDeclaration
type ClassLikeDeclaration = Node "ClassLikeDeclaration"
  ( name :: Opt Identifier
  , typeParameters :: Opt (Array TypeParameterDeclaration)
  -- , heritageClauses?: NodeArray<HeritageClause>;
  , members :: Array ClassElement
  )

type ClassStaticBlockDeclaration = Node "ClassStaticBlockDeclaration" ()
type Clauses = Node "Clauses" ()
type ColonToken = Node "ColonToken" ()
type CommaListExpression = Node "CommaListExpression" ()
type ComputedPropertyName = Node "ComputedPropertyName" ()
type ConditionalExpression = Node "ConditionalExpression" ()
type ConditionalTypeNode = Node "ConditionalTypeNode" ()
type ConstructSignatureDeclaration = Node "ConstructSignatureDeclaration" ()
type ConstructorDeclaration = Node "ConstructorDeclaration" ()
type ConstructorTypeNode = Node "ConstructorTypeNode" ()
type ContinueStatement = Node "ContinueStatement" ()
type DebuggerStatement = Node "DebuggerStatement" ()
type Declaration = Node "Declaration" ()
type DeclarationStatement = Node "DeclarationStatement" ()
type Decorator = Node "Decorator" ()
type DefaultClause = Node "DefaultClause" ()
type DeleteExpression = Node "DeleteExpression" ()
type DoStatement = Node "DoStatement" ()
type DotDotDotToken = Node "DotDotDotToken" ()
type ElementAccessExpression = Node "ElementAccessExpression" ()
type Elements = Node "Elements" ()
type EmptyStatement = Node "EmptyStatement" ()
type EndOfDeclarationMarker = Node "EndOfDeclarationMarker" ()
type Enum = Node "Enum" ()
type EnumDeclaration = Node "EnumDeclaration" ()
type EnumMember = Node "EnumMember" ()
type EqualsGreaterThanToken = Node "EqualsGreaterThanToken" ()
type ExclamationToken = Node "ExclamationToken" ()
type ExportAssignment = Node "ExportAssignment" ()
type ExportDeclaration = Node "ExportDeclaration"
  ( parent :: Node "" () -- SourceFile | ModuleBlock;
  -- , exportClause ∷ NamedExports;
  -- , moduleSpecifier?: Expression;
  )

type ExportKeyword = Node "ExportKeyword" ()
type ExportSpecifier = Node "ExportSpecifier" ()
type Expression = Node "Expression" ()
type ExpressionStatement = Node "ExpressionStatement" ()
type ExpressionWithTypeArguments = Node "ExpressionWithTypeArguments" ()
type ExternalModuleReference = Node "ExternalModuleReference" ()
type ForInStatement = Node "ForInStatement" ()
type ForOfStatement = Node "ForOfStatement" ()
type ForStatement = Node "ForStatement" ()

-- | There is more to FunctionDeclaration
--  export interface FunctionLikeDeclarationBase extends SignatureDeclarationBase {
--      readonly asteriskToken?: AsteriskToken;
--      readonly questionToken?: QuestionToken;
--      readonly exclamationToken?: ExclamationToken;
--      readonly body?: Block | Expression;
--  }
type FunctionDeclaration = Node "FunctionDeclaration" (name :: Opt Identifier)
type FunctionExpression = Node "FunctionExpression" ()
type FunctionTypeNode = Node "FunctionTypeNode" ()
type GetAccessorDeclaration = Node "GetAccessorDeclaration" ()
type HeritageClause = Node "HeritageClause" ()
type Identifier = Node "Identifier" ()
type Identifiers = Node "Identifiers" ()
type IfStatement = Node "IfStatement" ()
type ImportClause = Node "ImportClause" ()
type ImportDeclaration = Node "ImportDeclaration" ()
type ImportEqualsDeclaration = Node "ImportEqualsDeclaration" ()
type ImportExpression = Node "ImportExpression" ()
type ImportSpecifier = Node "ImportSpecifier" ()
type ImportTypeNode = Node "ImportTypeNode" ()
type IndexSignatureDeclaration = Node "IndexSignatureDeclaration" ()
type IndexedAccessTypeNode = Node "IndexedAccessTypeNode" ()
type InterfaceDeclaration = Node "InterfaceDeclaration"
  ( name :: Identifier
  , typeParameters :: Opt (Array TypeParameterDeclaration)
  -- , heritageClauses?: NodeArray<HeritageClause>;
  , members :: Array TypeElement
  )

type InferTypeNode = Node "InferTypeNode" ()
type IntersectionTypeNode = Node "IntersectionTypeNode" ()
type JSDoc = Node "JSDoc" ()
type JSDocAllType = Node "JSDocAllType" ()
type JSDocFunctionType = Node "JSDocFunctionType" ()
type JSDocLink = Node "JSDocLink" ()
type JSDocLinkCode = Node "JSDocLinkCode" ()
type JSDocLinkPlain = Node "JSDocLinkPlain" ()
type JSDocMemberName = Node "JSDocMemberName" ()
type JSDocNameReference = Node "JSDocNameReference" ()
type JSDocNamepathType = Node "JSDocNamepathType" ()
type JSDocNonNullableType = Node "JSDocNonNullableType" ()
type JSDocNullableType = Node "JSDocNullableType" ()
type JSDocOptionalType = Node "JSDocOptionalType" ()
type JSDocSignature = Node "JSDocSignature" ()
type JSDocTypeExpression = Node "JSDocTypeExpression" ()
type JSDocTypeLiteral = Node "JSDocTypeLiteral" ()
type JSDocUnknownType = Node "JSDocUnknownType" ()
type JSDocVariadicType = Node "JSDocVariadicType" ()
type JSX = Node "JSX" ()
type JsxAttribute = Node "JsxAttribute" ()
type JsxAttributes = Node "JsxAttributes" ()
type JsxClosingElement = Node "JsxClosingElement" ()
type JsxClosingFragment = Node "JsxClosingFragment" ()
type JsxElement = Node "JsxElement" ()
type JsxExpression = Node "JsxExpression" ()
type JsxFragment = Node "JsxFragment" ()
type JsxOpeningElement = Node "JsxOpeningElement" ()
type JsxOpeningFragment = Node "JsxOpeningFragment" ()
type JsxSelfClosingElement = Node "JsxSelfClosingElement" ()
type JsxSpreadAttribute = Node "JsxSpreadAttribute" ()
type JsxText = Node "JsxText" ()
type LabeledStatement = Node "LabeledStatement" ()
type LiteralTypeNode = Node "LiteralTypeNode" ()
type MappedTypeNode = Node "MappedTypeNode" ()
type MergeDeclarationMarker = Node "MergeDeclarationMarker" ()
type MetaProperty = Node "MetaProperty" ()
type MethodDeclaration = Node "MethodDeclaration" ()
type MethodSignature = Node "MethodSignature" ()
type MinusToken = Node "MinusToken" ()
type Misc = Node "Misc" ()
type MissingDeclaration = Node "MissingDeclaration" ()
type ModuleBlock = Node "ModuleBlock" ()
type ModuleDeclaration = Node "ModuleDeclaration" ()
type NamedExports = Node "NamedExports" ()
type NamedImports = Node "NamedImports" ()
type NamedTupleMember = Node "NamedTupleMember" ()
type Names = Node "Names" ()
type NamespaceExport = Node "NamespaceExport" ()
type NamespaceExportDeclaration = Node "NamespaceExportDeclaration" ()
type NamespaceImport = Node "NamespaceImport" ()
type NewExpression = Node "NewExpression" ()
type NoSubstitutionTemplateLiteral = Node "NoSubstitutionTemplateLiteral" ()
type NonNullExpression = Node "NonNullExpression" ()
type NotEmittedStatement = Node "NotEmittedStatement" ()
type NumericLiteral = Node "NumericLiteral" ()
type ObjectBindingPattern = Node "ObjectBindingPattern" ()
type ObjectLiteralExpression = Node "ObjectLiteralExpression" ()
type OmittedExpression = Node "OmittedExpression" ()
type OptionalTypeNode = Node "OptionalTypeNode" ()
-- type BindingName = Identifier | BindingPattern;
type ParameterDeclaration = Node "ParameterDeclaration"
  ( name :: Node "BindingName" ()
  , questionToken :: Opt QuestionToken
  , "type" :: Opt TypeNode
  , initializer :: Opt Expression
  )

type ParenthesizedExpression = Node "ParenthesizedExpression" ()
type ParenthesizedTypeNode = Node "ParenthesizedTypeNode" ()
type PartiallyEmittedExpression = Node "PartiallyEmittedExpression" ()
type PropertyName = Node "PropertyName" ()
type PlusToken = Node "PlusToken" ()
type PostfixUnaryExpression = Node "PostfixUnaryExpression" ()
type PrefixUnaryExpression = Node "PrefixUnaryExpression" ()
type PrivateIdentifier = Node "PrivateIdentifier" ()
type PropertyAccessExpression = Node "PropertyAccessExpression" ()
type PropertyAssignment = Node "PropertyAssignment" ()
type PropertyDeclaration = Node "PropertyDeclaration" ()
type PropertySignature = Node "PropertySignature" ()
type Punctuation = Node "Punctuation" ()
type QualifiedName = Node "QualifiedName" ()
type QuestionDotToken = Node "QuestionDotToken" ()
type QuestionToken = Node "QuestionToken" ()
type ReadonlyKeyword = Node "ReadonlyKeyword" ()
type References = Node "References" ()
type RegularExpressionLiteral = Node "RegularExpressionLiteral" ()
type RestTypeNode = Node "RestTypeNode" ()
type ReturnStatement = Node "ReturnStatement" ()
type SemicolonClassElement = Node "SemicolonClassElement" ()
type SetAccessorDeclaration = Node "SetAccessorDeclaration" ()
type ShorthandPropertyAssignment = Node "ShorthandPropertyAssignment" ()
type SourceFile = Node "SourceFile"
  ( isDeclarationFile :: Boolean
  , fileName :: String
  , moduleName :: Opt String
  , statements :: Array Statement
  , text :: String
  , languageVersion :: ScriptTarget
  )

type SpreadAssignment = Node "SpreadAssignment" ()
type SpreadElement = Node "SpreadElement" ()
type Statement = Node "Statement" ()
type StaticKeyword = Node "StaticKeyword" ()
type StringLiteral = Node "StringLiteral" ()
type SuperExpression = Node "SuperExpression" ()
type SwitchStatement = Node "SwitchStatement" ()
type SyntheticExpression = Node "SyntheticExpression" ()
type SyntaxList = Node "SyntaxList" (_children :: Array (Node "" ()))
type SyntheticReferenceExpression = Node "SyntheticReferenceExpression" ()
type TaggedTemplateExpression = Node "TaggedTemplateExpression" ()
type TemplateExpression = Node "TemplateExpression" ()
type TemplateHead = Node "TemplateHead" ()
type TemplateLiteralTypeNode = Node "TemplateLiteralTypeNode" ()
type TemplateLiteralTypeSpan = Node "TemplateLiteralTypeSpan" ()
type TemplateMiddle = Node "TemplateMiddle" ()
type TemplateSpan = Node "TemplateSpan" ()
type TemplateTail = Node "TemplateTail" ()
type ThisTypeNode = Node "ThisTypeNode" ()
type ThrowStatement = Node "ThrowStatement" ()
type TokenNode = Node "TokenNode" ()
type TryStatement = Node "TryStatement" ()
type TupleTypeNode = Node "TupleTypeNode" ()
type TypeAliasDeclaration = Node "TypeAliasDeclaration"
  ( name :: Identifier
  , typeParameters :: Opt (Array TypeParameterDeclaration)
  , "type" :: Typ ()
  )

type TypeAssertion = Node "TypeAssertion" ()
type TypeElement = Node "TypeElement"
  ( name :: PropertyName
  , questionToken :: Opt TokenNode
  )

type TypeLiteralNode = Node "TypeLiteralNode" ()
type TypeMember = Node "TypeMember" ()
type TypeNode = Node "TypeNode" ()
type TypeOfExpression = Node "TypeOfExpression" ()
type TypeOperatorNode = Node "TypeOperatorNode" ()
type TypeParameterDeclaration = Node "TypeParameterDeclaration"
  ( name :: Identifier
  , constraint :: Opt TypeNode
  , default :: Opt TypeNode
  )

type TypePredicateNode = Node "TypePredicateNode" ()
type TypeQueryNode = Node "TypeQueryNode" ()
type TypeReferenceNode = Node "TypeReferenceNode" ()

-- | Not a real node kind but rather a union
-- | export type SignatureDeclaration =
-- |   | CallSignatureDeclaration | ConstructSignatureDeclaration
-- |   | MethodSignature | IndexSignatureDeclaration
-- |   | FunctionTypeNode | ConstructorTypeNode
-- |   | JSDocFunctionType | FunctionDeclaration
-- |   | MethodDeclaration | ConstructorDeclaration
-- |   | AccessorDeclaration | FunctionExpression
-- |   | ArrowFunction
type SignatureDeclaration = Node "SignatureDeclaration" ()
type UnionTypeNode = Node "UnionTypeNode" ()
type Unparsed = Node "Unparsed" ()
type UnparsedPrepend = Node "UnparsedPrepend" ()
type UnparsedSource = Node "UnparsedSource" ()
type VariableDeclaration = Node "VariableDeclaration" ()
type VariableDeclarationList = Node "VariableDeclarationList" ()
type VariableStatement = Node "VariableStatement" ()
type VoidExpression = Node "VoidExpression" ()
type WhileStatement = Node "WhileStatement" ()
type WithStatement = Node "WithStatement" ()
type Words = Node "Words" ()
type YieldExpression = Node "YieldExpression" ()
