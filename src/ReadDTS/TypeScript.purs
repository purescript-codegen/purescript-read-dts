module ReadDTS.TypeScript where

import Prelude

import Control.Alt ((<|>))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons, class Union) as Row
import TypeScript.Compiler.Checker (getFullyQualifiedName, getSymbolAtLocation)
import TypeScript.Compiler.Factory.NodeTests (asClassDeclaration, asFunctionDeclaration, asInterfaceDeclaration, asTypeAliasDeclaration)
import TypeScript.Compiler.Types (FullyQualifiedName, Node, Typ, TypeChecker, TypeFlags)
import TypeScript.Compiler.Types.Nodes (DeclarationStatement)
import TypeScript.Compiler.Types.Nodes (DeclarationStatement, interface) as Nodes
import TypeScript.Compiler.Types.Typs (interface) as Typs

isNodeExported :: forall l k. TypeChecker -> Node l k -> Boolean
isNodeExported = runFn2 isNodeExportedImpl

foreign import isNodeExportedImpl :: forall l k. Fn2 TypeChecker (Node l k) Boolean

foreign import showSyntaxKind :: forall l k. Node l k -> String

foreign import formatTypeFlags :: TypeFlags -> String

formatTypeFlags' :: forall i. Typ i -> String
formatTypeFlags' = formatTypeFlags <<< _.flags <<< Typs.interface

foreign import formatSyntaxKind :: forall l k. Node l k -> String

toDeclarationStatement ::
  forall i tag tagRow tagRow_.
  Row.Cons tag Void () tagRow =>
  Row.Union
    tagRow
    tagRow_
    ( "ClassDeclaration" :: Void
    , "ClassLikeDeclaration" :: Void
    , "FunctionDeclaration" :: Void
    , "InterfaceDeclaration" :: Void
    , "TypeAliasDeclaration" :: Void
    ) =>
  Node tag i ->
  Nodes.DeclarationStatement
toDeclarationStatement = toDeclarationStatementImpl

-- | FIXME: don't export
foreign import toDeclarationStatementImpl :: forall l r. Node l r -> Nodes.DeclarationStatement

asDeclarationStatement :: forall l r. Node l r -> Maybe DeclarationStatement
asDeclarationStatement node =
  (toDeclarationStatement <$> asTypeAliasDeclaration node)
  <|> (toDeclarationStatement <$> asInterfaceDeclaration node)
  <|> (toDeclarationStatement <$> asClassDeclaration node)
  <|> (toDeclarationStatement  <$> asFunctionDeclaration node)

getDeclarationStatementFqn :: TypeChecker -> DeclarationStatement -> FullyQualifiedName
getDeclarationStatementFqn checker node = unsafePartial $ fromJust do
  symbol <-
    (getSymbolAtLocation checker <<< _.name <<< Nodes.interface =<< asTypeAliasDeclaration node)
    <|> (getSymbolAtLocation checker =<< asInterfaceDeclaration node)
    <|> (getSymbolAtLocation checker =<< asClassDeclaration node)
    <|> (getSymbolAtLocation checker =<< asFunctionDeclaration node)
  pure $ getFullyQualifiedName checker symbol


