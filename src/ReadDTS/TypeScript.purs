module ReadDTS.TypeScript where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Prim.Row (class Cons, class Union) as Row
import TypeScript.Compiler.Types (Node, TypeChecker, TypeFlags)
import TypeScript.Compiler.Types.Nodes (DeclarationStatement) as Nodes

isNodeExported :: forall l k. TypeChecker -> Node l k -> Boolean
isNodeExported = runFn2 isNodeExportedImpl

foreign import isNodeExportedImpl :: forall l k. Fn2 TypeChecker (Node l k) Boolean

foreign import showSyntaxKind :: forall l k. Node l k -> String

foreign import formatTypeFlags :: TypeFlags -> String

toDeclarationStatement ::
  forall i tag tagRow tagRow_.
  Row.Cons tag Void () tagRow =>
  Row.Union
    tagRow
    tagRow_
    ( "ClassDeclaration" :: Void
    , "ClassLikeDeclaration" :: Void
    , "InterfaceDeclaration" :: Void
    , "TypeAliasDeclaration" :: Void
    ) =>
  Node tag i ->
  Nodes.DeclarationStatement
toDeclarationStatement = toDeclarationStatementImpl

-- | Internal - should be private
foreign import toDeclarationStatementImpl :: forall l r. Node l r -> Nodes.DeclarationStatement

