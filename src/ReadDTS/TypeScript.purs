module ReadDTS.TypeScript where

import Data.Function.Uncurried (Fn2, runFn2)
import TypeScript.Compiler.Types (Node, TypeChecker, TypeFlags)

isNodeExported :: forall l k. TypeChecker -> Node l k -> Boolean
isNodeExported = runFn2 isNodeExportedImpl

foreign import isNodeExportedImpl :: forall l k. Fn2 TypeChecker (Node l k) Boolean

foreign import showSyntaxKind :: forall l k. Node l k -> String

foreign import formatTypeFlags :: TypeFlags -> String


