module ReadDTS.TypeScript where

import Data.Function.Uncurried (Fn2, runFn2)
import TypeScript.Compiler.Types (Node, TypeChecker)

isNodeExported :: forall k. TypeChecker -> Node k -> Boolean
isNodeExported = runFn2 isNodeExportedImpl

foreign import isNodeExportedImpl :: forall k. Fn2 TypeChecker (Node k) Boolean

foreign import showSyntaxKind :: forall k. Node k -> String

