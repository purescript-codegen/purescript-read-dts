module TypeScript.Compiler.Types.Symbol where

import Data.Function.Uncurried (Fn1, runFn1)
import TypeScript.Compiler.Types.Nodes as Nodes

type SymbolFlag = Int

type SymbolFlags = { "Optional" :: SymbolFlag }

foreign import symbolFlags :: SymbolFlags

getFlags :: Symbol -> SymbolFlag
getFlags = runFn1 getFlagsImpl

foreign import getFlagsImpl :: Fn1 Symbol Int

getName :: Symbol -> String
getName = runFn1 getNameImpl

foreign import getNameImpl :: Fn1 Symbol String

getDeclarations :: Symbol -> Array Nodes.Declaration
getDeclarations = runFn1 getDeclarationsImpl

foreign import getDeclarationsImpl :: Fn1 Symbol (Array Nodes.Declaration)
