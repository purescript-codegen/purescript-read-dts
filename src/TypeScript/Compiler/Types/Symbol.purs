module TypeScript.Compiler.Types.Symbol where

import Data.Function.Uncurried (Fn1, runFn1)
import TypeScript.Compiler.Types (Symbol_)
import TypeScript.Compiler.Types.Nodes as Nodes

type SymbolFlag = Int

type SymbolFlags = { "Optional" :: SymbolFlag }

foreign import symbolFlags :: SymbolFlags

getFlags :: Symbol_ -> SymbolFlag
getFlags = runFn1 getFlagsImpl

foreign import getFlagsImpl :: Fn1 Symbol_ Int

getName :: Symbol_ -> String
getName = runFn1 getNameImpl

foreign import getNameImpl :: Fn1 Symbol_ String

getDeclarations :: Symbol_ -> Array Nodes.Declaration
getDeclarations = runFn1 getDeclarationsImpl

foreign import getDeclarationsImpl :: Fn1 Symbol_ (Array Nodes.Declaration)
