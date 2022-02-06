module TypeScript.Compiler.Types.Symbol where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Int.Bits ((.&.))
import Prim.Row (class Cons) as Row
import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy)
import TypeScript.Compiler.Types (Symbol_)
import TypeScript.Compiler.Types.Nodes as Nodes

type SymbolFlags = Int

type SymbolFlagsRow = ("Optional" :: SymbolFlags, "ModuleMember" :: SymbolFlags)

foreign import symbolFlags :: { | SymbolFlagsRow }

interface
  :: Symbol_
  -> { flags :: SymbolFlags
     , name :: String
     , declarations :: Array Nodes.Declaration
     }
interface s =
  { flags: getFlags s
  , name: getName s
  , declarations: getDeclarations s
  }

getFlags :: Symbol_ -> SymbolFlags
getFlags = runFn1 getFlagsImpl

foreign import getFlagsImpl :: Fn1 Symbol_ Int

getName :: Symbol_ -> String
getName = runFn1 getNameImpl

foreign import getNameImpl :: Fn1 Symbol_ String

getDeclarations :: Symbol_ -> Array Nodes.Declaration
getDeclarations = runFn1 getDeclarationsImpl

foreign import getDeclarationsImpl :: Fn1 Symbol_ (Array Nodes.Declaration)

checkFlag
  :: forall s symbolFlags_
   . IsSymbol s
  => Row.Cons s SymbolFlags symbolFlags_ SymbolFlagsRow
  => Symbol_
  -> Proxy s
  -> Boolean
checkFlag s n = Record.get n symbolFlags .&. getFlags s /= 0
