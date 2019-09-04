module ReadDTS where

import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import Foreign (Foreign)
import Node.Path (FilePath)

type CompilerOptions = Foreign

type OnVisit a t = 
  { interface ∷ 
      { name ∷ String
      , members ∷ Array { name ∷ String, type ∷ t, optional ∷ Boolean }
      }
      → a
  , typeAlias ∷ { name ∷ String, type ∷ t } → a
  }

type OnType t = 
  { unionOrIntersection ∷ Array t → t
  , primitive ∷ String → t
  , stringLiteral ∷  String → t
  , numberLiteral ∷ Number → t
  , unknown ∷ String → t
  }

readDTS
  ∷ ∀ a t
  . FilePath
  → CompilerOptions
  → OnVisit a t
  → OnType t
  → Effect (Array a)
readDTS = runEffectFn4 _readDTS

foreign import _readDTS
  ∷ ∀ a t
  . EffectFn4
      FilePath
      CompilerOptions
      (OnVisit a t)
      (OnType t)
      (Array a)

foreign import compilerOptions ∷ CompilerOptions
