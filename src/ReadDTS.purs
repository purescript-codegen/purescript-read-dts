module ReadDTS where

import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign)
import Node.Path (FilePath)

type CompilerOptions = Foreign

type OnType t =
  { interface ∷
      { name ∷ String
      , members ∷ Array { name ∷ String, type ∷ t, optional ∷ Boolean }
      }
      → t
  , typeAlias ∷ { name ∷ String, type ∷ t } → t
  , unionOrIntersection ∷ Array t → t
  , primitive ∷ String → t
  , stringLiteral ∷  String → t
  , numberLiteral ∷ Number → t
  , unknown ∷ String → t
  }

readDTS
  ∷ ∀ t
  . FilePath
  → CompilerOptions
  → OnType t
  → Effect (Array t)
readDTS = runEffectFn3 _readDTS

foreign import _readDTS
  ∷ ∀ t
  . EffectFn3
      FilePath
      CompilerOptions
      (OnType t)
      (Array t)

foreign import compilerOptions ∷ CompilerOptions
