module ReadDTS where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn3, EffectFn4, runEffectFn3, runEffectFn4)
import Foreign (Foreign)
import Node.Path (FilePath)

-- |
foreign import data TsString ∷ Type
foreign import eqIdentifierImpl ∷ TsString → TsString → Boolean
instance eqIdentifier ∷ Eq TsString where
  eq = eqIdentifierImpl

type OnDeclaration t =
  { interface ∷
      { name ∷ String
      , fullyQualifiedName ∷ String
      , typeParameters ∷ Array t
      , members ∷ Array
          { name ∷ String
          , type ∷ t
          , optional ∷ Boolean
          }
      }
      → Effect Unit
  , typeAlias ∷ { name ∷ String, "type" ∷ t } → Effect Unit
  }

type InterfaceReference t =
 { typeArguments ∷ Array t, fullyQualifiedName ∷ String, read ∷ Effect Unit }

type OnType t =
  { intersection ∷ Array t → t
  , interfaceReference ∷ InterfaceReference t → t
  , numberLiteral ∷ Number → t
  , primitive ∷ String → t
  , stringLiteral ∷ String → t
  , typeParameter ∷ TsString → t
  , union ∷ Array t → t
  , unknown ∷ String → t
  }

type Visit t =
  { onType ∷ OnType t
  , onDeclaration ∷ OnDeclaration t
  }

type CompilerOptions = Foreign

readDTS
  ∷ ∀ t
  . CompilerOptions
  → Visit t
  → FilePath
  → Effect Unit
readDTS = runEffectFn3 _readDTS

foreign import _readDTS
  ∷ ∀ t
  . EffectFn3
      CompilerOptions
      (Visit t)
      FilePath
      Unit

foreign import compilerOptions ∷ CompilerOptions
