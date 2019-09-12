module ReadDTS where

import Prelude

import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Profunctor (lcmap)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign)
import Node.Path (FilePath)
import Type.Prelude (SProxy(..))

foreign import data TsString ∷ Type
foreign import eqIdentifierImpl ∷ TsString → TsString → Boolean
instance eqIdentifier ∷ Eq TsString where
  eq = eqIdentifierImpl

newtype FullyQualifiedName = FullyQualifiedName String
derive instance eqFullyQualifiedName ∷ Eq FullyQualifiedName
derive instance ordFullyQualifiedName ∷ Ord FullyQualifiedName
derive newtype instance showFullyQualifiedName ∷ Show FullyQualifiedName

-- | XXX: Is there a way to pass `Maybe` constructors to FFI
-- | but preserving typechecking on typescript side and drop this
-- | `nullable` parameter?
type OnDeclarationBase (nullable ∷ Type → Type) d t =
  { interface ∷
      { name ∷ String
      , fullyQualifiedName ∷ FullyQualifiedName
      , typeParameters ∷ Array t
      , members ∷ Array
          { name ∷ String
          , type ∷ t
          , optional ∷ Boolean
          }
      }
      → d
  , typeAlias ∷
      { name ∷ String
      , typeParameters ∷ Array t
      , "type" ∷ t
      }
      → d
  , unknown ∷
      { fullyQualifiedName ∷ nullable FullyQualifiedName
      , msg ∷ String
      }
      → d
  }

-- | * As typescript allows us to define
-- | recursive types `read` should be
-- | treated with caution. You should guard
-- | against infinite reference traversing.
-- |
-- | * Because this lib implements only part of
-- | the ts compiler API we are not able to promise that
-- | `read` reference resolution will not end up with
-- | something like `unknown`.
type TypeReference d t =
  { typeArguments ∷ Array t
  , fullyQualifiedName ∷ FullyQualifiedName
  , read ∷ Effect d
  }

type OnTypeBase (nullable ∷ Type → Type) d t =
  { intersection ∷ Array t → t
  , numberLiteral ∷ Number → t
  , primitive ∷ String → t
  , stringLiteral ∷ String → t
  , typeParameter ∷ { identifier ∷ TsString, default ∷ nullable t } → t
  , typeReference ∷ TypeReference d t → t
  , union ∷ Array t → t
  , unknown ∷ String → t
  }

type OnType d t = OnTypeBase Maybe d t
type OnDeclaration d t = OnDeclarationBase Maybe d t

type VisitBase nullable d t =
  { onType ∷ OnTypeBase nullable d t
  , onDeclaration ∷ OnDeclarationBase nullable d t
  }

type CompilerOptions = Foreign

type Visit d t = VisitBase Maybe d t

readDTS
  ∷ ∀ d t
  . CompilerOptions
  → Visit d t
  → FilePath
  → Effect (Array d)
readDTS opts visit = (runEffectFn3 _readDTS) opts visit'
  where
    visit'
      = over (_onDeclarationL <<< _unknownL) (lcmap  (over _fullyQualifiedNameL toMaybe))
      <<< over (_onTypeL <<< _typeParameterL) (lcmap (over _defaultL toMaybe))
      $ visit

    _onDeclarationL = prop (SProxy ∷ SProxy "onDeclaration")
    _onTypeL = prop (SProxy ∷ SProxy "onType")
    _typeParameterL = prop (SProxy ∷ SProxy "typeParameter")
    _fullyQualifiedNameL = prop (SProxy ∷ SProxy "fullyQualifiedName")
    _unknownL = prop (SProxy ∷ SProxy "unknown")
    _defaultL = prop (SProxy ∷ SProxy "default")


foreign import _readDTS
  ∷ ∀ d t
  . EffectFn3
      CompilerOptions
      (VisitBase Nullable d t)
      FilePath
      (Array d)

foreign import compilerOptions ∷ CompilerOptions
