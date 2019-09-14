module ReadDTS where

import Prelude

import Data.Lens (Lens, over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Profunctor (lcmap)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign (Foreign)
import Node.Path (FilePath)
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data TsDeclaration ∷ Type

foreign import data TsString ∷ Type
foreign import eqIdentifierImpl ∷ TsString → TsString → Boolean
instance eqIdentifier ∷ Eq TsString where
  eq = eqIdentifierImpl

unsafeTsStringToString ∷ TsString → String
unsafeTsStringToString = unsafeCoerce

newtype FullyQualifiedName = FullyQualifiedName String
derive instance eqFullyQualifiedName ∷ Eq FullyQualifiedName
derive instance ordFullyQualifiedName ∷ Ord FullyQualifiedName
derive newtype instance showFullyQualifiedName ∷ Show FullyQualifiedName

fqnToString ∷ FullyQualifiedName → String
fqnToString (FullyQualifiedName s) = s

type Property t =
  { name ∷ String
  , type ∷ t
  , optional ∷ Boolean
  }

-- | XXX: Is there a way to pass `Maybe` constructors to FFI
-- | but preserving typechecking on typescript side and drop this
-- | `nullable` parameter?
type OnDeclarationBase (nullable ∷ Type → Type) d t =
  { interface ∷
      { name ∷ String
      , fullyQualifiedName ∷ FullyQualifiedName
      , typeParameters ∷ Array t
      , properties ∷ Array (Property t)
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
-- | recursive types `ref` resolution
-- | should be treated with caution. You should guard
-- | against infinite declartion reference traversing.
-- |
-- | * Because this lib implements only part of
-- | the ts compiler API we are not able to promise that
-- | `ref` reference resolution will not end up with
-- | something like `unknown`.
type TypeReference d t =
  { typeArguments ∷ Array t
  , fullyQualifiedName ∷ FullyQualifiedName
  , ref ∷ TsDeclaration
  }

type OnTypeBase (nullable ∷ Type → Type) d t =
  { anonymousObject ∷ Array (Property t) → t
  , array ∷ t → t
  , intersection ∷ Array t → t
  , primitive ∷ String → t
  , tuple ∷ Array t → t
  , typeParameter ∷ { identifier ∷ TsString, default ∷ nullable t } → t
  , typeReference ∷ TypeReference d t → t
  , union ∷ Array t → t
  , unknown ∷ String → t
  }

type OnType d t = OnTypeBase Maybe d t
type OnDeclaration d t = OnDeclarationBase Maybe d t

type VisitBase nullable d t =
  { onTypeNode ∷ OnTypeBase nullable d t
  , onDeclaration ∷ OnDeclarationBase nullable d t
  }

type CompilerOptions = Foreign

type Visit d t = VisitBase Maybe d t

type Declarations d =
  { topLevel ∷ Array d
  , readDeclaration ∷ TsDeclaration → Effect d
  }

readDTS
  ∷ ∀ d t
  . CompilerOptions
  → Visit d t
  → FilePath
  → Effect (Declarations d)
readDTS opts visit = (runEffectFn3 _readDTS) opts visit'
  where
    visit'
      = over (_onDeclarationL <<< _unknownL) (lcmap  (over _fullyQualifiedNameL toMaybe))
      <<< over (_onTypeNodeL <<< _typeParameterL) (lcmap (over _defaultL toMaybe))
      $ visit

    -- | An example signature in case you want to turn these into polymorphic ones :-P
    _onTypeNodeL ∷ ∀ a b r. Lens { onTypeNode ∷ a | r } { onTypeNode ∷ b | r } a b
    _onTypeNodeL = prop (SProxy ∷ SProxy "onTypeNode")
    _onDeclarationL = prop (SProxy ∷ SProxy "onDeclaration")
    _typeParameterL = prop (SProxy ∷ SProxy "typeParameter")
    _typeReferenceL = prop (SProxy ∷ SProxy "typeReference")
    _fullyQualifiedNameL = prop (SProxy ∷ SProxy "fullyQualifiedName")
    _unknownL = prop (SProxy ∷ SProxy "unknown")
    _defaultL = prop (SProxy ∷ SProxy "default")

foreign import _readDTS
  ∷ ∀ d t
  . EffectFn3
      CompilerOptions
      (VisitBase Nullable d t)
      FilePath
      (Declarations d)

foreign import compilerOptions ∷ CompilerOptions
