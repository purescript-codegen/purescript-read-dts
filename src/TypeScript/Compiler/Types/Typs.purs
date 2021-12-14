module TypeScript.Compiler.Types.Typs where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Undefined.NoProblem (Opt)
import TypeScript.Compiler.Types (Typ)
import Unsafe.Coerce (unsafeCoerce)

interface :: forall i. Typ i -> { | i }
interface = unsafeCoerce

type BooleanLiteralType = Typ (value :: Boolean)

type GenericTypeRow r = InterfaceTypeRow r

type InterfaceTypeRow r =
  ( typeParameters :: Opt (Array TypeParameter)
  , outerTypeParameters :: Opt (Array TypeParameter)
  , localTypeParameters :: Opt (Array TypeParameter)
  , thisType :: Opt TypeParameter
  | r
  )

type NumberLiteralType = Typ (value :: Number)

type StringLiteralType = Typ (value :: String)

type TypeParameter = Typ ()

type TypeReference = Typ
  ( target :: Typ (GenericTypeRow ())
  , typeArguments :: Opt (Array (Typ ()))
  )

asNumberLiteralType :: forall r. Typ r -> Maybe NumberLiteralType
asNumberLiteralType = toMaybe <<< runFn1 asNumberLiteralTypeImpl

foreign import asNumberLiteralTypeImpl :: forall r. Fn1 (Typ r) (Nullable NumberLiteralType)

-- | You can find test driven heuristic against `ObjectFlags` on the `ts` side.
-- | Touching internal properites here.
type ObjectType = Typ (properties :: Array Symbol) -- (objectFlags :: ObjectFlag, properties :: Array Symbol)

asObjectType :: forall r. Typ r -> Maybe ObjectType
asObjectType = toMaybe <<< runFn1 asObjectTypeImpl

foreign import asObjectTypeImpl :: forall r. Fn1 (Typ r) (Nullable ObjectType)

asStringLiteralType :: forall r. Typ r -> Maybe StringLiteralType
asStringLiteralType = toMaybe <<< runFn1 asStringLiteralTypeImpl

foreign import asStringLiteralTypeImpl :: forall r. Fn1 (Typ r) (Nullable StringLiteralType)

asTypeReference :: forall r. Typ r -> Maybe TypeReference
asTypeReference = toMaybe <<< runFn1 asTypeReferenceImpl

foreign import asTypeReferenceImpl :: forall r. Fn1 (Typ r) (Nullable TypeReference)

