module TypeScript.Compiler.Types.Typs.Internal where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import TypeScript.Compiler.Types (Typ)

-- | This type is not exposed by tsc
-- | but useful for debugging...
type IntrinsicType = Typ (intrinsicName :: String)

asIntrinsicType :: forall r. Typ r -> Maybe IntrinsicType
asIntrinsicType = toMaybe <<< runFn1 asIntrinsicTypeImpl

foreign import asIntrinsicTypeImpl :: forall r. Fn1 (Typ r) (Nullable IntrinsicType)

reflectBooleanLiteralType :: forall r. Typ r -> Maybe Boolean
reflectBooleanLiteralType = toMaybe <<< runFn1 reflectBooleanLiteralTypeImpl

foreign import reflectBooleanLiteralTypeImpl :: forall r. Fn1 (Typ r) (Nullable Boolean)

