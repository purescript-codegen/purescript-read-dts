module ReadDTS.TypeScript.Compiler.Checker where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import TypeScript.Compiler.Types.Typs (TypeReference)
import TypeScript.Compiler.Types (TypeChecker, Typ)

-- | We need to expose some extra (internal!) pieces of the type checker
-- | to easily process some builtin types etc.
-- | Should this be added to the more "official" root `TypeScript` bindings?
getElementTypeOfArrayType :: TypeChecker -> TypeReference -> Effect (Maybe (Typ ()))
getElementTypeOfArrayType c r = toMaybe <$> runEffectFn2 getElementTypeOfArrayTypeImpl c r

foreign import getElementTypeOfArrayTypeImpl :: EffectFn2 TypeChecker TypeReference (Nullable (Typ ()))

isArrayType :: TypeChecker -> TypeReference -> Effect Boolean
isArrayType = runEffectFn2 isArrayTypeImpl

foreign import isArrayTypeImpl :: EffectFn2 TypeChecker TypeReference Boolean

isTupleType :: TypeChecker -> TypeReference -> Effect Boolean
isTupleType = runEffectFn2 isTupleTypeImpl

foreign import isTupleTypeImpl :: EffectFn2 TypeChecker TypeReference Boolean

isReadonlyArrayType :: TypeChecker -> TypeReference -> Effect Boolean
isReadonlyArrayType = runEffectFn2 isReadonlyArrayTypeImpl

foreign import isReadonlyArrayTypeImpl :: EffectFn2 TypeChecker TypeReference Boolean

type PropertyName = String

getTypeOfPropertyOfType :: TypeChecker -> TypeReference -> PropertyName -> Effect (Maybe (Typ ()))
getTypeOfPropertyOfType c r = map toMaybe <$> runEffectFn3 getTypeOfPropertyOfTypeImpl c r

foreign import getTypeOfPropertyOfTypeImpl :: EffectFn3 TypeChecker TypeReference PropertyName (Nullable (Typ ()))

-- | For these singletons we can enforce base `Typ ()` restriction.
isNullType :: TypeChecker -> Typ () -> Boolean
isNullType = runFn2 isNullTypeImpl

foreign import isNullTypeImpl :: Fn2 TypeChecker (Typ ()) Boolean

isUndefinedType :: TypeChecker -> Typ () -> Boolean
isUndefinedType = runFn2 isUndefinedTypeImpl

foreign import isUndefinedTypeImpl :: Fn2 TypeChecker (Typ ()) Boolean
