module TypeScript.Compiler.Checker.Internal where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import TypeScript.Compiler.Types (TypeChecker, Typ)
import TypeScript.Compiler.Types.Typs (TypeReference)

isAnyType ∷ forall i. TypeChecker -> Typ i -> Boolean
isAnyType = runFn2 isAnyTypeImpl

foreign import isAnyTypeImpl :: forall i. Fn2 TypeChecker (Typ i) Boolean

isArrayType ∷ forall i. TypeChecker -> Typ i -> Boolean
isArrayType = runFn2 isArrayTypeImpl

foreign import isArrayTypeImpl :: forall i. Fn2 TypeChecker (Typ i) Boolean

-- | We need to expose some extra (internal!) pieces of the type checker
-- | to easily process some builtin types etc.
getElementTypeOfArrayType ∷ forall i. TypeChecker -> Typ i -> Maybe (Typ ())
getElementTypeOfArrayType c = toMaybe <<< runFn2 getElementTypeOfArrayTypeImpl c

foreign import getElementTypeOfArrayTypeImpl :: forall i. Fn2 TypeChecker (Typ i) (Nullable (Typ ()))

isBooleanType :: TypeChecker -> Typ () -> Boolean
isBooleanType = runFn2 isBooleanTypeImpl

foreign import isBooleanTypeImpl :: Fn2 TypeChecker (Typ ()) Boolean

-- | For these singletons we can enforce base `Typ ()` restriction.
isNullType :: forall i. TypeChecker -> Typ i -> Boolean
isNullType = runFn2 isNullTypeImpl

foreign import isNullTypeImpl :: forall i. Fn2 TypeChecker (Typ i) Boolean

isNumberType :: TypeChecker -> Typ () -> Boolean
isNumberType = runFn2 isNumberTypeImpl

foreign import isNumberTypeImpl :: forall i. Fn2 TypeChecker (Typ i) Boolean

isStringType :: TypeChecker -> Typ () -> Boolean
isStringType = runFn2 isStringTypeImpl

foreign import isStringTypeImpl :: forall i. Fn2 TypeChecker (Typ i) Boolean

isTupleType ∷ TypeChecker -> TypeReference -> Boolean
isTupleType = runFn2 isTupleTypeImpl

foreign import isTupleTypeImpl :: forall i. Fn2 TypeChecker (Typ i) Boolean

isReadonlyArrayType ∷ forall i. TypeChecker -> (Typ i) -> Boolean
isReadonlyArrayType = runFn2 isReadonlyArrayTypeImpl

foreign import isReadonlyArrayTypeImpl :: forall i. Fn2 TypeChecker (Typ i) Boolean

isUndefinedType :: forall i. TypeChecker -> Typ i -> Boolean
isUndefinedType = runFn2 isUndefinedTypeImpl

foreign import isUndefinedTypeImpl :: forall i. Fn2 TypeChecker (Typ i) Boolean

type PropertyName = String

getTypeOfPropertyOfType ∷ forall i. TypeChecker → (Typ i) → PropertyName → (Maybe (Typ ()))
getTypeOfPropertyOfType c r = toMaybe <<< runFn3 getTypeOfPropertyOfTypeImpl c r

foreign import getTypeOfPropertyOfTypeImpl :: forall i. Fn3 TypeChecker (Typ i) PropertyName (Nullable (Typ ()))

