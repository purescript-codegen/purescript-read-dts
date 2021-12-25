module TypeScript.Compiler.Checker where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import TypeScript.Compiler.Types (FullyQualifiedName(..), Node, Symbol_, Typ, TypeChecker)
import TypeScript.Compiler.Types.Typs (TypeReference)

typeToString :: forall r. TypeChecker -> Typ r -> String
typeToString = runFn2 typeToStringImpl

foreign import typeToStringImpl :: forall r. Fn2 TypeChecker (Typ r) String

getSymbolAtLocation :: forall r. TypeChecker -> Node r -> Maybe Symbol_
getSymbolAtLocation c = toMaybe <<< runFn2 getSymbolAtLocationImpl c

foreign import getSymbolAtLocationImpl :: forall k. Fn2 TypeChecker (Node k) (Nullable Symbol_)

getTypeAtLocation :: forall k. TypeChecker -> Node k -> Maybe (Typ ())
getTypeAtLocation c = toMaybe <<< runFn2 getTypeAtLocationImpl c

foreign import getTypeAtLocationImpl :: forall k. Fn2 TypeChecker (Node k) (Nullable (Typ ()))

getTypeOfSymbolAtLocation :: forall k. TypeChecker -> Symbol_ -> Node k -> Maybe (Typ ())
getTypeOfSymbolAtLocation c s = toMaybe <<< runFn3 getTypeOfSymbolAtLocationImpl c s

foreign import getTypeOfSymbolAtLocationImpl :: forall i. Fn3 TypeChecker Symbol_ (Node i) (Nullable (Typ ()))

getFullyQualifiedName :: TypeChecker -> Symbol_ -> FullyQualifiedName
getFullyQualifiedName c = FullyQualifiedName <<< runFn2 getFullyQualifiedNameImpl c

foreign import getFullyQualifiedNameImpl :: Fn2 TypeChecker Symbol_ String

getTypeArguments :: TypeChecker -> TypeReference -> Array (Typ ())
getTypeArguments = runFn2 getTypeArgumentsImpl

foreign import getTypeArgumentsImpl :: Fn2 TypeChecker TypeReference (Array (Typ ()))

