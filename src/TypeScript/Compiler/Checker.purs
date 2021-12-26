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

getSymbolAtLocation :: forall l r. TypeChecker -> Node l r -> Maybe Symbol_
getSymbolAtLocation c = toMaybe <<< runFn2 getSymbolAtLocationImpl c

foreign import getSymbolAtLocationImpl :: forall l r. Fn2 TypeChecker (Node l r) (Nullable Symbol_)

getTypeAtLocation :: forall l r. TypeChecker -> Node l r -> Maybe (Typ ())
getTypeAtLocation c = toMaybe <<< runFn2 getTypeAtLocationImpl c

foreign import getTypeAtLocationImpl :: forall l r. Fn2 TypeChecker (Node l r) (Nullable (Typ ()))

getTypeOfSymbolAtLocation :: forall l r. TypeChecker -> Symbol_ -> Node l r -> Maybe (Typ ())
getTypeOfSymbolAtLocation c s = toMaybe <<< runFn3 getTypeOfSymbolAtLocationImpl c s

foreign import getTypeOfSymbolAtLocationImpl :: forall l r. Fn3 TypeChecker Symbol_ (Node l r) (Nullable (Typ ()))

getFullyQualifiedName :: TypeChecker -> Symbol_ -> FullyQualifiedName
getFullyQualifiedName c = FullyQualifiedName <<< runFn2 getFullyQualifiedNameImpl c

foreign import getFullyQualifiedNameImpl :: Fn2 TypeChecker Symbol_ String

getTypeArguments :: TypeChecker -> TypeReference -> Array (Typ ())
getTypeArguments = runFn2 getTypeArgumentsImpl

foreign import getTypeArgumentsImpl :: Fn2 TypeChecker TypeReference (Array (Typ ()))

