module TypeScript.Compiler.Checker where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import TypeScript.Compiler.Types (FullyQualifiedName(..), Node, Signature, SignatureKind, Symbol_, Typ, TypeChecker)
import TypeScript.Compiler.Types.Typs (TypeReference)

typeToString :: forall r. TypeChecker -> Typ r -> String
typeToString = runFn2 typeToStringImpl

foreign import typeToStringImpl :: forall r. Fn2 TypeChecker (Typ r) String

foreign import data ModuleSymbol :: Type

foreign import getExportsOfModuleImpl :: Fn2 TypeChecker Symbol_ (Array Symbol_)

getExportsOfModule :: TypeChecker -> Symbol_ -> Array Symbol_
getExportsOfModule c = runFn2 getExportsOfModuleImpl c

foreign import getExportsAndPropertiesOfModuleImpl :: Fn2 TypeChecker Symbol_ (Array Symbol_)

getExportsAndPropertiesOfModule :: TypeChecker -> Symbol_ -> Array Symbol_
getExportsAndPropertiesOfModule c = runFn2 getExportsAndPropertiesOfModuleImpl c

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

getExportedFullyQualifiedName :: TypeChecker -> Symbol_ -> FullyQualifiedName
getExportedFullyQualifiedName c s = do
  let
    s' = getExportSymbolOfSymbol c s
  getFullyQualifiedName c s'

-- You should rather use getExportedSymbol' because the original API is non obvious:
--
-- Original docs:
--
--  If a symbol is a local symbol with an associated exported symbol, returns the exported symbol.
--  Otherwise returns its input.
--  For example, at `export type T = number;`:
--      - `getSymbolAtLocation` at the location `T` will return the exported symbol for `T`.
--      - But the result of `getSymbolsInScope` will contain the *local* symbol for `T`, not the exported symbol.
--      - Calling `getExportSymbolOfSymbol` on that local symbol will return the exported symbol.
getExportSymbolOfSymbol :: TypeChecker -> Symbol_ -> Symbol_
getExportSymbolOfSymbol c = runFn2 getExportSymbolOfSymbolImpl c

foreign import getExportSymbolOfSymbolImpl :: Fn2 TypeChecker Symbol_ Symbol_

getTypeArguments :: TypeChecker -> TypeReference -> Array (Typ ())
getTypeArguments = runFn2 getTypeArgumentsImpl

foreign import getTypeArgumentsImpl :: Fn2 TypeChecker TypeReference (Array (Typ ()))

getSignaturesOfType :: forall i. TypeChecker -> Typ i -> SignatureKind -> Array Signature
getSignaturesOfType = runFn3 getSignaturesOfTypeImpl

foreign import getSignaturesOfTypeImpl :: forall i. Fn3 TypeChecker (Typ i) SignatureKind (Array Signature)

