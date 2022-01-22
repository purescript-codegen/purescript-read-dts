module TypeScript.Compier.Types where

import TypeScript.Compiler.Types (Signature, Symbol_, Typ)
import TypeScript.Compiler.Types.Nodes (SignatureDeclaration) as Nodes
import TypeScript.Compiler.Types.Typs (TypeParameter) as Typs

foreign import getDeclaration :: Signature -> Nodes.SignatureDeclaration

foreign import getTypeParameters :: Signature -> Array Typs.TypeParameter

foreign import getParameters :: Signature -> Array Symbol_

foreign import getReturnType :: Signature -> Typ ()

-- foreign import getDocumentationComment :: Signature -> --(): SymbolDisplayPart[] {

