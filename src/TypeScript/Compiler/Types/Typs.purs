module TypeScript.Compiler.Types.Typs where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Undefined.NoProblem (Opt)
import TypeScript.Compiler.Types (Typ)

-- | At the moment it is enough to use `Typ` api to access internals of this interface.
type TypeParameter = Typ ()

type InterfaceTypeRow r =
  ( typeParameters :: Opt (Array TypeParameter)
  , outerTypeParameters :: Opt (Array TypeParameter)
  , localTypeParameters :: Opt (Array TypeParameter)
  , thisType :: Opt TypeParameter
  | r
  )

type GenericTypeRow r = InterfaceTypeRow r

type TypeReference = Typ
  ( target :: Typ (GenericTypeRow ())
  , typeArguments :: Opt (Array (Typ ()))
  )

asTypeReference :: forall r. Typ r -> Maybe TypeReference
asTypeReference = toMaybe <<< runFn1 asTypeReferenceImpl

foreign import asTypeReferenceImpl :: forall r. Fn1 (Typ r) (Nullable TypeReference)

