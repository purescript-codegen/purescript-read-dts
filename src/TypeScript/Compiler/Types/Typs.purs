module TypeScript.Compiler.Types.Typs where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Undefined.NoProblem (Opt)
import Type.Row (type (+))
import TypeScript.Compiler.Types (Symbol_, Typ, TypeFlags)
import Unsafe.Coerce (unsafeCoerce)

type TypRow r =
  ( aliasSymbol :: Opt Symbol_
  , aliasTypeArguments :: Opt (Array (Typ ()))
  , flags :: TypeFlags
  , symbol :: Symbol_
  | r
  )

interface :: forall i. Typ i -> { | TypRow + i }
interface = unsafeCoerce

forget :: forall i. Typ i -> Typ ()
forget = unsafeCoerce

type BooleanLiteralType = Typ (value :: Boolean)

type GenericTypeRow r = InterfaceTypeRow r

asClassType :: forall r. Typ r -> Maybe InterfaceType
asClassType = toMaybe <<< runFn1 asClassTypeImpl

foreign import asClassTypeImpl :: forall r. Fn1 (Typ r) (Nullable InterfaceType)

type IntersectionType = Typ (types :: Array (Typ ()))

asIntersectionType :: forall r. Typ r -> Maybe IntersectionType
asIntersectionType = toMaybe <<< runFn1 asIntersectionTypeImpl

foreign import asIntersectionTypeImpl :: forall r. Fn1 (Typ r) (Nullable IntersectionType)

-- | Interface or Class
type InterfaceTypeRow r
  = ObjectTypeRow
  + ( typeParameters :: Opt (Array TypeParameter)
    , outerTypeParameters :: Opt (Array TypeParameter)
    , localTypeParameters :: Opt (Array TypeParameter)
    , thisType :: Opt TypeParameter
    | r
    )

type InterfaceType = Typ (InterfaceTypeRow ())

asInterfaceType :: forall r. Typ r -> Maybe InterfaceType
asInterfaceType = toMaybe <<< runFn1 asInterfaceTypeImpl

foreign import asInterfaceTypeImpl :: forall r. Fn1 (Typ r) (Nullable InterfaceType)

type NumberLiteralType = Typ (value :: Number)

asNumberLiteralType :: forall r. Typ r -> Maybe NumberLiteralType
asNumberLiteralType = toMaybe <<< runFn1 asNumberLiteralTypeImpl

foreign import asNumberLiteralTypeImpl :: forall r. Fn1 (Typ r) (Nullable NumberLiteralType)

foreign import data ObjectFlags :: Type

type ObjectTypeRow r = (objectFlags :: ObjectFlags, properties :: Array Symbol | r)

-- | `ObjectType` has a broad scope in the ts terms
type ObjectType = Typ (ObjectTypeRow + ())

asObjectType :: forall r. Typ r -> Maybe ObjectType
asObjectType = toMaybe <<< runFn1 asObjectTypeImpl

foreign import asObjectTypeImpl :: forall r. Fn1 (Typ r) (Nullable ObjectType)

type StringLiteralType = Typ (value :: String)

asStringLiteralType :: forall r. Typ r -> Maybe StringLiteralType
asStringLiteralType = toMaybe <<< runFn1 asStringLiteralTypeImpl

foreign import asStringLiteralTypeImpl :: forall r. Fn1 (Typ r) (Nullable StringLiteralType)

type TypeParameter = Typ ()

type TypeReference = Typ
  ( target :: Typ (GenericTypeRow ())
  , typeArguments :: Opt (Array (Typ ()))
  )

asTypeReference :: forall r. Typ r -> Maybe TypeReference
asTypeReference = toMaybe <<< runFn1 asTypeReferenceImpl

foreign import asTypeReferenceImpl :: forall r. Fn1 (Typ r) (Nullable TypeReference)


type UnionType = Typ (types :: Array (Typ ()))

asUnionType :: forall r. Typ r -> Maybe UnionType
asUnionType = toMaybe <<< runFn1 asUnionTypeImpl

foreign import asUnionTypeImpl :: forall r. Fn1 (Typ r) (Nullable UnionType)

asTypeParameter :: forall r. Typ r -> Maybe TypeParameter
asTypeParameter = toMaybe <<< runFn1 asTypeParameterImpl

foreign import asTypeParameterImpl :: forall r. Fn1 (Typ r) (Nullable TypeParameter)

getProperties :: forall r. Typ r -> Array Symbol_
getProperties = runFn1 getPropertiesImpl

foreign import getPropertiesImpl :: forall i. Fn1 (Typ i) (Array Symbol_)

getSymbol :: forall i. Typ i -> Maybe Symbol_
getSymbol = toMaybe <<< runFn1 getSymbolImpl

foreign import getSymbolImpl :: forall i. Fn1 (Typ i) (Nullable Symbol_)

-- | Dafault type for type parameter
getDefault :: forall i. Typ i -> Maybe (Typ ())
getDefault = toMaybe <<< runFn1 getDefaultImpl

foreign import getDefaultImpl :: forall i. Fn1 (Typ i) (Nullable (Typ ()))
