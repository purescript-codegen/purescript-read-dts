module ReadDTS.Types where

import ReadDTS (OnVisit, OnType)

data TopLevelDec t
  = Interface { name ∷ String, members ∷ Array (InterfaceMember t) }
  | TypeAlias { name ∷ String, type ∷ t }

type InterfaceMember t = { name ∷ String, type ∷ t, optional ∷ Boolean }
-- newtype InterfaceMember t = InterfaceMember 
--   { name ∷ String, type ∷ t, optional ∷ Boolean }
-- derive instance newtypeInterfaceMember ∷ Newtype (InterfaceMember t) _

data Type
  = Union (Array Type)
  | Primitive String
  | StringLiteral String
  | NumberLiteral Number
  | Unknown String

type OnVisit_ = OnVisit (TopLevelDec Type) Type
type OnType_ = OnType Type

onVisit ∷ OnVisit_
onVisit = 
  { interface: Interface
  , typeAlias: TypeAlias
  }

onType ∷ OnType_
onType = 
  { unionOrIntersection: Union
  , primitive: Primitive
  , stringLiteral: StringLiteral
  , numberLiteral: NumberLiteral
  , unknown: Unknown
  }
