module ReadDTS.Types where

import Prelude

import Data.String (joinWith)
import ReadDTS (OnVisit, OnType)

data TopLevelDec t
  = Interface { name ∷ String, members ∷ Array (InterfaceMember t) }
  | TypeAlias { name ∷ String, type ∷ t }

instance showTopLevelDec ∷ Show t ⇒ Show (TopLevelDec t) where
  show = case _ of
    TypeAlias r → joinWith " " ["type", r.name, ":", show r.type]
    Interface { name, members } → 
      let
        onMember r = joinWith " "
          [r.name, if r.optional then "?:" else ":", show r.type]
      in
        "Interface " <> name <> ": " <> joinWith "; " (map onMember members)

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

instance showType ∷ Show Type where
  show = case _ of
    Union xs → joinWith " | " $ map show xs
    Primitive p → p
    StringLiteral s → "'" <> s <> "'"
    NumberLiteral n → show n
    Unknown s → "\"" <> s <> "\""

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
