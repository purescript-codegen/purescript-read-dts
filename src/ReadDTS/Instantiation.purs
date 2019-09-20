module ReadDTS.Instantiation where

import Prelude

import Control.Monad.Except (Except, throwError)
import Data.Array as Array
import Data.Functor.Mu (Mu, roll)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Matryoshka (cata)
import ReadDTS.AST (Application(..), TypeConstructor(..), TypeNode)
import ReadDTS.AST as AST

-- | Not our current problems:
-- |
-- | * How to represent recursive cycles so they can be useful?
-- |
-- | * Should we provide some lazy evaluating value in place of recursive type?
-- |
-- | * Should we provide `fullyQualifiedNames` somewhere too?

-- | Current problems:
-- |
-- | * We want to move `AST.apply*` here and rename it probably
-- | `Instatiation.typeNode` `Instantiation.application`.
-- |
-- | * We want to start from the basic types up so let's start to material-ui
-- | Badge.d.ts and provide some kind of "tests" for it.
-- |
-- | * Do we really want to expand ts intersections here? Do we lose something?
-- | How we are going to treat intersections which contain 'Unknown'?

data TypeF a
  = Array a
  | Boolean
  | Number
  -- | Object (Map String { t ∷ a, optional ∷ Boolean })
  | Object (Array { type ∷ a, optional ∷ Boolean, name ∷ String })
  | String
  | Tuple (Array a)
  | Union (Array a)
  | Unknown String

type Type = Mu TypeF

-------------------------------------------------------------------------------- 

type Apply = Map String Type → Except String Type

applyApplication ∷ Application Apply → Apply
applyApplication (Application { typeArguments, typeConstructor }) =
  case typeConstructor of
    Interface { properties, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip applyTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
        _typeL = prop (SProxy ∷ SProxy "type")
      roll <$> Object <$> for properties \{ name, type: t, optional } →
        { name, type: _, optional } <$> (flip applyTypeNode ctx' t)
    TypeAlias { type: t, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip applyTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      applyTypeNode t ctx'
    UnknownTypeConstructor r → const $ pure $ roll $ Unknown
      ("Unknown type constructor: " <> show r.fullyQualifiedName)

applyTypeNode ∷ TypeNode Apply → Apply
applyTypeNode typeNode ctx = case typeNode of
  AST.AnonymousObject ps →
    roll <$> Object <$> for ps \{ name, type: t, optional } →
      { name, type: _, optional } <$> applyTypeNode t ctx
  AST.Array t → roll <$> Array <$> applyTypeNode t ctx
  AST.Boolean → pure $ roll Boolean
  AST.Intersection ts → 
    throwError "Apply for Intersection unsupported"
    -- roll <$> Intersection <$> traverse (flip applyTypeNode ctx) ts
  AST.Number → pure $ roll Number
  AST.String → pure $ roll String
  AST.TypeParameter { name, default } → 
    case Map.lookup name ctx, default of
      Just t, _ → pure t
      Nothing, Just d → applyTypeNode d ctx
      _, _ → throwError ("Variable not defined: " <> name)
  AST.Union ts → roll <$> Union <$> traverse (flip applyTypeNode ctx) ts
  AST.Tuple ts → roll <$> Tuple <$> traverse (flip applyTypeNode ctx) ts
  AST.TypeApplication ref → ref ctx
  AST.UnknownTypeNode s → pure $ roll $ Unknown s

app ∷ Mu Application → Except String Type
app t = cata applyApplication t Map.empty

-- foo ∷ Mu TypeNode → Type
