module ReadDTS.Instantiation where

import Prelude

import Control.Monad.Except (Except, throwError)
import Data.Array as Array
import Data.Functor.Mu (Mu)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Global.Unsafe (unsafeStringify)
import Matryoshka (cata)
import Partial.Unsafe (unsafeCrashWith)
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
-- | How we are going to treat intersections which contain 'Uknown'?

data TypeF a
  = Array a
  | Boolean
  | Number
  | Object (Map String { t ∷ a, optional ∷ Boolean })
  | String
  | Tuple (Array a)
  | Union (Array a)
  | Uknown String

type Type = Mu TypeF

-------------------------------------------------------------------------------- 

type Apply a = Map String (TypeNode a) → Except String (TypeNode a)

applyApplication ∷ ∀ a. Application (Apply a) → (Apply a)
applyApplication (Application { typeArguments, typeConstructor }) =
  case typeConstructor of
    Interface { properties, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip applyTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
        _typeL = prop (SProxy ∷ SProxy "type")
      AST.AnonymousObject <$> for properties \{ name, type: t, optional } →
        { name, type: _, optional } <$> (flip applyTypeNode ctx' t)
    TypeAlias { type: t, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip applyTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      applyTypeNode t ctx'
    UnknownTypeConstructor r → const $ pure $ AST.UnknownTypeNode
      ("Unknown type constructor: " <> show r.fullyQualifiedName)

applyTypeNode ∷ ∀ a. TypeNode (Apply a) → (Apply a)
applyTypeNode (AST.AnonymousObject ps) ctx =
  AST.AnonymousObject <$> for ps \{ name, type: t, optional } →
    { name, type: _, optional } <$> applyTypeNode t ctx
applyTypeNode (AST.Array t) ctx = AST.Array <$> applyTypeNode t ctx
applyTypeNode AST.Boolean ctx = pure AST.Boolean
applyTypeNode (AST.Intersection ts) ctx = 
  AST.Intersection <$> traverse (flip applyTypeNode ctx) ts
applyTypeNode AST.Number ctx = pure AST.Number
applyTypeNode AST.String ctx = pure $ AST.String
applyTypeNode (AST.TypeParameter { name, default }) ctx = 
  case Map.lookup name ctx, default of
    Just t, _ → pure t
    Nothing, Just d → applyTypeNode d ctx
    _, _ → throwError ("Variable not defined: " <> name)
applyTypeNode (AST.Union ts) ctx = AST.Union <$> traverse (flip applyTypeNode ctx) ts
applyTypeNode (AST.Tuple ts) ctx = AST.Tuple <$> traverse (flip applyTypeNode ctx) ts
applyTypeNode (AST.TypeApplication ref) ctx = unsafeCrashWith
  $ "Matching void case (applyTypeNode): " <> unsafeStringify ref
applyTypeNode (AST.UnknownTypeNode s) ctx = pure $ AST.UnknownTypeNode s

app ∷ ∀ a. Mu Application → Except String (TypeNode a)
app t = cata applyApplication t Map.empty

-- foo ∷ Mu TypeNode → Type
