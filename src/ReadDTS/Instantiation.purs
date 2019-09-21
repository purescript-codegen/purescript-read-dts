module ReadDTS.Instantiation where

import Prelude

import Control.Monad.Except (Except, throwError)
import Data.Array as Array
import Data.Foldable (foldMap, foldl, intercalate)
import Data.Functor.Mu (Mu(..), roll)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..)) as Tuple
import Matryoshka (Algebra, cata)
import ReadDTS.AST (Application(..), TypeConstructor(..), TypeNode, Application')
import ReadDTS.AST as AST
import Text.Pretty (Columns(..), Doc, Stack(..), hcat, text)

data TypeF a
  = Any
  | Array a
  | Boolean
  | Intersection a a
  | Number
  | Object (Map String { type ∷ a, optional ∷ Boolean })
  | String
  | Tuple (Array a)
  | BooleanLiteral Boolean
  | NumberLiteral Number
  | StringLiteral String
  | Union (Array a)
  | Unknown String
derive instance functorTypeF ∷ Functor TypeF

type Type = Mu TypeF

type Instantiate = Map String Type → Except String Type

instantiateApplication ∷ Algebra Application Instantiate
instantiateApplication (Application { typeArguments, typeConstructor }) =
  case typeConstructor of
    Interface { properties, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip instantiateTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      roll <$> Object <<< Map.fromFoldable <$> for properties \{ name, type: t, optional } →
        (Tuple.Tuple name <<< { type: _, optional }) <$> instantiateTypeNode t ctx'
    TypeAlias { type: t, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip instantiateTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      instantiateTypeNode t ctx'
    UnknownTypeConstructor r → const $ pure $ roll $ Unknown
      ("Unknown type constructor: " <> show r.fullyQualifiedName)

intersection ∷ Type → Type → Type
intersection (In (Object o1)) (In (Object o2)) = roll $ Object $
  Map.unionWith step o1 o2
  where
    step { type: t1, optional: op1 } { type: t2, optional: op2 } =
      { type: intersection t1 t2
      , optional: op1 && op2
      }
intersection t1 t2 = roll $ Intersection t1 t2

instantiateTypeNode ∷ Algebra TypeNode Instantiate
instantiateTypeNode typeNode ctx = case typeNode of
  AST.Any → pure $ roll Any
  AST.AnonymousObject properties →
    roll <$> Object <<< Map.fromFoldable <$> for properties \{ name, type: t, optional } →
      ((Tuple.Tuple name <<< { type: _, optional }) <$> instantiateTypeNode t ctx)
  AST.Array t → roll <$> Array <$> instantiateTypeNode t ctx
  AST.Boolean → pure $ roll Boolean
  AST.Intersection ts →
    traverse (flip instantiateTypeNode ctx) ts >>= Array.reverse >>> Array.uncons >>> case _ of
      Nothing → throwError "Empty intersection"
      Just { head, tail: [] } → pure head
      Just { head, tail } → pure $ foldl intersection head tail
  AST.Number → pure $ roll Number
  AST.String → pure $ roll String
  AST.TypeParameter { name, default } →
    case Map.lookup name ctx, default of
      Just t, _ → pure t
      Nothing, Just d → instantiateTypeNode d ctx
      _, _ → throwError ("Variable not defined: " <> name)
  AST.Tuple ts → roll <$> Tuple <$> traverse (flip instantiateTypeNode ctx) ts
  AST.TypeApplication ref → ref ctx
  AST.BooleanLiteral b → pure $ roll $ BooleanLiteral b
  AST.NumberLiteral n → pure $ roll $ NumberLiteral n
  AST.StringLiteral s → pure $ roll $ StringLiteral s
  AST.Union ts → roll <$> Union <$> traverse (flip instantiateTypeNode ctx) ts
  AST.UnknownTypeNode s → pure $ roll $ Unknown s

instantiate
   ∷ TypeConstructor (Application')
   → Array Type
   → Except String Type
instantiate tc args = instantiateApplication application mempty
  where
  application = Application
    { typeArguments: map (AST.TypeApplication <<< const <<< pure) args
    , typeConstructor: map (cata instantiateApplication) tc
    }

inline ∷ String → Columns
inline = Columns <<< text

joinWithDoc ∷ Doc → Array Doc → Doc
joinWithDoc sep elems =
  unwrap $ intercalate (Columns sep) (map Columns elems)

pprint ∷ Algebra TypeF Doc
pprint Any = text "any"
pprint (Array t) = hcat [ text "[", t, text "]" ]
pprint Boolean = text "boolean"
pprint (Intersection t1 t2) = hcat [ t1, text " & ", t2 ]
pprint Number = text "number"
pprint (Object props) = props'
  where
    sep = text ":  "
    sepOpt = text "?: "
    step (Tuple.Tuple n { type: t, optional }) =
      Stack $ hcat [ text n, (if optional then sepOpt else sep), t ]
    props'
      = unwrap
      <<< foldMap step
      $ (Map.toUnfoldable props ∷ Array _)
pprint String = text "string"
pprint (Tuple ts) =
  hcat [ text "(", joinWithDoc (text ", ") ts, text ")" ]
pprint (BooleanLiteral b) = text $ "@" <> show b
pprint (StringLiteral s) = text $ "@" <> show s
pprint (NumberLiteral n) = text $ "@" <> show n
pprint (Union ts) = joinWithDoc (text " | ") ts
pprint (Unknown s) = text $ "unknown: " <> s
