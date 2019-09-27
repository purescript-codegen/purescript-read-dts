module ReadDTS.Instantiation where

import Prelude

import Control.Monad.Except (Except, throwError)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap, foldl, foldlDefault, foldrDefault, intercalate, length)
import Data.Functor.Mu (Mu(..), roll)
import Data.List (List(..)) as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Data.Tuple (Tuple(..)) as Tuple
import Matryoshka (Algebra, cata)
import ReadDTS (FullyQualifiedName, fqnToString)
import ReadDTS.AST (Application(..), Application', TypeConstructor(..), TypeNode)
import ReadDTS.AST as AST
import Text.Pretty (Columns(..), Doc, Stack(..), hcat, render, text)
import Text.Pretty (Stack(..), beside, empty, width) as Pretty

type Property a = { type ∷ a, optional ∷ Boolean }

sequenceProperty ∷ ∀ a t. Applicative t ⇒ Property (t a) → t (Property a)
sequenceProperty { "type": t, optional } = { "type": _, optional } <$> t

type Name = Either String FullyQualifiedName

data TypeF a
  = Any
  | Array a
  | Boolean
  | BooleanLiteral Boolean
  | Intersection a a
  | Null
  | Number
  | NumberLiteral Number
  | Object String (Map String (Property a))
  | String
  | StringLiteral String
  | Tuple (Array a)
  | Undefined
  | Union (Array a)
  | Unknown String
derive instance functorTypeF ∷ Functor TypeF

instance foldableTypeF ∷ Foldable TypeF where
  foldMap _ Any = mempty
  foldMap f (Array t) = f t
  foldMap _ Boolean = mempty
  foldMap _ (BooleanLiteral _) = mempty
  foldMap f (Intersection t1 t2) = f t1 <> f t2
  foldMap _ Null = mempty
  foldMap f Number = mempty
  foldMap _ (NumberLiteral _) = mempty
  foldMap f (Object _ ts) = foldMap (f <<< _.type) ts
  foldMap f String = mempty
  foldMap f (StringLiteral _) = mempty
  foldMap f (Tuple ts) = foldMap f ts
  foldMap _ Undefined = mempty
  foldMap f (Union ts) = foldMap f ts
  foldMap f (Unknown _)= mempty

  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableTypeF ∷ Traversable TypeF where
  sequence Any = pure Any
  sequence (Array t) = Array <$> t
  sequence Boolean = pure Boolean
  sequence (BooleanLiteral b) = pure (BooleanLiteral b)
  sequence (Intersection t1 t2) = Intersection <$> t1 <*> t2
  sequence Null = pure Null
  sequence Number = pure $ Number
  sequence (NumberLiteral n) = pure $ NumberLiteral n
  sequence (Object n ts) = Object n <$> (sequence <<< map sequenceProperty) ts
  sequence String = pure $ String
  sequence (StringLiteral s) = pure $ (StringLiteral s)
  sequence (Tuple ts) = Tuple <$> (sequence ts)
  sequence Undefined = pure Undefined
  sequence (Union ts) = Union <$> (sequence ts)
  sequence (Unknown msg) = pure $ Unknown msg

  traverse = traverseDefault

type Type = Mu TypeF

type Instantiate = Map String Type → Except String Type

instantiateApplication ∷ Algebra Application Instantiate
instantiateApplication (Application { typeArguments, typeConstructor }) =
  case typeConstructor of
    Interface { fullyQualifiedName, properties, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip instantiateTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      roll <$> Object (fqnToString fullyQualifiedName) <<< Map.fromFoldable <$> for properties \{ name, type: t, optional } →
        (Tuple.Tuple name <<< { type: _, optional }) <$> instantiateTypeNode t ctx'
    TypeAlias { type: t, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip instantiateTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      instantiateTypeNode t ctx'
    UnknownTypeConstructor r → const $ pure $ roll $ Unknown
      ("Unknown type constructor: " <> show r.fullyQualifiedName)

intersection ∷ Type → Type → Type
intersection (In (Object fqn1 o1)) (In (Object fqn2 o2)) = roll $ Object (fqn1 <> " & " <> fqn2) $
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
  AST.AnonymousObject fqn properties →
    roll <$> Object (fqnToString fqn) <<< Map.fromFoldable <$> for properties \{ name, type: t, optional } →
      ((Tuple.Tuple name <<< { type: _, optional }) <$> instantiateTypeNode t ctx)
  AST.ApplicationWithRef ref → ref ctx
  AST.Array t → roll <$> Array <$> instantiateTypeNode t ctx
  AST.Boolean → pure $ roll Boolean
  AST.BooleanLiteral b → pure $ roll $ BooleanLiteral b
  AST.Intersection ts →
    traverse (flip instantiateTypeNode ctx) ts >>= Array.reverse >>> Array.uncons >>> case _ of
      Nothing → throwError "Empty intersection"
      Just { head, tail: [] } → pure head
      Just { head, tail } → pure $ foldl intersection head tail
  AST.Null → pure $ roll Null
  AST.Number → pure $ roll Number
  AST.NumberLiteral n → pure $ roll $ NumberLiteral n
  AST.String → pure $ roll String
  AST.StringLiteral s → pure $ roll $ StringLiteral s
  AST.TypeParameter { name, default } →
    case Map.lookup name ctx, default of
      Just t, _ → pure t
      Nothing, Just d → instantiateTypeNode d ctx
      _, _ → throwError ("Variable not defined: " <> name)
  AST.Tuple ts → roll <$> Tuple <$> traverse (flip instantiateTypeNode ctx) ts
  AST.Undefined → pure $ roll Undefined
  AST.Union ts → roll <$> Union <$> traverse (flip instantiateTypeNode ctx) ts
  AST.UnknownTypeNode s → pure $ roll $ Unknown s

instantiate
   ∷ TypeConstructor (Application')
   → Array Type
   → Except String Type
instantiate tc args = instantiateApplication application mempty
  where
  application = Application
    { typeArguments: map (AST.ApplicationWithRef <<< const <<< pure) args
    , typeConstructor: map (cata instantiateApplication) tc
    }

inline ∷ String → Columns
inline = Columns <<< text

joinWithDoc ∷ Doc → Array Doc → Maybe Int → Doc
joinWithDoc sep elems (Just w) = case Array.uncons elems of
  Nothing → Pretty.empty 0 0
  Just { head, tail } →  doc
    where
      step { row, rows } curr = if Pretty.width row + Pretty.width sep + Pretty.width curr > w
        then { row: curr, rows: List.Cons row rows }
        else { row: row `Pretty.beside` sep `Pretty.beside` curr, rows }
      { row, rows } = foldl step { row: head, rows: List.Nil } tail
      doc = unwrap $ foldl (\rows curr → rows <> Pretty.Stack (sep `Pretty.beside` curr)) (Pretty.Stack row) $ rows

joinWithDoc sep elems Nothing =
  unwrap $ intercalate (Columns sep) (map Columns elems)

isObjectLiteral ∷ Type → Boolean
isObjectLiteral (In (Object _ members)) = length members == 0
isObjectLiteral _ = false

pprint ∷ Type → String
pprint = render <<< cata alg
  where
  alg ∷ Algebra TypeF Doc
  alg Any = text "any"
  alg (Array t) = hcat [ text "[", t, text "]" ]
  alg Boolean = text "boolean"
  alg (Intersection t1 t2) = hcat [ t1, text " & ", t2 ]
  alg Null = text "null"
  alg Number = text "number"
  alg (Object _ props) | length props == 0 = text "{}"
  alg (Object fqn props) = objDoc
    where
      sep = text ":  "
      sepOpt = text "?: "
      step (Tuple.Tuple n { type: t, optional }) =
        Stack $ hcat [ text n, (if optional then sepOpt else sep), t ]
      nameCol = Columns $ text $ "<" <> fqn <> ">: "
      propsCol
        = Columns
        <<< unwrap
        <<< foldMap step
        $ (Map.toUnfoldable props ∷ Array _)
      objDoc = unwrap $ nameCol <> propsCol
  alg String = text "string"
  alg (Tuple ts) =
    hcat [ text "(", joinWithDoc (text ", ") ts (Just 80), text ")" ]
  alg (BooleanLiteral b) = text $ "@" <> show b
  alg (StringLiteral s) = text $ "@" <> show s
  alg (NumberLiteral n) = text $ "@" <> show n
  alg Undefined = text "undefined"
  alg (Union ts) = joinWithDoc (text " | ") ts (Just 80)
  alg (Unknown s) = text $ "unknown: " <> s <> ""
