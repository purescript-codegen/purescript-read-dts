module ReadDTS.Instantiation where

import Prelude

import Control.Monad.Except (Except, throwError)
import Data.Array (filter)
import Data.Array as Array
import Data.Either (Either)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldlDefault, foldrDefault, length)
import Data.Functor.Mu (Mu(..), roll)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Data.Tuple (Tuple(..)) as Tuple
import Matryoshka (Algebra, cata)
import ReadDTS (FullyQualifiedName, fqnToString)
import ReadDTS.AST (Application(..), Application', TypeConstructor, TypeNode)
import ReadDTS.AST as AST

type Property a = { type ∷ a, optional ∷ Boolean }

sequenceProperty ∷ ∀ a t. Applicative t ⇒ Property (t a) → t (Property a)
sequenceProperty { "type": t, optional } = { "type": _, optional } <$> t

type Name = Either String FullyQualifiedName

data TypeF a
  = Any
  | Array a
  | Boolean
  | BooleanLiteral Boolean
  | Function
    { parameters ∷ Array { name ∷ String, type ∷ a }
    , returnType ∷ a
    }
  | Intersection a a
  | Null
  | Number
  | NumberLiteral Number
  | Module
    { fullyQualifiedName ∷ String
    , types ∷ Array a
    }
  | Object String (Map String (Property a))
  | String
  | StringLiteral String
  | Tuple (Array a)
  | Undefined
  | Union (Array a)
  | Unknown String
  | Void
derive instance functorTypeF ∷ Functor TypeF
derive instance genericTypeF ∷ Generic (TypeF a) _
derive instance eqTypeF ∷ Eq a ⇒ Eq (TypeF a)
instance showTypeF ∷ Show a ⇒ Show (TypeF a) where
  show = genericShow

-- | XXX: Really literal and naive instance which was required
-- | for testing. I'm not sure if we should provide it here.
instance eq1TypeF ∷ Eq1 TypeF where
  eq1 Any Any = true
  eq1 (Array a1) (Array a2) = eq a1 a2
  eq1 Boolean Boolean = true
  eq1 (BooleanLiteral b1) (BooleanLiteral b2) = b1 == b2
  eq1 (Intersection n1 a1) (Intersection n2 a2) = n1 == n2 && a1 == a2
  eq1 Null Null = true
  eq1 Number Number = true
  eq1 (NumberLiteral n1) (NumberLiteral n2) = n1 == n2
  eq1 (Module m1) (Module m2) = m1.fullyQualifiedName == m2.fullyQualifiedName
  eq1 (Object n1 props1) (Object n2 props2) = n1 == n2 && props1 == props2
  eq1 String String = true
  eq1 (StringLiteral s1) (StringLiteral s2) = s1 == s2
  eq1 (Tuple arr1) (Tuple arr2) = arr1 == arr2
  eq1 Undefined Undefined = true
  eq1 (Union arr1) (Union arr2) = arr1 == arr2
  eq1 (Unknown s1) (Unknown s2) = s1 == s2
  eq1 Void Void = true
  eq1 _ _ = false

instance foldableTypeF ∷ Foldable TypeF where
  foldMap _ Any = mempty
  foldMap f (Array t) = f t
  foldMap _ Boolean = mempty
  foldMap _ (BooleanLiteral _) = mempty
  foldMap f (Function r) = foldMap (f <<< _.type) r.parameters <> f r.returnType
  foldMap f (Intersection t1 t2) = f t1 <> f t2
  foldMap f (Module m) = foldMap f m.types
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
  foldMap _ Void = mempty

  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableTypeF ∷ Traversable TypeF where
  sequence Any = pure Any
  sequence (Array t) = Array <$> t
  sequence Boolean = pure Boolean
  sequence (BooleanLiteral b) = pure (BooleanLiteral b)
  sequence (Function r)
    = map Function
    $ { parameters: _, returnType: _ }
    <$> traverse sequenceParameter r.parameters
    <*> r.returnType
    where
      sequenceParameter { name, type: t } = { name, type: _ } <$> t
  sequence (Intersection t1 t2) = Intersection <$> t1 <*> t2
  sequence (Module { fullyQualifiedName, types }) =
    Module <<< { types: _, fullyQualifiedName } <$> sequence types
  sequence Null = pure Null
  sequence Number = pure $ Number
  sequence (NumberLiteral n) = pure $ NumberLiteral n
  sequence (Object n ts) = Object n <$> (traverse sequenceProperty) ts
  sequence String = pure $ String
  sequence (StringLiteral s) = pure $ (StringLiteral s)
  sequence (Tuple ts) = Tuple <$> (sequence ts)
  sequence Undefined = pure Undefined
  sequence (Union ts) = Union <$> (sequence ts)
  sequence (Unknown msg) = pure $ Unknown msg
  sequence Void = pure Void

  traverse = traverseDefault

type Type = Mu TypeF

type Instantiate = Map String Type → Except String Type

instantiateApplication ∷ Algebra Application Instantiate
instantiateApplication (Application { typeArguments, typeConstructor }) =
  case typeConstructor of
    AST.Interface { fullyQualifiedName, properties, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip instantiateTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      roll <$> Object (fqnToString fullyQualifiedName) <<< Map.fromFoldable <$> for properties \{ name, type: t, optional } →
        (Tuple.Tuple name <<< { type: _, optional }) <$> instantiateTypeNode t ctx'
    AST.Module { declarations, fullyQualifiedName } → \ctx → do
      roll <<< Module <<< { fullyQualifiedName: fqnToString fullyQualifiedName, types: _ } <$> for declarations \tc → do
        instantiateApplication (Application { typeArguments: [], typeConstructor: tc }) ctx
    AST.TypeAlias { type: t, typeParameters } → \ctx → do
      typeArguments' ← traverse (flip instantiateTypeNode ctx) typeArguments
      let
        ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      instantiateTypeNode t ctx'
    AST.Class c → const $ pure $ roll $ Unknown ("Class specialization not implemented yet")
    AST.UnknownTypeConstructor r → const $ pure $ roll $ Unknown
      ("Unknown type constructor: " <> show r.fullyQualifiedName)

intersection ∷ Type → Type → Type
intersection (In (Union ms1)) (In (Union ms2)) = roll $ Union (filter (_ `Array.elem` ms2) ms1)
intersection (In (Union ms)) (In String) = roll $ Union (filter step ms)
  where
    step = case _ of
      In (StringLiteral _) → true
      otherwise → false
intersection s@(In String) u@(In (Union ms)) = intersection u s
intersection s@(In String) (In String) = s
intersection (In (Union ms)) (In Number) = roll $ Union (filter step ms)
  where
    step = case _ of
      In (NumberLiteral _) → true
      otherwise → false
intersection n@(In Number) u@(In (Union _)) = intersection u n
intersection n@(In Number) (In Number) = n
intersection (In (Union ms)) (In Boolean) = roll $ Union (filter step ms)
  where
    step = case _ of
      In (BooleanLiteral _) → true
      otherwise → false
intersection b@(In Boolean) u@(In (Union _)) = intersection u b
intersection b@(In Boolean) (In Boolean) = b
intersection n@(In Null) (In Null) = n
intersection u@(In Undefined) (In Undefined) = u
intersection u@(In Void) (In Void) = u
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
  AST.Function fd → do
    parameters ← traverse (\r@{ name } → { name, type: _ } <$> instantiateTypeNode r.type ctx) fd.parameters
    returnType ← instantiateTypeNode fd.returnType ctx
    pure $ roll $ Function { parameters, returnType }
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
  AST.Void → pure $ roll Void

instantiate
   ∷ TypeConstructor (Application')
   → Array Type
   → Except String Type
instantiate tc args = instantiateApplication application Map.empty
  where
  application = Application
    { typeArguments: map (AST.ApplicationWithRef <<< const <<< pure) args
    , typeConstructor: map (cata instantiateApplication) tc
    }

isObjectLiteral ∷ Type → Boolean
isObjectLiteral (In (Object _ members)) = length members == 0
isObjectLiteral _ = false

