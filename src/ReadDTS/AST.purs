module ReadDTS.AST where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.FoldableWithIndex (foldMapDefault)
import Data.Functor.Mu (Mu)
import Data.Map (Map)
import Data.Map (insert) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Traversable (class Traversable, for, sequence, traverseDefault)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (modify) as Ref
import Global.Unsafe (unsafeStringify)
import Matryoshka (class Corecursive, CoalgebraM, GCoalgebraM, anaM, embed, futuM)
import Matryoshka.Pattern.CoEnvT (CoEnvT(..))
import ReadDTS (FullyQualifiedName(..), TsDeclaration, compilerOptions, fqnToString, readDTS, unsafeTsStringToString)
import ReadDTS (OnDeclaration, OnType, TsString) as ReadDTS
import Unsafe.Coerce (unsafeCoerce)

type Property ref =
  { name ∷ String
  , type ∷ TypeNode ref
  , optional ∷ Boolean
  }

sequenceProperty ∷ ∀ ref t. Applicative t ⇒ Property (t ref) → t (Property ref)
sequenceProperty { name, type: t, optional } = { type: _, name, optional } <$> sequence t

data Declaration ref
  = Interface
    { fullyQualifiedName ∷ FullyQualifiedName
    , name ∷ String
    , properties ∷ Array (Property ref)
    , typeParameters ∷ Array (TypeNode ref)
    }
  | TypeAlias
    { name ∷ String
    , type ∷ (TypeNode ref)
    , typeParameters ∷ Array (TypeNode ref)
    }
  | UnknownDeclaration
    { fullyQualifiedName ∷ Maybe FullyQualifiedName
    , msg ∷ String
    }
derive instance functorDeclaration ∷ Functor Declaration

instance foldableDeclaration ∷ Foldable Declaration where
  foldMap f (Interface i)
    = foldMap (foldMap f <<< _.type) i.properties
    <> foldMap (foldMap f) i.typeParameters
  foldMap f (TypeAlias ta)
    = foldMap f ta.type
    <> foldMap (foldMap f) ta.typeParameters
  foldMap f (UnknownDeclaration _) = mempty

  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableDeclaration ∷ Traversable Declaration where
  sequence (Interface i) = map Interface
    $ (\ms tp → i { properties = ms, typeParameters = tp })
    <$> (sequence <<< map sequenceProperty) i.properties
    <*> (sequence <<< map sequence) i.typeParameters
  sequence (TypeAlias ta) = map TypeAlias $ { type: _, typeParameters: _, name: ta.name }
    <$> sequence ta.type
    <*> (sequence <<< map sequence) ta.typeParameters
  sequence (UnknownDeclaration s) = pure $ (UnknownDeclaration s)
  traverse = traverseDefault

data TypeNode ref
  = AnonymousObject (Array (Property ref))
  | Boolean
  | Intersection (Array (TypeNode ref))
  | Number
  | String
  | TypeParameter
    { identifier ∷ ReadDTS.TsString
    , default ∷ Maybe (TypeNode ref)
    }
  | TypeReference
    { typeArguments ∷ Array (TypeNode ref)
    , fullyQualifiedName ∷ FullyQualifiedName
    , ref ∷ ref
    }
  | Union (Array (TypeNode ref))
  | UnknownTypeNode String

derive instance functorTypeNode ∷ Functor TypeNode

instance foldableTypeNode ∷ Foldable TypeNode where
  foldMap f (AnonymousObject ts) = foldMap (foldMap f <<< _.type) ts
  foldMap _ Boolean = mempty
  foldMap f (Intersection ts) = fold (map (foldMap f) ts)
  foldMap f Number = mempty
  foldMap f String = mempty
  foldMap f (Union ts) = fold (map (foldMap f) ts)
  foldMap f (TypeParameter { default }) = fold (map (foldMap f) default)
  foldMap f (TypeReference { ref }) = f ref
  foldMap f (UnknownTypeNode _) = mempty
  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableTypeNode ∷ Traversable TypeNode where
  sequence (AnonymousObject ts) = AnonymousObject <$> (sequence <<< map sequenceProperty) ts
  sequence Boolean = pure Boolean
  sequence (Intersection ts) = Intersection <$> (sequence <<< map sequence) ts
  sequence Number = pure $ Number
  sequence String = pure $ String
  sequence (TypeParameter { identifier, default }) =
    TypeParameter <<< { identifier, default: _ } <$> (sequence <<< map sequence) default
  sequence (TypeReference { ref, fullyQualifiedName, typeArguments }) = map TypeReference
    $ { ref: _, typeArguments: _, fullyQualifiedName }
    <$> ref
    <*> (sequence <<< map sequence) typeArguments
  sequence (Union ts) = Union <$> (sequence <<< map sequence) ts
  sequence (UnknownTypeNode s) = pure $ UnknownTypeNode s
  traverse = traverseDefault

type TsRef = TsDeclaration
-- { ts ∷ TsDeclaration, fullyQualifiedName ∷ FullyQualifiedName }

type ReadDeclaration = TsRef → Effect (Declaration TsRef)

type Seed =
  { level ∷ Int
  , declaration ∷ TsRef
  }

type Declaration' = Mu Declaration

coalgebra ∷ ReadDeclaration → CoalgebraM Effect Declaration Seed
coalgebra readDeclaration { level, declaration } = if level < 5
  then do
    d ← readDeclaration declaration
    let
      seed tsRef = { level: level + 1, declaration: tsRef }
    pure $ (map seed d)
  else pure $ UnknownDeclaration
    { fullyQualifiedName: Nothing
    , msg: "Maximum recursion depth"
    }

build ∷ String → Effect (Array (Declaration Declaration'))
build fileName = do
  let
    onDeclaration = { interface: Interface, typeAlias: TypeAlias, unknown: UnknownDeclaration }
    onTypeNode =
      { anonymousObject: AnonymousObject
      , intersection: Intersection
      , primitive: case _ of
        "number" → Number
        "string" → String
        "boolean" → Boolean
        x → UnknownTypeNode ("Unknown primitive type:" <> x)
      , typeParameter: TypeParameter
      , typeReference: TypeReference
      , union: Union
      , unknown: UnknownTypeNode
      }
  { readDeclaration, topLevel } ← readDTS compilerOptions { onDeclaration, onTypeNode } fileName
  let
    u :: Seed → Effect Declaration'
    u = anaM (coalgebra readDeclaration)
  for topLevel \declaration →
    sequence $ map ({ declaration: _, level: 0} >>> u) declaration

type Repr = { fullyQualifiedName ∷ Maybe String, repr ∷ String }

pprintDeclaration ∷ Declaration Repr → Repr
pprintDeclaration (Interface i) =
  { fullyQualifiedName: Just (fqnToString i.fullyQualifiedName)
  , repr:
    "interface "
      <> show i.fullyQualifiedName
      <> " <" <> joinWith ", " (map pprintTypeNode i.typeParameters) <> "> : \n\t"
  }
  -- <> joinWith ";\n\t" (map onMember i.properties)
  -- where
  --  onMember r = joinWith " " [r.name, if r.optional then "?:" else ":", r.type.repr ]
pprintDeclaration _ = { repr: "Other declaration", fullyQualifiedName: Nothing }

pprintTypeNode ∷ TypeNode Repr → String
pprintTypeNode Boolean = "boolean"
pprintTypeNode (Intersection reprs) =
  joinWith " & " <<< map (pprintTypeNode) $ reprs
pprintTypeNode Number = "number"
pprintTypeNode String = "string"
pprintTypeNode (TypeParameter r) = case r.default of
  Just d → unsafeTsStringToString r.identifier <> "=" <> pprintTypeNode d
  Nothing → unsafeTsStringToString r.identifier
pprintTypeNode (Union reprs) =
  joinWith " | " <<< map (pprintTypeNode) $ reprs
pprintTypeNode _ = "Unfinished"


