module ReadDTS.AST where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), Except)
import Control.Monad.Free (Free, liftF)
import Data.Array (fold)
import Data.Array (zip) as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.FoldableWithIndex (foldMapDefault)
import Data.Functor.Mu (Mu)
import Data.Lens (Lens, over, traversed)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map (fromFoldable, insert, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe, optional)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (joinWith)
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (modify) as Ref
import Global.Unsafe (unsafeStringify)
import Matryoshka (class Corecursive, Algebra, CoalgebraM, GCoalgebraM, AlgebraM, anaM, embed, futuM)
import Matryoshka.Pattern.CoEnvT (CoEnvT(..))
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Lacks) as Row
import ReadDTS (FullyQualifiedName(..), OnType, TsDeclaration, Visit, compilerOptions, fqnToString, readDTS, unsafeTsStringToString)
import ReadDTS (OnDeclaration, OnType, TsString, unsafeTsStringToString) as ReadDTS
import Record (insert) as Record
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Property ref =
  { name ∷ String
  , type ∷ TypeNode ref
  , optional ∷ Boolean
  }

-- sequenceProperty ∷ ∀ ref t. Applicative t ⇒ Property (t ref) → t (Property ref)
sequenceProperty { name, type: t, optional } = { type: _, name, optional } <$> sequence t

type TypeParameter ref =
  { name ∷ String
  , default ∷ Maybe (TypeNode ref)
  }

sequenceTypeParameter ∷ ∀ ref t. Applicative t ⇒ TypeParameter (t ref) → t (TypeParameter ref)
sequenceTypeParameter { name, default: t } = { default: _, name } <$> (sequence <<< map sequence) t

newtype Var = Var String
derive instance eqVar ∷ Eq Var
derive instance ordVar ∷ Ord Var
printVar ∷ Var → String
printVar (Var s) = show s

newtype Application ref = Application
  { typeArguments ∷ Array (TypeNode ref)
  , declaration ∷ TypeConstructor ref
  }
derive instance functorApplication ∷ Functor Application

instance foldableApplication ∷ Foldable Application where
  foldMap f (Application d)
    = foldMap (foldMap f) d.typeArguments
    <> foldMap f d.declaration
  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableApplication ∷ Traversable Application where
  sequence (Application d@{ declaration, typeArguments }) = map Application
    $ (\b ta → d { declaration = b, typeArguments = ta })
    <$> (sequence declaration)
    <*> (sequence <<< map sequence) typeArguments
  traverse = traverseDefault

data TypeConstructor ref =
  Interface
    { fullyQualifiedName ∷ FullyQualifiedName
    , name ∷ String
    , properties ∷ Array (Property ref)
    , typeParameters ∷ Array (TypeParameter ref)
    }
  | TypeAlias
    { name ∷ String
    , type ∷ TypeNode ref
    , typeParameters ∷ Array (TypeParameter ref)
    }
  | UnknownTypeConstructor
    { fullyQualifiedName ∷ Maybe FullyQualifiedName
    , msg ∷ String
    }
derive instance functorTypeConstructor ∷ Functor TypeConstructor

instance foldableTypeConstructor ∷ Foldable TypeConstructor where
  foldMap f (Interface i)
    = foldMap (foldMap f <<< _.type) i.properties
    <> foldMap (foldMap (foldMap f) <<< _.default) i.typeParameters
  foldMap f (TypeAlias ta)
    = foldMap f ta.type
    <> foldMap (foldMap (foldMap f) <<< _.default) ta.typeParameters
  foldMap f (UnknownTypeConstructor _) = mempty

  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableTypeConstructor ∷ Traversable TypeConstructor where
  sequence (Interface i) = map Interface
    $ (\ms tp → i { properties = ms, typeParameters = tp })
    <$> (sequence <<< map sequenceProperty) i.properties
    <*> (sequence <<< map sequenceTypeParameter) i.typeParameters
  sequence (TypeAlias ta) = map TypeAlias $ { type: _, typeParameters: _, name: ta.name }
    <$> sequence ta.type
    <*> (sequence <<< map sequenceTypeParameter) ta.typeParameters
  sequence (UnknownTypeConstructor s) = pure $ (UnknownTypeConstructor s)
  traverse = traverseDefault

data TypeNode ref
  = AnonymousObject (Array (Property ref))
  | Array (TypeNode ref)
  | Boolean
  | Intersection (Array (TypeNode ref))
  | Number
  | String
  | Tuple (Array (TypeNode ref))
  | TypeReference ref
  | TypeParameter (TypeParameter ref)
  | Union (Array (TypeNode ref))
  | UnknownTypeNode String

derive instance functorTypeNode ∷ Functor TypeNode

instance foldableTypeNode ∷ Foldable TypeNode where
  foldMap f (AnonymousObject ts) = foldMap (foldMap f <<< _.type) ts
  foldMap f (Array t) = foldMap f t
  foldMap _ Boolean = mempty
  foldMap f (Intersection ts) = fold (map (foldMap f) ts)
  foldMap f Number = mempty
  foldMap f String = mempty
  foldMap f (Union ts) = fold (map (foldMap f) ts)
  foldMap f (Tuple ts) = fold (map (foldMap f) ts)
  foldMap f (TypeParameter { default }) = fold (map (foldMap f) default)
  foldMap f (TypeReference ref) = f ref
  foldMap f (UnknownTypeNode _) = mempty
  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableTypeNode ∷ Traversable TypeNode where
  sequence (AnonymousObject ts) = AnonymousObject <$> (sequence <<< map sequenceProperty) ts
  sequence (Array t) = Array <$> sequence t
  sequence Boolean = pure Boolean
  sequence (Intersection ts) = Intersection <$> (sequence <<< map sequence) ts
  sequence Number = pure $ Number
  sequence String = pure $ String
  sequence (Tuple ts) = Tuple <$> (sequence <<< map sequence) ts
  sequence (TypeParameter { name, default }) =
    TypeParameter <<< { name, default: _ } <$> (sequence <<< map sequence) default
  sequence (TypeReference ref) = TypeReference <$> ref
  sequence (Union ts) = Union <$> (sequence <<< map sequence) ts
  sequence (UnknownTypeNode s) = pure $ UnknownTypeNode s
  traverse = traverseDefault

newtype TsRef = TsRef
  { ref ∷ TsDeclaration
  , typeArguments ∷ Array (TypeNode TsRef)
  , fullyQualifiedName ∷ FullyQualifiedName
  }
derive instance newtypeTsRef ∷ Newtype TsRef _

-- | This simple `readTypeConstructor` extensions
-- | should allow us to provide cache for
-- | already loaded declarations
type ReadTypeConstructor = TsRef → Effect (TypeConstructor TsRef)

type Seed = { level ∷ Int, ref ∷ TsRef }

type Application' = Mu Application

coalgebra ∷ ReadTypeConstructor → CoalgebraM Effect Application Seed
coalgebra readApplication { level, ref: tsRef@(TsRef { fullyQualifiedName, typeArguments }) } = if level < 5
  then do
    d ← readApplication tsRef
    pure $ Application
      { declaration: (map seed d)
      , typeArguments: map seed <$> typeArguments
      }
  else pure $ Application
    { typeArguments: map seed <$> typeArguments
    , declaration: UnknownTypeConstructor
      { fullyQualifiedName: Just fullyQualifiedName
      , msg: "Maximum recursion depth"
      }
    }
  where
    seed = { level: level + 1, ref: _ }

visit ∷ Visit (TypeConstructor TsRef) (TypeNode TsRef)
visit =
  { onDeclaration:
    { interface: Interface <<< over (_typeParametersL <<< traversed <<< _nameL) unsafeTsStringToString
    , typeAlias: TypeAlias <<< over (_typeParametersL <<< traversed <<< _nameL) unsafeTsStringToString
    , unknown: UnknownTypeConstructor
    }
  , onTypeNode:
    { anonymousObject: AnonymousObject
    , array: Array
    , intersection: Intersection
    , primitive: case _ of
      "number" → Number
      "string" → String
      "boolean" → Boolean
      x → UnknownTypeNode ("Unknown primitive type:" <> x)
    , tuple: Tuple
    , typeParameter:
        let
          _name = prop (SProxy ∷ SProxy "name")
        in
          TypeParameter <<< over _name ReadDTS.unsafeTsStringToString
    , typeReference: TypeReference <<< TsRef
    , union: Union
    , unknown: UnknownTypeNode
    }
  }
  where
    _nameL ∷ ∀ a b r. Lens { name ∷ a | r } { name ∷ b | r } a b
    _nameL = prop (SProxy ∷ SProxy "name")

    _typeParametersL ∷ ∀ a b r. Lens { typeParameters ∷ a | r } { typeParameters ∷ b | r } a b
    _typeParametersL = prop (SProxy ∷ SProxy "typeParameters")

build ∷ String → Effect (Array (TypeConstructor Application'))
build fileName = do
  { readDeclaration, topLevel } ← readDTS compilerOptions visit fileName
  let
    u ∷ Seed → Effect Application'
    u = anaM (coalgebra (\(TsRef { ref }) → readDeclaration ref))
  for topLevel \declaration →
    sequence $ map ({ ref: _, level: 0} >>> u) declaration

type Apply a = Map String (TypeNode Void) → Except String (TypeNode Void)

applyApplication :: Application Apply -> Apply
applyApplication (Application { typeArguments, declaration }) = case declaration of
  Interface { properties, typeParameters } → \ctx → do
    typeArguments' ← traverse (flip applyTypeNode ctx) typeArguments
    let
      ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
      _typeL = prop (SProxy ∷ SProxy "type")
    AnonymousObject <$> for properties \{ name, type: t, optional } →
      { name, type: _, optional } <$> (flip applyTypeNode ctx' t)
  TypeAlias { type: t, typeParameters } → \ctx → do
    typeArguments' ← traverse (flip applyTypeNode ctx) typeArguments
    let
      ctx' = Map.fromFoldable (Array.zip (map _.name typeParameters) typeArguments')
    applyTypeNode t ctx'
  UnknownTypeConstructor r → const $ pure $ UnknownTypeNode
    ("Unknown type constructor: " <> show r.fullyQualifiedName)

applyTypeNode ∷ TypeNode Apply → Apply
applyTypeNode (AnonymousObject ps) ctx = AnonymousObject <$> for ps \{ name, type: t, optional } →
  { name, type: _, optional } <$> (flip applyTypeNode ctx t)
applyTypeNode (Array t) ctx = Array <$> applyTypeNode t ctx
applyTypeNode Boolean ctx = pure Boolean
applyTypeNode (Intersection ts) ctx = Intersection <$> traverse (flip applyTypeNode ctx) ts
applyTypeNode Number ctx = pure Number
applyTypeNode String ctx = pure $ String
applyTypeNode (TypeParameter { name, default }) ctx = case Map.lookup name ctx, default of
  Just t, _ → pure t
  Nothing, Just d → applyTypeNode d ctx
  _, _ → throwError ("Variable not defined: " <> name)
applyTypeNode (Union ts) ctx = Union <$> traverse (flip applyTypeNode ctx) ts
applyTypeNode (Tuple ts) ctx = Tuple <$> traverse (flip applyTypeNode ctx) ts
applyTypeNode (TypeReference ref) ctx = unsafeCrashWith ("Matching void case (applyTypeNode): " <> unsafeStringify ref)
applyTypeNode (UnknownTypeNode s) ctx = pure $ UnknownTypeNode s


-- -- type Repr = { fullyQualifiedName ∷ Maybe String, repr ∷ String }
-- -- 
-- -- pprintApplication ∷ Application Repr → Repr
-- -- pprintApplication (Application { typeArguments, declaration }) = case declaration of
-- --   Interface { fullyQualifiedName } ->
-- --     fullyQualifiedName <> args
-- --   TypeAlias { name } ->
-- --     name <> args
-- --   UnknownTypeConstructor { fullyQualifiedName } → case fullyQualifiedName of
-- --     Just fqn → "unknown: " <> fqn
-- --     Nothing → "unknown:?"
-- --   where
-- --     args = "<" <> joinWith ", " map pprintType typeArguments <> ">"
-- -- 
-- -- 
-- -- pprintTypeConstructor ∷ TypeConstructor Repr → Repr
-- -- pprintTypeConstructor (Interface =
-- -- 
-- -- 
-- --   { fullyQualifiedName: Just (fqnToString i.fullyQualifiedName)
-- --   , repr:
-- --     "interface "
-- --       <> show i.fullyQualifiedName
-- --       <> " <" <> joinWith ", " (map pprintTypeNode i.typeParameters) <> "> : \n\t"
-- --   }
-- --   -- <> joinWith ";\n\t" (map onMember i.properties)
-- --   -- where
-- --   --  onMember r = joinWith " " [r.name, if r.optional then "?:" else ":", r.type.repr ]
-- -- pprintApplication _ = { repr: "Other declaration", fullyQualifiedName: Nothing }
-- -- 
-- -- 
-- -- pprintTypeNode ∷ TypeNode Repr → String
-- -- pprintTypeNode Boolean = "boolean"
-- -- pprintTypeNode (Intersection reprs) =
-- --   joinWith " & " <<< map (pprintTypeNode) $ reprs
-- -- pprintTypeNode Number = "number"
-- -- pprintTypeNode String = "string"
-- -- pprintTypeNode (TypeParameter r) = case r.default of
-- --   Just d → unsafeTsStringToString r.identifier <> "=" <> pprintTypeNode d
-- --   Nothing → unsafeTsStringToString r.identifier
-- -- pprintTypeNode (Union reprs) =
-- --   joinWith " | " <<< map (pprintTypeNode) $ reprs
-- -- pprintTypeNode _ = "Unfinished"
-- -- 
-- -- 
