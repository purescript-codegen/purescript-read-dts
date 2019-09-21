module ReadDTS.AST where

import Prelude

import Data.Array (fold)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Mu (Mu)
import Data.Lens (Lens, over, traversed)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Effect (Effect)
import Global.Unsafe (unsafeStringify)
import Matryoshka (CoalgebraM, anaM)
import ReadDTS (FullyQualifiedName, TsDeclaration, Visit, compilerOptions, fqnToString, readDTS, unsafeTsStringToString)
import ReadDTS (unsafeTsStringToString) as ReadDTS
import Type.Prelude (SProxy(..))

type Property ref =
  { name ∷ String
  , type ∷ TypeNode ref
  , optional ∷ Boolean
  }

sequenceProperty ∷ ∀ ref t. Applicative t ⇒ Property (t ref) → t (Property ref)
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

-- | * Recursion of this type closes down
-- | in the `TypeNode` `TypeApplication` constructor.
-- |
-- | * FFI provides us `TsDeclaration` reference which
-- | we are able to exapand using provided by FFI
-- | `readDeclaration` effect. This is a basis for
-- | our unfolding scheme in `build` function.
newtype Application ref = Application
  { typeArguments ∷ Array (TypeNode ref)
  , typeConstructor ∷ TypeConstructor ref
  }
derive instance functorApplication ∷ Functor Application

instance foldableApplication ∷ Foldable Application where
  foldMap f (Application d)
    = foldMap (foldMap f) d.typeArguments
    <> foldMap f d.typeConstructor
  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableApplication ∷ Traversable Application where
  sequence (Application d@{ typeConstructor, typeArguments }) = map Application
    $ (\tc ta → d { typeConstructor = tc, typeArguments = ta })
    <$> (sequence typeConstructor)
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
  | Any
  | Array (TypeNode ref)
  | Boolean
  | Intersection (Array (TypeNode ref))
  | Number
  | String
  | Tuple (Array (TypeNode ref))
  | TypeApplication ref
  | TypeParameter (TypeParameter ref)
  -- | In typescript this type level is
  -- | mixed up with value level in declarations.
  -- | For example this ts union:
  -- | `'a' | 'b' | 8`
  -- | is going to be read as:
  -- | `Union [StringLiteral "a", StringLiteral "b", NumberLiteral 8]`
  | BooleanLiteral Boolean
  | StringLiteral String
  | NumberLiteral Number
  | Union (Array (TypeNode ref))
  | UnknownTypeNode String

derive instance functorTypeNode ∷ Functor TypeNode

instance foldableTypeNode ∷ Foldable TypeNode where
  foldMap f (AnonymousObject ts) = foldMap (foldMap f <<< _.type) ts
  foldMap _ Any = mempty
  foldMap f (Array t) = foldMap f t
  foldMap _ Boolean = mempty
  foldMap f (Intersection ts) = fold (map (foldMap f) ts)
  foldMap f Number = mempty
  foldMap f String = mempty
  foldMap f (Tuple ts) = fold (map (foldMap f) ts)
  foldMap f (TypeApplication ref) = f ref
  foldMap f (TypeParameter { default }) = fold (map (foldMap f) default)
  foldMap _ (BooleanLiteral _) = mempty
  foldMap _ (NumberLiteral _) = mempty
  foldMap _ (StringLiteral _) = mempty
  foldMap f (Union ts) = fold (map (foldMap f) ts)
  foldMap f (UnknownTypeNode _) = mempty
  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableTypeNode ∷ Traversable TypeNode where
  sequence (AnonymousObject ts) = AnonymousObject <$> (sequence <<< map sequenceProperty) ts
  sequence Any = pure Any
  sequence (Array t) = Array <$> sequence t
  sequence Boolean = pure Boolean
  sequence (Intersection ts) = Intersection <$> (sequence <<< map sequence) ts
  sequence Number = pure $ Number
  sequence String = pure $ String
  sequence (Tuple ts) = Tuple <$> (sequence <<< map sequence) ts
  sequence (TypeParameter { name, default }) =
    TypeParameter <<< { name, default: _ } <$> (sequence <<< map sequence) default
  sequence (TypeApplication ref) = TypeApplication <$> ref
  sequence (BooleanLiteral n) = pure $ BooleanLiteral n
  sequence (NumberLiteral n) = pure $ NumberLiteral n
  sequence (StringLiteral s) = pure $ StringLiteral s
  sequence (Union ts) = Union <$> (sequence <<< map sequence) ts
  sequence (UnknownTypeNode s) = pure $ UnknownTypeNode s
  traverse = traverseDefault

newtype TsRef = TsRef
  { ref ∷ TsDeclaration
  , typeArguments ∷ Array (TypeNode TsRef)
  , fullyQualifiedName ∷ FullyQualifiedName
  }
derive instance newtypeTsRef ∷ Newtype TsRef _

type ReadDeclaration = TsRef → Effect (TypeConstructor TsRef)

type Seed = { level ∷ Int, ref ∷ TsRef }

type Application' = Mu Application

-- | XXX: Of course we should parametrize by this maxLevel value ;-)
coalgebra ∷ ReadDeclaration → CoalgebraM Effect Application Seed
coalgebra readDeclaration { level, ref: tsRef@(TsRef { fullyQualifiedName, typeArguments }) } =
  if level < 5
  then do
    d ← readDeclaration tsRef
    pure $ Application
      { typeConstructor: map seed d
      , typeArguments: map seed <$> typeArguments
      }
  else pure $ Application
    { typeArguments: map seed <$> typeArguments
    , typeConstructor: UnknownTypeConstructor
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
        "any" → Any
        "boolean" → Boolean
        "number" → Number
        "string" → String
        x → UnknownTypeNode ("Unknown primitive type:" <> x)
    , tuple: Tuple
    , typeParameter:
        let _name = prop (SProxy ∷ SProxy "name") in
        TypeParameter <<< over _name ReadDTS.unsafeTsStringToString
    , typeReference: TypeApplication <<< TsRef
    , booleanLiteral: BooleanLiteral
    , numberLiteral: NumberLiteral
    , stringLiteral: StringLiteral
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
    go ∷ Seed → Effect Application'
    go = anaM $ coalgebra \(TsRef { ref }) → readDeclaration ref
  for topLevel \typeConstructor →
    traverse ({ ref: _, level: 0} >>> go) typeConstructor

type Repr = { fullyQualifiedName ∷ Maybe String, repr ∷ String }

pprintApplication ∷ Application Repr → Repr
pprintApplication (Application { typeArguments, typeConstructor }) =
  let
    args = joinWith ", " $ map pprintTypeNode typeArguments
    r = pprintTypeConstructor typeConstructor
  in
    r { repr = r.repr <> "<" <> args <> ">" }
  -- case declaration of
  -- Interface { fullyQualifiedName } ->
  --   fullyQualifiedName <> args
  -- TypeAlias { name } ->
  --   name <> args
  -- UnknownTypeConstructor { fullyQualifiedName } → case fullyQualifiedName of
  --   Just fqn → "unknown: " <> fqn
  --   Nothing → "unknown:?"
  -- where
  --   args = "<" <> joinWith ", " map pprintTypeNode typeArguments <> ">"

pprintTypeConstructor ∷ TypeConstructor Repr → Repr
pprintTypeConstructor = case _ of
  Interface { fullyQualifiedName, name, properties, typeParameters } →
    { fullyQualifiedName: Just $ fqnToString fullyQualifiedName
    , repr: 
        "interface "
          <> show fullyQualifiedName <> " <"
          <> joinWith ", " (map pprintTypeParameter typeParameters) <> "> : \n\t"
          <> joinWith ";\n\t" (map onMember properties)
    }
  t → { fullyQualifiedName: Nothing, repr: unsafeStringify t }
  where
    onMember ∷ Property Repr → String
    onMember r = joinWith " " [r.name, if r.optional then "?:" else ":", pprintTypeNode r.type ]
-- pprintApplication _ = { repr: "Other declaration", fullyQualifiedName: Nothing }

pprintTypeParameter ∷ TypeParameter Repr → String
pprintTypeParameter { name, default } = case default of
      Just d → name <> "=" <> pprintTypeNode d
      Nothing → name

pprintTypeNode ∷ TypeNode Repr → String
pprintTypeNode Boolean = "boolean"
pprintTypeNode (Intersection reprs) =
  joinWith " & " <<< map (pprintTypeNode) $ reprs
pprintTypeNode Number = "number"
pprintTypeNode String = "string"
pprintTypeNode (TypeParameter r) = pprintTypeParameter r
pprintTypeNode (Union reprs) =
  joinWith " | " <<< map (pprintTypeNode) $ reprs
pprintTypeNode t = unsafeStringify t
