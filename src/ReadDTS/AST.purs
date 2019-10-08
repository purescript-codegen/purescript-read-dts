module ReadDTS.AST where

import Prelude

import Data.Array (fold)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens, over, traversed)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Effect (Effect)
import Matryoshka (CoalgebraM, anaM)
import ReadDTS (File, unsafeTsStringToString) as ReadDTS
import ReadDTS (FullyQualifiedName, TsDeclaration, Visit, CompilerOptions, readDTS, unsafeTsStringToString)
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
-- | in the `TypeNode` `ApplicationWithRef` constructor.
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
derive instance genericApplication ∷ Generic (Application ref) _

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
derive instance genericTypeConstructor ∷ Generic (TypeConstructor ref) _

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
  = AnonymousObject
      FullyQualifiedName
      (Array (Property ref))
  | Any
  | ApplicationWithRef ref
  | Array (TypeNode ref)
  | Boolean
  | Intersection (Array (TypeNode ref))
  | Number
  | String
  | Tuple (Array (TypeNode ref))
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
  | Null
  | Undefined
  | Union (Array (TypeNode ref))
  | UnknownTypeNode String

derive instance functorTypeNode ∷ Functor TypeNode
derive instance genericTypeNode ∷ Generic (TypeNode ref) _

instance foldableTypeNode ∷ Foldable TypeNode where
  foldMap f (AnonymousObject _ ts) = foldMap (foldMap f <<< _.type) ts
  foldMap _ Any = mempty
  foldMap f (Array t) = foldMap f t
  foldMap _ Boolean = mempty
  foldMap f (Intersection ts) = fold (map (foldMap f) ts)
  foldMap f Number = mempty
  foldMap f String = mempty
  foldMap f (Tuple ts) = fold (map (foldMap f) ts)
  foldMap f (ApplicationWithRef ref) = f ref
  foldMap f (TypeParameter { default }) = fold (map (foldMap f) default)
  foldMap _ (BooleanLiteral _) = mempty
  foldMap _ (NumberLiteral _) = mempty
  foldMap _ Null = mempty
  foldMap _ (StringLiteral _) = mempty
  foldMap _ Undefined = mempty
  foldMap f (Union ts) = fold (map (foldMap f) ts)
  foldMap f (UnknownTypeNode _) = mempty
  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableTypeNode ∷ Traversable TypeNode where
  sequence (AnonymousObject fqn ts) = AnonymousObject fqn <$> (sequence <<< map sequenceProperty) ts
  sequence Any = pure Any
  sequence (ApplicationWithRef ref) = ApplicationWithRef <$> ref
  sequence (Array t) = Array <$> sequence t
  sequence Boolean = pure Boolean
  sequence (BooleanLiteral n) = pure $ BooleanLiteral n
  sequence (Intersection ts) = Intersection <$> (sequence <<< map sequence) ts
  sequence Null = pure Null
  sequence Number = pure $ Number
  sequence (NumberLiteral n) = pure $ NumberLiteral n
  sequence String = pure $ String
  sequence (Tuple ts) = Tuple <$> (sequence <<< map sequence) ts
  sequence (TypeParameter { name, default }) =
    TypeParameter <<< { name, default: _ } <$> (sequence <<< map sequence) default
  sequence (StringLiteral s) = pure $ StringLiteral s
  sequence Undefined = pure Undefined
  sequence (Union ts) = Union <$> (sequence <<< map sequence) ts
  sequence (UnknownTypeNode s) = pure $ UnknownTypeNode s
  traverse = traverseDefault

-- | We need this `newtype` here because
-- | `typeArguments` contain recursive
-- | reference.
newtype ApplicationRef = ApplicationRef
  { ref ∷ TsDeclaration
  , typeArguments ∷ Array (TypeNode ApplicationRef)
  , fullyQualifiedName ∷ FullyQualifiedName
  }
derive instance newtypeApplicationRef ∷ Newtype ApplicationRef _

type ReadDeclaration = ApplicationRef → Effect (TypeConstructor ApplicationRef)

type Seed = { level ∷ Int, ref ∷ ApplicationRef }

type Application' = Mu Application

-- XXX:
-- * Parametrize by this maxLevel value
-- * Accumulate `path ∷ Set FullyQualifiedName` and detect cycles
-- * How to detect recurssion in the case of type aliases?
coalgebra ∷ ReadDeclaration → CoalgebraM Effect Application Seed
coalgebra readDeclaration { level, ref: tsRef@(ApplicationRef { fullyQualifiedName, typeArguments }) } =
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

visit ∷ Visit (TypeConstructor ApplicationRef) (TypeNode ApplicationRef)
visit =
  { onDeclaration:
    { interface: Interface <<< over (_typeParametersL <<< traversed <<< _nameL) unsafeTsStringToString
    , typeAlias: TypeAlias <<< over (_typeParametersL <<< traversed <<< _nameL) unsafeTsStringToString
    , unknown: UnknownTypeConstructor
    }
  , onTypeNode:
    { anonymousObject:
      \r → AnonymousObject r.fullyQualifiedName r.properties
    , array: Array
    , intersection: Intersection
    , primitive: case _ of
        "any" → Any
        "boolean" → Boolean
        "null" → Null
        "number" → Number
        "string" → String
        "undefined" → Undefined
        x → UnknownTypeNode ("Unknown primitive type:" <> x)
    , tuple: Tuple
    , typeParameter:
        let _name = prop (SProxy ∷ SProxy "name") in
        TypeParameter <<< over _name ReadDTS.unsafeTsStringToString
    , typeReference: ApplicationWithRef <<< ApplicationRef
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

build ∷ CompilerOptions → ReadDTS.File → Effect (Either (Array String) (Array (TypeConstructor Application')))
build compilerOptions file = do
  readDTS compilerOptions visit file >>= case _ of
    Right { readDeclaration, topLevel } → do
      let
        go ∷ Seed → Effect Application'
        go = anaM $ coalgebra \(ApplicationRef { ref }) → readDeclaration ref
      Right <$> for topLevel \typeConstructor →
        traverse ({ ref: _, level: 0} >>> go) typeConstructor
    Left err → pure (Left err)

