module ReadDTS.AST where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (gets, modify_)
import Data.Array (catMaybes)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault, fold)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map (empty, insert, lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Matryoshka (CoalgebraM, anaM)
import ReadDTS (FullyQualifiedName(..), Options, TsDeclaration, Visit)
import ReadDTS (Param, Prop, Props, sequenceProp) as ReadDTS
import Type.Prelude (SProxy(..))
import TypeScript.Compiler.Types (Typ) as TS

type Prop decl ref = ReadDTS.Prop (TypeNode decl ref)

type Param decl ref = ReadDTS.Param Maybe (TypeNode decl ref)

data TypeNode decl ref
  = Any
  -- | Application (ReadDTS.Application ref)
  -- | Array ref
  -- | Boolean
  -- -- | In typescript this type level is
  -- -- | mixed up with value level in declarations.
  -- -- | For example this ts union:
  -- -- | `'a' | 'b' | 8`
  -- -- | is going to be read as:
  -- -- | `Union [StringLiteral "a", StringLiteral "b", NumberLiteral 8]`
  -- | BooleanLiteral Boolean
  -- | Class (ReadDTS.Props ref)
  -- | Function (ReadDTS.Function ref)
  -- | Interface (Array (ReadDTS.Prop ref))
  -- | Intersection (Array ref)
  -- | Null
  -- | Number
  -- | NumberLiteral Number
  | Object (Array (ReadDTS.Prop ref))
  -- | String
  -- | StringLiteral String
  -- | Tuple (Array (TypeNode ref))
  -- | TypeParameter (TypeParameter ref)
  | TypeRef decl

-- | Undefined
-- | Union (Array (TypeNode ref))
-- | UnknownTypeNode String
-- | Void

derive instance functorTypeNode :: Functor (TypeNode decl)
derive instance eqTypeNode :: (Eq decl, Eq ref) => Eq (TypeNode decl ref)
derive instance (Ord decl, Ord ref) => Ord (TypeNode decl ref)
derive instance genericTypeNode :: Generic (TypeNode decl ref) _
instance showTypeNode :: (Show decl, Show ref) => Show (TypeNode decl ref) where
  show s = genericShow s

instance foldableTypeNode :: Foldable (TypeNode decl) where
  foldMap _ Any = mempty
  -- foldMap f (Array t) = foldMap f t
  -- foldMap _ Boolean = mempty
  -- foldMap f (Function r)
  --   = foldMap (foldMap f <<< _.type) r.parameters
  --   <> foldMap f r.returnType
  -- -- foldMap f (Intersection ts) = fold (map (foldMap f) ts)
  -- foldMap f (Intersection ts) = A.fold (map (foldMap f) ts)
  -- foldMap f (Interface ts) = foldMap (foldMap f <<< _.type) ts
  -- foldMap _ Number = mempty
  -- foldMap _ String = mempty
  -- foldMap f (Tuple ts) = A.fold (map (foldMap f) ts)
  -- foldMap f (Application { constructor, params }) = f constructor <> foldMap (foldMap f) params
  -- foldMap f (TypeParameter { default }) = fold (map (foldMap f) default)
  -- foldMap _ (BooleanLiteral _) = mempty
  -- foldMap _ (NumberLiteral _) = mempty
  foldMap f (Object ts) = foldMap (f <<< _.type) ts
  -- foldMap _ Null = mempty
  -- foldMap _ (StringLiteral _) = mempty
  foldMap _ (TypeRef _) = mempty
  -- foldMap _ Undefined = mempty
  -- foldMap f (Union ts) = A.fold (map (foldMap f) ts)
  -- foldMap _ (UnknownTypeNode _) = mempty
  -- foldMap _ Void = mempty

  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance traversableTypeNode :: Traversable (TypeNode decl) where
  sequence Any = pure Any
  --   sequence (Application { constructor, params }) =
  --     map Application $ { constructor: _, params: _ } <$> constructor <*> sequence (map sequence params)
  --   sequence (Array t) = Array <$> sequence t
  --   sequence Boolean = pure Boolean
  --   sequence (BooleanLiteral n) = pure $ BooleanLiteral n
  --   sequence (Function r) = map Function
  --     $ { parameters: _, returnType: _ }
  --     <$> traverse sequenceParameter r.parameters
  --     <*> sequence r.returnType
  --     where
  --       sequenceParameter { name, "type": t } = { name, "type": _ } <$> sequence t
  --   sequence (Interface ts) = Interface <$> (sequence <<< map ReadDTS.sequenceProp) ts
  --   sequence (Intersection ts) = Intersection <$> (sequence <<< map sequence) ts
  --   sequence Null = pure Null
  --   sequence Number = pure $ Number
  --   sequence (NumberLiteral n) = pure $ NumberLiteral n
  sequence (Object props) = map Object $ sequence $ map ReadDTS.sequenceProp props
  --   sequence String = pure $ String
  --   sequence (Tuple ts) = Tuple <$> (sequence <<< map sequence) ts
  --   sequence (TypeParameter { name, default }) =
  --     TypeParameter <<< { name, default: _ } <$> (sequence <<< map sequence) default
  --   sequence (StringLiteral s) = pure $ StringLiteral s
  sequence (TypeRef fqn) = pure $ TypeRef fqn
  --   sequence Undefined = pure Undefined
  --   sequence (Union ts) = Union <$> (sequence <<< map sequence) ts
  --   sequence (UnknownTypeNode s) = pure $ UnknownTypeNode s
  --   sequence Void = pure Void
  traverse = traverseDefault

-- newtype TypeDeclaration ref = TypeDeclaration
--   { fullyQualifiedName ∷ FullyQualifiedName
--   , "type" ∷ TypeNode FullyQualifiedName ref
--   }
-- 
-- derive instance functorTypeDeclaration ∷ Functor TypeDeclaration
-- derive instance Eq ref ⇒ Eq (TypeDeclaration ref)
-- derive instance Ord ref ⇒ Ord (TypeDeclaration ref)
-- derive instance genericTypeDeclaration ∷ Generic (TypeDeclaration ref) _
-- instance showTypeDeclaration ∷ (Show ref) ⇒ Show (TypeDeclaration ref) where
--   show s = genericShow s
-- 
-- instance foldableTypeDeclaration ∷ Foldable TypeDeclaration where
--   foldMap f (TypeDeclaration { type: t }) = foldMap f t
--   foldr f t = foldrDefault f t
--   foldl f t = foldlDefault f t
-- 
-- instance traversableTypeDeclaration ∷ Traversable TypeDeclaration where
--   sequence (TypeDeclaration { fullyQualifiedName, type: t }) =
--     TypeDeclaration <<< { fullyQualifiedName, type: _ } <$> sequence t
--   traverse = traverseDefault

type KnownDeclarations decl = Map FullyQualifiedName (Mu (TypeNode decl))

type TsRef =
  { ref :: TsDeclaration
  , fullyQualifiedName :: FullyQualifiedName
  }

type VisitMonad a = ExceptT (Array String) Effect a

type ReadTsTypeNode = TS.Typ () -> VisitMonad (TypeNode TsRef (TS.Typ ()))

type Seed = { level :: Int, ref :: TS.Typ () }

coalgebra :: ReadTsTypeNode -> CoalgebraM VisitMonad (TypeNode TsRef) Seed
coalgebra readTsTypeNode { level, ref } =
  if level < 10 then do
    td <- readTsTypeNode ref
    pure $ map seed td
  else
    throwError [ "Maximum recursion depth in ReadDTS.AST.visitor coalgebra" ]
  where
  seed = { level: level + 1, ref: _ }

-- type ReadDeclaration = TsDeclaration → Effect (TypeDeclaration TsRef)
-- 
-- type ReadTs = { readDeclaration ∷ ReadDeclaration, readType ∷ ReadType }
-- 
-- 
-- -- -- | TODO: rename to `visitor`
-- -- visitor ∷ Visit (TypeDeclaration TsTypeNode) (TypeNode TsTypeNode)
-- -- visitor =
-- --   { onDeclaration: \fqn t → TypeDeclaration { fullyQualifiedName: fqn, type: t }
-- --   , onTypeNode:
-- --     { any: Any
-- --     -- , array: Array
-- --     -- , function: Function
-- --     -- , intersection: Intersection
-- --     -- , interface: Interface
-- --     -- , primitive: case _ of
-- --     --     "any" → Any
-- --     --     "boolean" → Boolean
-- --     --     "null" → Null
-- --     --     "number" → Number
-- --     --     "string" → String
-- --     --     "undefined" → Undefined
-- --     --     "void" → Void
-- --     --     x → UnknownTypeNode ("Unknown primitive type:" <> x)
-- --     -- , tuple: Tuple
-- --     -- , typeParameter: TypeParameter
-- --     -- , typeReference: \{ fullyQualifiedName, ref, typeArguments } →
-- --     --     Application { constructor: { fullyQualifiedName, ref }, params: typeArguments }
-- --     -- , booleanLiteral: BooleanLiteral
-- --     -- , numberLiteral: NumberLiteral
-- --     , object: Object
-- --     -- , stringLiteral: StringLiteral
-- --     -- , union: Union
-- --     -- , unknown: UnknownTypeNode
-- --     }
-- --   }
-- --   where
-- --     _nameL ∷ ∀ a b r. Lens { name ∷ a | r } { name ∷ b | r } a b
-- --     _nameL = prop (SProxy ∷ SProxy "name")
-- -- 
-- -- --     _typeParametersL ∷ ∀ a b r. Lens { typeParameters ∷ a | r } { typeParameters ∷ b | r } a b
-- -- --     _typeParametersL = prop (SProxy ∷ SProxy "typeParameters")
-- -- -- 
-- -- type Declarations = Map FullyQualifiedName (TypeDeclaration FullyQualifiedName)
-- -- 
-- -- visit
-- --   ∷ Options
-- --   → Array String
-- --   → Array ReadDTS.InMemoryFile
-- --   → TypeScript.CompilerHost
-- --   → Effect
-- --     (Either
-- --       (Array String)
-- --       { topLevel ∷ Array FullyQualifiedName, declarations ∷ Declarations }
-- --     )
-- -- visit compilerOptions rootNames inMemoryFiles host = do
-- --   readDTS compilerOptions visitor rootNames inMemoryFiles host >>= case _ of
-- --     Right { readDeclaration, topLevel } → do
-- --       let
-- --         unfold ∷ Seed → _ (Mu _)
-- --         unfold = anaM (coalgebra readDeclaration)
-- --         toRes (topLevelFqns /\ declarations) = { topLevel: _, declarations } <$> topLevelFqns
-- --       map toRes <$> flip runStateT Map.empty $ runExceptT $ do
-- --         for topLevel \td@(TypeDeclaration d) → do
-- --           let
-- --             seed = { level: 1, ref: _ }
-- --             td' = map seed td
-- --           modify_ (Map.insert d.fullyQualifiedName (map (const d.fullyQualifiedName) td))
-- --           void $ traverse unfold td'
-- --           pure d.fullyQualifiedName
-- --     Left err → pure (Left err)
-- -- 
