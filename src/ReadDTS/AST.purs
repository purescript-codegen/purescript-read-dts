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
import ReadDTS (Options, TsDeclaration, Visit)
import ReadDTS (Param, Prop, Props, sequenceProp) as ReadDTS
import Type.Prelude (SProxy(..))
import TypeScript.Compiler.Types (FullyQualifiedName(..))
import TypeScript.Compiler.Types (Typ) as TS
import Unsafe.Reference (unsafeRefEq)

type Prop decl ref = ReadDTS.Prop (TsType decl ref)

type Param decl ref = ReadDTS.Param Maybe (TsType decl ref)

-- | We want this wrapper so we are able to provide basic instances
-- | for it.
-- | Some of them are just required for testing purposes.
newtype TSTyp = TSTyp (TS.Typ ())
instance Show TSTyp where
  show (TSTyp _) = "TSTyp"
instance Eq TSTyp where
  eq (TSTyp t1) (TSTyp t2) = unsafeRefEq t1 t2

data TsType decl ref
  = TsAny
  -- | Application (ReadDTS.Application ref)
  | TsArray ref
  | TsBoolean
  | TsBooleanLiteral Boolean
  -- -- | In typescript this type level is
  -- -- | mixed up with value level in declarations.
  -- -- | For example this ts union:
  -- -- | `'a' | 'b' | 8`
  -- -- | is going to be read as:
  -- -- | `Union [StringLiteral "a", StringLiteral "b", TsNumberLiteral 8]`
  -- | Class (ReadDTS.Props ref)
  -- | Function (ReadDTS.Function ref)
  -- | Interface (Array (ReadDTS.Prop ref))
  -- | Intersection (Array ref)
  | TsNull
  | TsNumber
  | TsNumberLiteral Number
  | TsObject (Array (ReadDTS.Prop ref))
  | TsString
  | TsStringLiteral String
  -- | Tuple (Array (TsType ref))
  -- | TypeParameter (TypeParameter ref)
  | TsTypeRef decl
  | TsUndefined
  | TsUnknown TSTyp
-- | Union (Array (TsType ref))
-- | Void

derive instance functorTsType :: Functor (TsType decl)
derive instance eqTsType :: (Eq decl, Eq ref) => Eq (TsType decl ref)
-- derive instance (Ord decl, Ord ref) => Ord (TsType decl ref)
derive instance genericTsType :: Generic (TsType decl ref) _
instance showTsType :: (Show decl, Show ref) => Show (TsType decl ref) where
  show s = genericShow s

instance Foldable (TsType decl) where
  foldMap _ TsAny = mempty
  foldMap f (TsArray t) = f t
  foldMap _ TsBoolean = mempty
  foldMap _ (TsBooleanLiteral _) = mempty
  -- foldMap f (Function r)
  --   = foldMap (foldMap f <<< _.type) r.parameters
  --   <> foldMap f r.returnType
  -- -- foldMap f (Intersection ts) = fold (map (foldMap f) ts)
  -- foldMap f (Intersection ts) = A.fold (map (foldMap f) ts)
  -- foldMap f (Interface ts) = foldMap (foldMap f <<< _.type) ts
  -- foldMap f (Tuple ts) = A.fold (map (foldMap f) ts)
  -- foldMap f (Application { constructor, params }) = f constructor <> foldMap (foldMap f) params
  -- foldMap f (TypeParameter { default }) = fold (map (foldMap f) default)
  foldMap _ TsNull = mempty
  foldMap _ TsNumber = mempty
  foldMap _ (TsNumberLiteral _) = mempty
  foldMap f (TsObject ts) = foldMap (f <<< _.type) ts
  foldMap _ TsString = mempty
  foldMap _ (TsStringLiteral _) = mempty
  foldMap _ (TsTypeRef _) = mempty
  foldMap _ TsUndefined = mempty
  -- foldMap f (Union ts) = A.fold (map (foldMap f) ts)
  foldMap _ (TsUnknown _) = mempty
  -- foldMap _ Void = mempty

  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance Traversable (TsType decl) where
  sequence TsAny = pure TsAny
  --   sequence (Application { constructor, params }) =
  --     map Application $ { constructor: _, params: _ } <$> constructor <*> sequence (map sequence params)
  sequence (TsArray t) = TsArray <$> t
  sequence TsBoolean = pure TsBoolean
  sequence (TsBooleanLiteral b) = pure $ TsBooleanLiteral b
  --   sequence (Function r) = map Function
  --     $ { parameters: _, returnType: _ }
  --     <$> traverse sequenceParameter r.parameters
  --     <*> sequence r.returnType
  --     where
  --       sequenceParameter { name, "type": t } = { name, "type": _ } <$> sequence t
  --   sequence (Interface ts) = Interface <$> (sequence <<< map ReadDTS.sequenceProp) ts
  --   sequence (Intersection ts) = Intersection <$> (sequence <<< map sequence) ts
  sequence TsNull = pure TsNull
  sequence TsNumber = pure $ TsNumber
  sequence (TsNumberLiteral n) = pure $ TsNumberLiteral n
  sequence (TsObject props) = map TsObject $ sequence $ map ReadDTS.sequenceProp props
  --   sequence (Tuple ts) = Tuple <$> (sequence <<< map sequence) ts
  --   sequence (TypeParameter { name, default }) =
  --     TypeParameter <<< { name, default: _ } <$> (sequence <<< map sequence) default
  sequence TsString = pure $ TsString
  sequence (TsStringLiteral s) = pure $ TsStringLiteral s
  sequence (TsTypeRef fqn) = pure $ TsTypeRef fqn
  sequence TsUndefined = pure TsUndefined
  --   sequence (Union ts) = Union <$> (sequence <<< map sequence) ts
  sequence (TsUnknown t) = pure $ TsUnknown t
  --   sequence Void = pure Void
  traverse = traverseDefault

-- newtype TypeDeclaration ref = TypeDeclaration
--   { fullyQualifiedName ∷ FullyQualifiedName
--   , "type" ∷ TsType FullyQualifiedName ref
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

type KnownDeclarations decl = Map FullyQualifiedName (Mu (TsType decl))

type TsRef =
  { ref :: TsDeclaration
  , fullyQualifiedName :: FullyQualifiedName
  }

type VisitMonad a = ExceptT (Array String) Effect a

type ReadTsTsType = TS.Typ () -> VisitMonad (TsType TsRef (TS.Typ ()))

type Seed = { level :: Int, ref :: TS.Typ () }

coalgebra :: ReadTsTsType -> CoalgebraM VisitMonad (TsType TsRef) Seed
coalgebra readTsTsType { level, ref } =
  if level < 10 then do
    td <- readTsTsType ref
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
-- -- visitor ∷ Visit (TypeDeclaration TsTsType) (TsType TsTsType)
-- -- visitor =
-- --   { onDeclaration: \fqn t → TypeDeclaration { fullyQualifiedName: fqn, type: t }
-- --   , onTsType:
-- --     { any: TsAny
-- --     -- , array: Array
-- --     -- , function: Function
-- --     -- , intersection: Intersection
-- --     -- , interface: Interface
-- --     -- , primitive: case _ of
-- --     --     "any" → TsAny
-- --     --     "boolean" → Boolean
-- --     --     "null" → TsNull
-- --     --     "number" → TsNumber
-- --     --     "string" → String
-- --     --     "undefined" → TsUndefined
-- --     --     "void" → Void
-- --     --     x → TsUnknownTsType ("TsUnknown primitive type:" <> x)
-- --     -- , tuple: Tuple
-- --     -- , typeParameter: TypeParameter
-- --     -- , typeReference: \{ fullyQualifiedName, ref, typeArguments } →
-- --     --     Application { constructor: { fullyQualifiedName, ref }, params: typeArguments }
-- --     -- , booleanLiteral: BooleanLiteral
-- --     -- , numberLiteral: TsNumberLiteral
-- --     , object: TsObject
-- --     -- , stringLiteral: StringLiteral
-- --     -- , union: Union
-- --     -- , unknown: TsUnknownTsType
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
