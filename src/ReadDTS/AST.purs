module ReadDTS.AST where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT(..), runExcept, runExceptT)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (gets, modify_)
import Data.Array (catMaybes)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifoldable (class Bifoldable, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Bifunctor (lmap) as Bifunctor
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault, fold)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (Map)
import Data.Map (empty, insert, lookup, toUnfoldableUnordered) as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Matryoshka (CoalgebraM, anaM)
import ReadDTS (Options, TsDeclaration, Visit, foldMapParam, sequenceParam)
import ReadDTS (Param, Prop, Props, Application, sequenceProp) as ReadDTS
import Type.Prelude (SProxy(..))
import TypeScript.Compiler.Types (FullyQualifiedName(..), Typ)
import TypeScript.Compiler.Types (Typ) as TS
import TypeScript.Compiler.Types.Nodes (Declaration) as Nodes
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

type Prop decl ref = ReadDTS.Prop (TsType decl ref)

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
  | TsApplication ref (NonEmptyArray ref)
  | TsArray ref
  | TsBoolean
  | TsBooleanLiteral Boolean
  | TsClass (Array (ReadDTS.Prop ref))
  -- | Function (ReadDTS.Function ref)
  | TsInterface (Array (ReadDTS.Prop ref))
  | TsIntersection (Array ref)
  | TsNull
  | TsNumber
  | TsNumberLiteral Number
  | TsObject (Array (ReadDTS.Prop ref))
  | TsString
  | TsStringLiteral String
  | TsTuple (Array ref)
  | TsParametric ref (NonEmptyArray (ReadDTS.Param ref))
  | TsParameter String
  | TsTypeRef decl
  | TsUndefined
  | TsUnion (Array ref)
  | TsUnknown TSTyp
-- | Void

derive instance functorTsType :: Functor (TsType decl)
derive instance eqTsType :: (Eq decl, Eq ref) => Eq (TsType decl ref)
-- derive instance (Ord decl, Ord ref) => Ord (TsType decl ref)
derive instance genericTsType :: Generic (TsType decl ref) _
instance showTsType :: (Show decl, Show ref) => Show (TsType decl ref) where
  show s = genericShow s

instance Bifunctor TsType where
  bimap f _ (TsTypeRef ref) = TsTypeRef (f ref)
  bimap _ g t = unsafeCoerce (map g t)

instance Foldable (TsType decl) where
  foldMap _ TsAny = mempty
  foldMap f (TsArray t) = f t
  foldMap _ TsBoolean = mempty
  foldMap _ (TsBooleanLiteral _) = mempty
  foldMap f (TsClass ts) = foldMap (f <<< _.type) ts
  -- foldMap f (Function r)
  --   = foldMap (foldMap f <<< _.type) r.parameters
  --   <> foldMap f r.returnType
  -- -- foldMap f (Intersection ts) = fold (map (foldMap f) ts)
  foldMap f (TsInterface ts) = foldMap (f <<< _.type) ts
  foldMap f (TsApplication c ps) = f c <> foldMap f ps
  foldMap f (TsIntersection ts) = foldMap f ts
  foldMap _ TsNull = mempty
  foldMap _ TsNumber = mempty
  foldMap _ (TsNumberLiteral _) = mempty
  foldMap f (TsObject ts) = foldMap (f <<< _.type) ts
  foldMap _ (TsParameter _) = mempty
  foldMap f (TsParametric body params) = f body <> foldMap (foldMapParam f) params
  foldMap _ TsString = mempty
  foldMap _ (TsStringLiteral _) = mempty
  foldMap f (TsTuple ts) = foldMap f ts
  foldMap _ (TsTypeRef _) = mempty
  foldMap _ TsUndefined = mempty
  foldMap f (TsUnion ts) = foldMap f ts
  foldMap _ (TsUnknown _) = mempty
  -- foldMap _ Void = mempty

  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance Bifoldable TsType where
  bifoldMap f _ (TsTypeRef decl) = f decl
  bifoldMap _ g t = unsafeCoerce (foldMap g t)
  bifoldr f = bifoldrDefault f
  bifoldl f = bifoldlDefault f

instance Traversable (TsType decl) where
  sequence TsAny = pure TsAny
  sequence (TsApplication c ps) = TsApplication <$> c <*> sequence ps
  sequence (TsArray t) = TsArray <$> t
  sequence TsBoolean = pure TsBoolean
  sequence (TsBooleanLiteral b) = pure $ TsBooleanLiteral b
  sequence (TsClass props) = map TsClass $ sequence $ map ReadDTS.sequenceProp props
  --   sequence (Function r) = map Function
  --     $ { parameters: _, returnType: _ }
  --     <$> traverse sequenceParameter r.parameters
  --     <*> sequence r.returnType
  --     where
  --       sequenceParameter { name, "type": t } = { name, "type": _ } <$> sequence t
  sequence (TsInterface props) = map TsInterface $ sequence $ map ReadDTS.sequenceProp props
  sequence (TsIntersection ts) = TsIntersection <$> sequence ts
  sequence TsNull = pure TsNull
  sequence TsNumber = pure $ TsNumber
  sequence (TsNumberLiteral n) = pure $ TsNumberLiteral n
  sequence (TsObject props) = map TsObject $ sequence $ map ReadDTS.sequenceProp props
  sequence (TsParameter t) = pure $ TsParameter t
  sequence (TsParametric body params) = TsParametric
    <$> body
    <*> traverse sequenceParam params
  sequence TsString = pure $ TsString
  sequence (TsStringLiteral s) = pure $ TsStringLiteral s
  sequence (TsTuple ts) = TsTuple <$> sequence ts
  sequence (TsTypeRef fqn) = pure $ TsTypeRef fqn
  sequence TsUndefined = pure TsUndefined
  sequence (TsUnion ts) = TsUnion <$> sequence ts
  sequence (TsUnknown t) = pure $ TsUnknown t
  --   sequence Void = pure Void
  traverse = traverseDefault

type Typ' = TsType Nodes.Declaration (Typ ())

type TypeDeclaration t = { fqn :: FullyQualifiedName, "type" :: t }

visitor :: Visit (TypeDeclaration Typ') Typ'
visitor =
  { onDeclaration: \fqn t -> { fqn, "type": t }
  , onType:
      { any: TsAny
      , application: TsApplication
      , array: TsArray
      , boolean: TsBoolean
      , booleanLiteral: TsBooleanLiteral
      , class: TsClass
      , intersection: TsIntersection
      , interface: TsInterface
      , object: TsObject
      , null: TsNull
      , number: TsNumber
      , numberLiteral: TsNumberLiteral
      , parameter: TsParameter
      , parametric: TsParametric
      , string: TsString
      , stringLiteral: TsStringLiteral
      , tuple: TsTuple
      , typeRef: TsTypeRef
      , undefined: TsUndefined
      , unknown: TsUnknown <<< TSTyp
      , union: TsUnion
      }
  }

type KnownDeclarations decl = Map FullyQualifiedName (Mu (TsType decl))

-- type Context = { env :: KnownDeclarations FullyQualifiedName, path :: Path }

type TsRef =
  { ref :: TsDeclaration
  , fullyQualifiedName :: FullyQualifiedName
  }

type TsDeclarations = Map FullyQualifiedName TsDeclaration

type VisitMonad a = ExceptT (Array String) (StateT TsDeclarations Effect) a

type ReadTsType m = TS.Typ () -> m (TsType FullyQualifiedName (TS.Typ ()))

type Seed = { level :: Int, ref :: TS.Typ () }

coalgebra ::
  forall m.
  MonadError (Array String) m =>
  ReadTsType m ->
  CoalgebraM m (TsType FullyQualifiedName) Seed
coalgebra readTsType { level, ref } =
  if level < 10 then do
    td <- readTsType ref
    pure $ map seed td
  else
    throwError [ "Maximum recursion depth in ReadDTS.AST.visitor coalgebra" ]
  where
  seed = { level: level + 1, ref: _ }

-- -- -- type ReadDeclaration = TsDeclaration → Effect (TypeDeclaration TsRef)
-- -- -- 
-- -- -- type ReadTs = { readDeclaration ∷ ReadDeclaration, readType ∷ ReadType }
-- -- -- 
-- -- -- -- type Declarations = Map FullyQualifiedName (TypeDeclaration FullyQualifiedName)
-- -- 
-- -- 
-- -- -- -- visit
-- -- -- --   ∷ Options
-- -- -- --   → Array String
-- -- -- --   → Array ReadDTS.InMemoryFile
-- -- -- --   → TypeScript.CompilerHost
-- -- -- --   → Effect
-- -- -- --     (Either
-- -- -- --       (Array String)
-- -- -- --       { topLevel ∷ Array FullyQualifiedName, declarations ∷ Declarations }
-- -- -- --     )

-- visit program = do
--   let
--     -- | This unfolding expands the tree of nested types.
--     -- | It leaves references do type declarations untouched.
--     unfoldTsRefs ∷ Seed → _ (Mu _)
--     unfoldTsRefs = anaM (coalgebra readDeclaration)
-- 
--     roots = readRootsDeclarations program visitor
--     readDeclarations decls = do
--       whenM (gets $ isNothing <<< Map.lookup fqn) do

--     go = for_ (Map.values roots :: List _) \{ fqn, type: t } -> do
--       whenM (gets $ isNothing <<< Map.lookup fqn) do
--         let
--           t' = unfoldTsRefs { level: 0, ref: t }
--           decls' = bifoldMap (\tsRef -> List.singleton tsRef) (const mempty) t'
--           t'' = Bifunctor.lmap _.fqn t'
--         modify_ (Map.insert fqn t'')
-- -- 
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
