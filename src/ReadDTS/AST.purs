module ReadDTS.AST where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Free (Free, wrap)
import Control.Monad.Rec.Loops (whileJust_)
import Control.Monad.State (execStateT)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Either (Either)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault, length)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Lens (use)
import Data.Lens.Record (prop)
import Data.Lens.Setter ((%=), (.=))
import Data.List (List(..), catMaybes, fromFoldable, length, singleton) as List
import Data.List (List, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Profunctor.Strong ((***))
import Data.Set (fromFoldable, insert, member) as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Debug (traceM)
import Matryoshka (cata, futuM)
import ReadDTS (Args, Params, Props, readDeclaration, readInnerType, readTopLevelType, sequenceArg, sequenceProp) as ReadDTS
import ReadDTS (Params, Visit, fqnToString, readRootDeclarationNodes, readRootDeclarations)
import ReadDTS.TypeScript (getDeclarationStatementFqn)
import Type.Prelude (Proxy(..))
import TypeScript.Compiler.Checker (getExportsOfModule, getSymbolAtLocation)
import TypeScript.Compiler.Program (getRootFileNames, getSourceFiles, getTypeChecker)
import TypeScript.Compiler.Types (FullyQualifiedName, Program, Typ, TypeChecker)
import TypeScript.Compiler.Types (Typ) as TS
import TypeScript.Compiler.Types.Nodes (DeclarationStatement, interface) as Nodes
import TypeScript.Compiler.Types.Nodes (getChildren)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

newtype TSTyp = TSTyp (TS.Typ ())

instance Show TSTyp where
  show (TSTyp _) = "TSTyp"

instance Eq TSTyp where
  eq (TSTyp t1) (TSTyp t2) = unsafeRefEq t1 t2

newtype TypeRepr = TypeRepr (Mu (TsType FullyQualifiedName))

derive instance Eq TypeRepr
derive newtype instance Show TypeRepr

-- | FIXME: Rename `ReadDTS.AST.TsType` to `TS.TypRepr.TypReprF`
data TsType decl ref
  = TsAny
  | TsApplication decl (NonEmptyArray ref)
  | TsArray ref
  | TsBoolean
  | TsBooleanLiteral Boolean
  -- | Base classes, list of constructors
  | TsClass
      { bases :: Array ref
      , constructors :: Array (ReadDTS.Args ref)
      , props :: ReadDTS.Props ref
      }
  | TsFunction (ReadDTS.Args ref) ref
  | TsInterface
    { bases :: Array ref
    , props :: (ReadDTS.Props ref)
    }
  | TsIntersection (Array ref)
  | TsNull
  | TsNumber
  | TsNumberLiteral Number
  | TsObject (ReadDTS.Props ref)
  | TsString
  | TsStringLiteral String
  | TsTuple (Array ref)
  | TsParametric ref (ReadDTS.Params ref)
  | TsParameter String
  | TsTypeRef decl
  | TsUndefined
  | TsUnion (Array ref)
  | TsUnknown TSTyp
  | TsMerge (Array ref)

printTsTypeConstructorName :: forall decl ref. TsType decl ref -> String
printTsTypeConstructorName TsAny = "TsAny"
printTsTypeConstructorName (TsApplication _ _) = "TsApplication"
printTsTypeConstructorName (TsArray _) = "TsArray"
printTsTypeConstructorName TsBoolean = "TsBoolean"
printTsTypeConstructorName (TsBooleanLiteral _) = "TsBooleanLiteral"
printTsTypeConstructorName (TsClass _) = "TsClass"
printTsTypeConstructorName (TsFunction _ _) = "TsFunction"
printTsTypeConstructorName (TsInterface _) = "TsInterface"
printTsTypeConstructorName (TsIntersection _) = "TsIntersection"
printTsTypeConstructorName TsNull = "TsNull"
printTsTypeConstructorName TsNumber = "TsNumber"
printTsTypeConstructorName (TsNumberLiteral _) = "TsNumberLiteral"
printTsTypeConstructorName (TsObject _) = "TsObject"
printTsTypeConstructorName TsString = "TsString"
printTsTypeConstructorName (TsStringLiteral _) = "TsStringLiteral"
printTsTypeConstructorName (TsTuple _) = "TsTuple"
printTsTypeConstructorName (TsParametric _ _) = "TsParametric"
printTsTypeConstructorName (TsParameter _) = "TsParameter"
printTsTypeConstructorName (TsTypeRef _) = "TsTypeRef"
printTsTypeConstructorName TsUndefined = "TsUndefined"
printTsTypeConstructorName (TsUnion _) = "TsUnion"
printTsTypeConstructorName (TsUnknown _) = "TsUnknown"
printTsTypeConstructorName (TsMerge _) = "TsMerge"

tsInterface :: forall dec t. Array t -> ReadDTS.Props t -> TsType dec t
tsInterface bases props = TsInterface { bases, props }

tsClass :: forall dec t. Array t -> Array (ReadDTS.Args t) -> ReadDTS.Props t -> TsType dec t
tsClass bases constructors props = TsClass { bases, constructors, props }

-- | TsVoid

derive instance functorTsType :: Functor (TsType decl)
derive instance eqTsType :: (Eq decl, Eq ref) => Eq (TsType decl ref)
instance eq1TsType :: Eq decl => Eq1 (TsType decl) where
  eq1 = eq

derive instance genericTsType :: Generic (TsType decl ref) _
instance showTsType :: (Show decl, Show ref) => Show (TsType decl ref) where
  show s = genericShow s

instance Bifunctor TsType where
  bimap f g (TsApplication decl ref) = TsApplication (f decl) (map g ref)
  bimap f _ (TsTypeRef decl) = TsTypeRef (f decl)
  bimap _ g t = unsafeCoerce (map g t)

instance Foldable (TsType decl) where
  foldMap _ TsAny = mempty
  foldMap f (TsArray t) = f t
  foldMap _ TsBoolean = mempty
  foldMap _ (TsBooleanLiteral _) = mempty
  foldMap f (TsClass { bases, constructors, props }) =
    foldMap f bases
    <> foldMap (foldMap (f <<< _.type)) constructors
    <> foldMap (f <<< _.type) props
  foldMap f (TsFunction args r) = foldMap (f <<< _.type) args <> f r
  foldMap f (TsInterface { bases, props }) =
    foldMap f bases
    <> foldMap (f <<< _.type) props
  -- foldMap f (Intersection ts) = fold (map (foldMap f) ts)
  foldMap f (TsApplication _ ps) = foldMap f ps
  foldMap f (TsIntersection ts) = foldMap f ts
  foldMap _ TsNull = mempty
  foldMap _ TsNumber = mempty
  foldMap _ (TsNumberLiteral _) = mempty
  foldMap f (TsObject ts) = foldMap (f <<< _.type) ts
  foldMap _ (TsParameter _) = mempty
  foldMap f (TsParametric body params) = f body <> foldMap f params
  foldMap _ TsString = mempty
  foldMap _ (TsStringLiteral _) = mempty
  foldMap f (TsTuple ts) = foldMap f ts
  foldMap _ (TsTypeRef _) = mempty
  foldMap _ TsUndefined = mempty
  foldMap f (TsUnion ts) = foldMap f ts
  foldMap _ (TsUnknown _) = mempty
  foldMap f (TsMerge ts) = foldMap f ts
  foldr f t = foldrDefault f t
  foldl f t = foldlDefault f t

instance Bifoldable TsType where
  bifoldMap f _ (TsApplication decl _) = f decl
  bifoldMap f _ (TsTypeRef decl) = f decl
  bifoldMap _ g t = unsafeCoerce (foldMap g t)
  bifoldr f = bifoldrDefault f
  bifoldl f = bifoldlDefault f

instance Traversable (TsType decl) where
  sequence TsAny = pure TsAny
  sequence (TsApplication decl ps) = TsApplication decl <$> sequence ps
  sequence (TsArray t) = TsArray <$> t
  sequence TsBoolean = pure TsBoolean
  sequence (TsBooleanLiteral b) = pure $ TsBooleanLiteral b
  sequence (TsClass { bases, constructors, props }) = ado
    bs' <- sequence bases
    cs' <- traverse (traverse ReadDTS.sequenceArg) constructors
    props' <- sequence $ map ReadDTS.sequenceProp props
    in
      tsClass bs' cs' props'
  sequence (TsFunction args r) = TsFunction
    <$> traverse ReadDTS.sequenceArg args
    <*> r
  sequence (TsInterface { bases, props }) = TsInterface <$> ado
    bases <- sequence bases
    props <- sequence (map ReadDTS.sequenceProp props)
    in
      { bases, props }
  sequence (TsIntersection ts) = TsIntersection <$> sequence ts
  sequence TsNull = pure TsNull
  sequence TsNumber = pure $ TsNumber
  sequence (TsNumberLiteral n) = pure $ TsNumberLiteral n
  sequence (TsObject props) = map TsObject $ sequence $ map ReadDTS.sequenceProp props
  sequence (TsParameter t) = pure $ TsParameter t
  sequence (TsParametric body params) = TsParametric
    <$> body
    <*> sequence params
  sequence TsString = pure $ TsString
  sequence (TsStringLiteral s) = pure $ TsStringLiteral s
  sequence (TsTuple ts) = TsTuple <$> sequence ts
  sequence (TsTypeRef decl) = pure $ TsTypeRef decl
  sequence TsUndefined = pure TsUndefined
  sequence (TsUnion ts) = TsUnion <$> sequence ts
  sequence (TsUnknown t) = pure $ TsUnknown t
  sequence (TsMerge ts) = TsMerge <$> sequence ts
  --   sequence Void = pure Void
  traverse = traverseDefault

type Typ' = TsType TsRef (Typ ())

type TypeDeclaration t = { fqn :: FullyQualifiedName, "type" :: t }

type TsRef =
  { ref :: Nodes.DeclarationStatement
  , fullyQualifiedName :: FullyQualifiedName
  }

visitor :: Visit (TypeDeclaration Typ') Typ'
visitor =
  { onDeclaration: { fqn: _, "type": _ }
  , onType:
      { any: TsAny
      , application: \fqn ref args ->
          TsApplication { fullyQualifiedName: fqn, ref } args
      , array: TsArray
      , boolean: TsBoolean
      , booleanLiteral: TsBooleanLiteral
      , class: tsClass
      , function: TsFunction
      , intersection: TsIntersection
      , interface: tsInterface
      , object: TsObject
      , null: TsNull
      , number: TsNumber
      , numberLiteral: TsNumberLiteral
      , parameter: TsParameter
      , parametric: TsParametric
      , string: TsString
      , stringLiteral: TsStringLiteral
      , tuple: TsTuple
      , typeRef: map TsTypeRef <<< { fullyQualifiedName: _, ref: _ }
      , undefined: TsUndefined
      , unknown: TsUnknown <<< TSTyp
      , union: TsUnion
      , merge: TsMerge
      }
  }

foldMapRec :: forall a m. Monoid m => (a -> m) -> Mu (TsType a) -> m
foldMapRec f = cata (bifoldMap f identity)

mapRec :: forall a b. (a -> b) -> Mu (TsType a) -> Mu (TsType b)
mapRec f = cata $ Mu.In <<< lmap f

type Decl = { fullyQualifiedName :: FullyQualifiedName, ref :: Nodes.DeclarationStatement }

type Seed = { level :: Int, ref :: Typ () }

type ReadTsType m = Seed -> m (TsType Decl (TS.Typ ()))

-- | Our unfold step function which handles some initial corner cases.
-- |
-- | * We want to "resolve" type ref during the top layer read
-- | because a lot of types are type refs (like arrays, tuples or
-- | even class/interface declaration) so we run `readTypeRef` only
-- | when `level > 0`.
-- |
-- | * We have to unfold manually parametric type because I have not found
-- | a way to properly detect and disambiguate this case based only on
-- | the `Typ ()` (it is self referencing `typeReference`...)

newtype MaxDepth = MaxDepth Int

mkCoalgebra
  :: forall m
   . MonadError (Array String) m
  => MaxDepth
  -> TypeChecker
  -> Maybe (Params (Typ ()))
  -> (Seed -> m (TsType Decl (Free (TsType Decl) Seed)))
mkCoalgebra (MaxDepth maxDepth) checker params seed = do
  -- FIXME: We should probably provide a dedicated constructor
  -- for *unread* scenario - probably a different one than
  -- `TsUnknown` because ts complier possibly uses that.
  if (seed.level > maxDepth) then pure (TsUnknown $ TSTyp seed.ref)
  -- throwError [ "Maximum recursion depth (max depth is " <> show maxDepth <> ") in ReadDTS.AST.visitor coalgebra" ]
  else do

    let
      mkSeed = { level: seed.level + 1, ref: _ }

      mapSeed :: forall decl. TsType decl (Typ ()) -> TsType decl (Free (TsType decl) Seed)
      mapSeed = map (pure <<< mkSeed)

      readTopLevelType ref = mapSeed $ ReadDTS.readTopLevelType checker ref visitor.onType

      readInnerType ref = mapSeed $ ReadDTS.readInnerType checker ref visitor.onType

    pure $
      if seed.level == 0 then case params of
        Just params' -> do
          TsParametric
            (wrap $ readTopLevelType seed.ref)
            (map (wrap <<< readInnerType) params')
        Nothing ->
          readTopLevelType seed.ref
      else readInnerType seed.ref

unfoldType
  :: forall m
   . MonadError (Array String) m
  => MaxDepth
  -> TypeChecker
  -> Maybe (Params (Typ ()))
  -> Seed
  -> m (Mu (TsType Decl))
unfoldType maxDepth checker params = futuM (mkCoalgebra maxDepth checker params)

unfoldType'
  :: forall m
   . MonadError (Array String) m
  => MaxDepth
  -> TypeChecker
  -> Maybe (Params (Typ ()))
  -> Typ ()
  -> m (Mu (TsType Decl))
unfoldType' maxDepth checker params typ = unfoldType maxDepth checker params { level: 0, ref: typ }

type RootDeclarations = Map FullyQualifiedName TypeRepr

types :: Program -> Either (Array String) RootDeclarations
types program = un Identity $ runExceptT do
  let
    _unknowns = prop (Proxy :: Proxy "unknowns")
    _knowns = prop (Proxy :: Proxy "knowns")
    _seen = prop (Proxy :: Proxy "seen")

    checker = getTypeChecker program
    roots = List.fromFoldable $ readRootDeclarationNodes program

    -- FIXME: We are skipping declarations for which we were not
    -- able to extract fqn. This should be reported for sure.
    step decl = do
      fqn <- getDeclarationStatementFqn checker decl
      pure $ fqn /\ decl

    unknowns = List.catMaybes $ map step roots

    ctx =
      { unknowns
      , knowns: List.Nil
      , seen: Set.fromFoldable $ map fst unknowns
      }

    getUnknown = do
      use _unknowns >>= case _ of
        head : tail -> do
          _unknowns .= tail
          pure $ Just head
        List.Nil -> pure Nothing

    foldTypeDecls = foldMapRec (\{ fullyQualifiedName: fqn, ref } -> List.singleton (fqn /\ ref))
    stripTypeRefs = mapRec _.fullyQualifiedName

  -- traceM roots
  res <- map _.knowns $ flip execStateT ctx $ whileJust_ getUnknown \(fqn /\ node) -> do
    case ReadDTS.readDeclaration checker node of
      Nothing -> throwError [ "Problem reading " <> fqnToString fqn ]
      Just { typ, params } -> do
        type_ <- unfoldType (MaxDepth 4) checker params { level: 0, ref: typ }

        _knowns %= List.Cons (fqn /\ stripTypeRefs type_)
        for (foldTypeDecls type_) \decl@(fqn' /\ _) -> do
          seen <- use _seen
          when (not $ fqn' `Set.member` seen) do
            _seen %= Set.insert fqn'
            _unknowns %= List.Cons decl

  pure $ Map.fromFoldable $ (identity *** TypeRepr) <$> res
