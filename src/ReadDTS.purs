module ReadDTS where

import Prelude

import Data.Array (catMaybes, elem, filter, uncons) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray) as Array.NonEmpty
import Data.Array.NonEmpty (fromArray) as NonEmpty
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Undefined.NoProblem ((!))
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Debug (traceM)
import ReadDTS.TypeScript (asDeclarationStatement, isNodeExported)
import Type.Prelude (Proxy(..))
import TypeScript.Compier.Types (getParameters, getReturnType) as Signature
import TypeScript.Compiler.Checker (getFullyQualifiedName, getSymbolAtLocation, getTypeArguments, getTypeAtLocation, getTypeOfSymbolAtLocation)
import TypeScript.Compiler.Checker.Internal (getElementTypeOfArrayType, isAnyType, isBooleanType, isNullType, isNumberType, isStringType, isTupleType, isUndefinedType)
import TypeScript.Compiler.Factory.NodeTests (asClassDeclaration, asEmptyStatement, asFunctionDeclaration, asInterfaceDeclaration, asParameterDeclaration, asTypeAliasDeclaration)
import TypeScript.Compiler.Program (getRootFileNames, getSourceFiles, getTypeChecker)
import TypeScript.Compiler.Types (FullyQualifiedName(..), Node, Program, Typ, TypeChecker)
import TypeScript.Compiler.Types.Nodes (DeclarationStatement, TypeParameterDeclaration, interface) as Nodes
import TypeScript.Compiler.Types.Nodes (getChildren)
import TypeScript.Compiler.Types.Symbol (checkFlag, getDeclarations, getName) as Symbol
import TypeScript.Compiler.Types.Typs (TypeReference, asClassType, asInterfaceType, asIntersectionType, asNumberLiteralType, asObjectType, asStringLiteralType, asTypeParameter, asTypeReference, asUnionType, getCallSignatures, getProperties, getSymbol)
import TypeScript.Compiler.Types.Typs (interface) as Typs
import TypeScript.Compiler.Types.Typs.Internal (reflectBooleanLiteralType)
import TypeScript.Compiler.UtilitiesPublic (idText)

type FQN = FullyQualifiedName

newtype Param :: Type -> Type
newtype Param t = Param
  { name :: String
  , default :: Maybe t
  }

derive instance Newtype (Param t) _
derive instance Eq t => Eq (Param t)
derive instance Generic (Param t) _
derive instance Functor Param
instance Foldable Param where
  foldMap f (Param { default }) = foldMap f default
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance Traversable Param where
  sequence (Param { name, default: t }) = (Param <<< { default: _, name }) <$> sequence t
  traverse = traverseDefault

instance Show t => Show (Param t) where
  show = genericShow

newtype Params t = Params (NonEmptyArray (Param t))

derive instance Newtype (Params t) _
derive instance Eq t => Eq (Params t)
derive instance Generic (Params t) _
derive instance Functor Params
instance Foldable Params where
  foldMap f (Params ps) = foldMap (foldMap f) ps
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance Traversable Params where
  sequence (Params ps) = Params <$> traverse sequence ps
  traverse f = traverseDefault f

instance Show t => Show (Params t) where
  show = genericShow

type Prop t =
  { name :: String
  , type :: t
  , optional :: Boolean
  }

sequenceProp :: forall m ref. Applicative m => Prop (m ref) -> m (Prop ref)
sequenceProp { name, type: t, optional } = { type: _, name, optional } <$> t

type Props t = Array (Prop t)

-- | `Arg` and `Prop` share the same structure so we reuse them.
type Arg t = Prop t
type Args t = Props t

sequenceArg :: forall m ref. Applicative m => Arg (m ref) -> m (Arg ref)
sequenceArg = sequenceProp

-- | XXX: Is there a way to pass `Maybe` constructors to FFI
-- | but preserving typechecking on typescript side and drop this
-- | `nullable` parameter?
type OnDeclaration d t = FQN -> t -> d

type OnType :: Type -> Type
type OnType t =
  { any :: t
  , array :: Typ () -> t
  , boolean :: t
  , application :: FQN -> Nodes.DeclarationStatement -> NonEmptyArray (Typ ()) -> t
  , booleanLiteral :: Boolean -> t
  , class :: Props (Typ ()) -> t
  , function :: Args (Typ ()) -> Typ () -> t
  , interface :: Props (Typ ()) -> t
  , intersection :: Array (Typ ()) -> t
  , object :: Props (Typ ()) -> t
  , number :: t
  , numberLiteral :: Number -> t
  , null :: t
  , parameter :: String -> t
  , parametric :: Typ () -> Params (Typ ()) -> t
  , string :: t
  , stringLiteral :: String -> t
  , tuple :: Array (Typ ()) -> t
  , typeRef :: FQN -> Nodes.DeclarationStatement -> t
  , undefined :: t
  , union :: Array (Typ ()) -> t
  , unknown :: Typ () -> t
  }

type Visit d t =
  { onType :: OnType t
  , onDeclaration :: OnDeclaration d t
  }

-- | `compile` - runs full compiler checks so it caches
-- | all possible errors in the ts code
type Options =
  { compile :: Boolean
  , debug :: Boolean
  }

defaults :: Options
defaults =
  { compile: true
  , debug: true
  }

readRootDeclarationNodes :: Program -> Array Nodes.DeclarationStatement
readRootDeclarationNodes program = do
  let
    checker = getTypeChecker program
    rootNames = getRootFileNames program
    fileName = Nodes.interface >>> _.fileName
    -- FIXME: Filtering was broken after introduction of InSubdir compiler host.
    rootFiles = getSourceFiles program -- $ Array.filter ((_ `Array.elem` rootNames) <<< fileName) $ getSourceFiles program
  -- | `SourceFile` "usually" has as a single root child of type `SyntaxList`.
  -- | * We are not interested in this particular child.
  -- | * We should probably recurse into any container like
  -- node (`Block`, `ModuleDeclaration` etc.) down the stream too.
  Array.catMaybes $ rootFiles >>= getChildren >>= getChildren <#> \(node :: Node "" ()) -> do
    -- | Ignore non exported declarations and "semicolons"
    if isNodeExported checker node && isNothing (asEmptyStatement node) then do
      asDeclarationStatement node
    else
      Nothing

readRootDeclarations
  :: Program
  -> Map FullyQualifiedName
       { typ :: Typ ()
       , params :: Maybe (Params (Typ ()))
       }
readRootDeclarations program = do
  let
    checker = getTypeChecker program
    decls = readRootDeclarationNodes program
  Map.fromFoldable $ Array.catMaybes $ decls <#> \node -> do
    case readDeclaration checker node of
      Just { fqn, params, typ } -> Just $ fqn /\ { params, typ }
      Nothing -> do
        Nothing

readDeclaration
  :: forall l r
   . TypeChecker
  -> Node l r
  -> Maybe
       { fqn :: FullyQualifiedName
       , params :: Maybe (Params (Typ ()))
       , typ :: Typ ()
       }
readDeclaration checker = do
  let
    params :: forall k. { typeParameters :: _ | k } -> _
    params n = map Params $ NonEmpty.fromArray $ map (readTypeParameterDeclaration checker) (n.typeParameters ! [])

    readDeclaration' node
      | Just name <- NoProblem.toMaybe <<< _.name <<< Nodes.interface =<< asFunctionDeclaration node = do
          typ <- getTypeAtLocation checker name
          s <- getSymbol typ
          let
            fqn = getFullyQualifiedName checker s
          -- | TODO: Type parameters can be extracted from the `Signature`
          -- | when function type is constructed (they aren't at the moment
          -- | because I don't know the exact meaning of them.
          pure { fqn, params: Nothing, typ }
      | Just n <- Nodes.interface <$> asInterfaceDeclaration node = do
          typ <- getTypeAtLocation checker node
          s <- getSymbol typ
          let
            -- t' = readType checker t (params n) visit.onType
            fqn = getFullyQualifiedName checker s
          pure { fqn, params: params n, typ } -- /\ visit.onDeclaration fqn t'
      | Just n <- Nodes.interface <$> asClassDeclaration node = do
          typ <- getTypeAtLocation checker node
          s <- getSymbol typ
          let
            -- t' = readType checker t (params n) visit.onType
            fqn = getFullyQualifiedName checker s
          -- pure $ fqn /\ visit.onDeclaration fqn t'
          pure { fqn, params: params n, typ } -- /\ visit.onDeclaration fqn t'
      | Just n <- Nodes.interface <$> asTypeAliasDeclaration node = do
          typ <- getTypeAtLocation checker node
          s <- getSymbolAtLocation checker n.name
          let
            -- t' = readType checker t (params n) visit.onType
            fqn = getFullyQualifiedName checker s
          -- pure $ fqn /\ visit.onDeclaration fqn t'
          pure { fqn, params: params n, typ } -- /\ visit.onDeclaration fqn t'
      | otherwise = do
          Nothing
  readDeclaration'

readTypeParameterDeclaration :: TypeChecker -> Nodes.TypeParameterDeclaration -> Param (Typ ())
readTypeParameterDeclaration checker n = do
  let
    n' = Nodes.interface n
    default = do
      d <- NoProblem.toMaybe n'.default
      getTypeAtLocation checker d
  Param
    { name: idText n'.name
    -- constraint: ....
    , default
    }

fqnToString :: FullyQualifiedName -> String
fqnToString (FullyQualifiedName fqn) = fqn

asTypeRef :: forall t. TypeChecker -> Typ () -> OnType t -> Maybe t
asTypeRef checker t onType = do
  let
    r = Typs.interface t
    args = r.aliasTypeArguments ! []
  -- traceM "Trying type ref:"
  -- traceM $ formatTypeFlags' t
  -- traceM $ Typs.asTypeReference t
  s <- NoProblem.toMaybe r.aliasSymbol
  -- traceM $ "|args| = " <> show (length args)
  let
    fqn = getFullyQualifiedName checker s
  -- traceM $ "fqn = " <> fqnToString fqn
  -- traceM $ append "| declarations | = " $ show $ length $ Symbol.getDeclarations s
  { head: ref } <- Array.uncons $ Symbol.getDeclarations s

  ref' <- asDeclarationStatement ref
  case Array.NonEmpty.fromArray args of
    Just args' -> do
      pure $ onType.application fqn ref' args'
    Nothing -> do
      pure $ onType.typeRef fqn ref'

asApplication
  :: forall i
   . TypeChecker
  -> Typ i
  -> Maybe
       (FQN /\ Nodes.DeclarationStatement /\ NonEmptyArray (Typ ()))
asApplication checker t = do
  { target: body, typeArguments } <- asTypeReference t <#> Typs.interface
  ta <- NoProblem.toMaybe typeArguments
  params <- Array.NonEmpty.fromArray $ ta

  s <- getSymbol body
  { head: decl } <- Array.uncons $ Symbol.getDeclarations s
  decl' <- asDeclarationStatement decl
  let
    fqn = getFullyQualifiedName checker s
  pure $ fqn /\ decl' /\ params

-- | This function is unable to detect parametric types.
-- | It doesn't handle type references either. Why?
-- | Because I was not able to distinguish type references from
-- | parametric type. The simplest example is something like:
-- |
-- | ```typescript
-- | export class Y {};"
-- | export class X<param=number>{ y: Y<string> }"
-- | ```
-- |
-- | The ts type representation (flags, symbol etc.) is
-- | the same for `X` and `Y` in this context as far as I was
-- | able to detect.
-- |
-- | It is just easier to handle these two cases separatly during
-- | unfolding.
-- |
-- | TODO: Is enum: https://stackoverflow.com/a/55406883/194614
readType :: forall t. TypeChecker -> Typ () -> OnType t -> t
readType checker t onType
  | isAnyType checker t = onType.any
  | Just e <- getElementTypeOfArrayType checker t = onType.array e
  | isBooleanType checker t = onType.boolean
  | Just bl <- reflectBooleanLiteralType t = onType.booleanLiteral bl
  | Just i <- asIntersectionType t = onType.intersection $ Typs.interface i # _.types
  | Just i <- asInterfaceType t = do
      let
        props = readProperties checker i
      onType.interface (fromMaybe [] props)
  | Just i <- asClassType t = do
      let
        props = readProperties checker i
      onType.class (fromMaybe [] props)
  | isNullType checker t = onType.null
  | isNumberType checker t = onType.number
  | Just n <- asNumberLiteralType t = onType.numberLiteral $ Typs.interface n # _.value
  | isStringType checker t = onType.string
  | Just s <- asStringLiteralType t = onType.stringLiteral $ Typs.interface s # _.value
  | Just u <- asUnionType t = onType.union $ Typs.interface u # _.types
  | Just r <- asTupleTypeReference checker t = onType.tuple (getTypeArguments checker r)
  | isUndefinedType checker t = onType.undefined
  | Just (fqn /\ decl /\ args) <- asApplication checker t = onType.application fqn decl args
  | Just { args, returnType } <- asFunction checker t =
      onType.function args returnType
  -- | At this point we are sure that this type is not a tuple, array etc.
  -- | Let's assume it is `ObjectType` which represents "object"...
  | Just o <- asObjectType t = do
      let
        props = readProperties checker o
      onType.object (fromMaybe [] props)
  | Just t' <- Typs.interface <$> asTypeParameter t = do
      let
        s = t'.symbol
        n = Symbol.getName s
      onType.parameter n
  | otherwise = do
      onType.unknown t

asFunction
  :: forall r
   . TypeChecker
  -> Typ r
  -> Maybe { args :: Args (Typ ()), returnType :: Typ () }
asFunction checker t = do
  { head: sig } <- Array.uncons (getCallSignatures t)
  -- | We should probabl extract type params here from signature and
  -- | ignore the `params` arg.
  let
    param s = do
      { head: decl } <- Array.uncons <<< Symbol.getDeclarations $ s
      pd <- Nodes.interface <$> asParameterDeclaration decl
      pt <- getTypeOfSymbolAtLocation checker s decl
      pure
        { name: Symbol.getName s
        , type: pt
        , optional: isJust (NoProblem.toMaybe pd.questionToken)
        }
  args <- traverse param $ Signature.getParameters sig
  pure { args, returnType: Signature.getReturnType sig }

asTupleTypeReference :: forall i. TypeChecker -> Typ i -> Maybe TypeReference
asTupleTypeReference checker t =
  if isTupleType checker t then asTypeReference t
  else Nothing

readProperties :: forall i. TypeChecker -> Typ i -> Maybe (Array (Prop (Typ ())))
readProperties checker t = do
  let
    step s = do
      { head: decl } <- Array.uncons <<< Symbol.getDeclarations $ s
      t' <- getTypeOfSymbolAtLocation checker s decl
      let
        optional = Symbol.checkFlag s (Proxy :: Proxy "Optional")
      pure { name: Symbol.getName s, type: t', optional }

  traverse step $ getProperties t
