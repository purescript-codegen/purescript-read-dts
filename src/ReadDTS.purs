module ReadDTS where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes)
import Data.Array (catMaybes, elem, filter, head, length, uncons) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray) as Array.NonEmpty
import Data.Array.NonEmpty (fromArray) as NonEmpty
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault, for_)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, for, sequence, traverse, traverseDefault)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Undefined.NoProblem ((!))
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Debug (traceM)
import ReadDTS.TypeScript (asDeclarationStatement, isNodeExported)
import Type.Prelude (Proxy(..))
import TypeScript.Compier.Types (getParameters, getReturnType) as Signature
import TypeScript.Compiler.Checker (getExportedFullyQualifiedName, getFullyQualifiedName, getSignaturesOfType, getSymbolAtLocation, getTypeArguments, getTypeAtLocation, getTypeOfSymbolAtLocation)
import TypeScript.Compiler.Checker.Internal (getElementTypeOfArrayType, isAnyType, isBooleanType, isNullType, isNumberType, isStringType, isTupleType, isUndefinedType)
import TypeScript.Compiler.Factory.NodeTests (asClassDeclaration, asEmptyStatement, asExportAssignment, asFunctionDeclaration, asInterfaceDeclaration, asParameterDeclaration, asTypeAliasDeclaration)
import TypeScript.Compiler.Factory.NodeTests as NodeTests
import TypeScript.Compiler.Factory.NodeTests as NodesTests
import TypeScript.Compiler.Program (getRootFileNames, getSourceFiles, getTypeChecker)
import TypeScript.Compiler.Types (FullyQualifiedName(..), Node, Program, Signature, Typ, TypeChecker, construct)
import TypeScript.Compiler.Types (call, construct) as Compiler.Types
import TypeScript.Compiler.Types.Nodes (DeclarationStatement, TypeParameterDeclaration, VariableStatement, interface) as Nodes
import TypeScript.Compiler.Types.Nodes (getChildren, nodeText, onProps)
import TypeScript.Compiler.Types.Symbol (checkFlag, getDeclarations, getName, interface) as Symbol
import TypeScript.Compiler.Types.Symbol (getDeclarations)
import TypeScript.Compiler.Types.Typs (TypeReference, asClassType, asInterfaceType, asIntersectionType, asNumberLiteralType, asObjectType, asStringLiteralType, asTypeParameter, asTypeReference, asUnionType, getApparentProperties, getBaseTypes, getCallSignatures, getConstructSignatures, getProperties, getSymbol)
import TypeScript.Compiler.Types.Typs (interface) as Typs
import TypeScript.Compiler.Types.Typs.Internal (reflectBooleanLiteralType)
import TypeScript.Compiler.UtilitiesPublic (idText)
import TypeScript.Debug (formatNodeFlags, formatNodeFlags', formatSymbolFlags', formatTypeFlags')
import Unsafe.Coerce (unsafeCoerce)

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
  -- | FIXME: we can be more specific here - the first array is of type `ts.BaseType`
  , class :: Array (Typ ()) -> Array (Args (Typ ())) -> Props (Typ ()) -> t
  , function :: Args (Typ ()) -> Typ () -> t
  , interface :: Array (Typ ()) -> Props (Typ ()) -> t
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
  , merge :: Array (Typ ()) -> t
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
    -- rootFiles = getSourceFiles program -- $ Array.filter ((_ `Array.elem` rootNames) <<< fileName) $ getSourceFiles program
    rootFiles = Array.filter ((_ `Array.elem` rootNames) <<< fileName) $ getSourceFiles program
  -- | `SourceFile` "usually" has as a single root child of type `SyntaxList`.
  -- | * We are not interested in this particular child.
  -- | * We should probably recurse into any container like
  -- node (`Block`, `ModuleDeclaration` etc.) down the stream too.
  -- traceM "unfilteredRootFiles:"
  -- unfilteredRootFiles = getSourceFiles program -- $ Array.filter ((_ `Array.elem` rootNames) <<< fileName) $ getSourceFiles program
  -- traceM $ map fileName unfilteredRootFiles
  -- traceM "rootFiles"
  -- traceM $ map fileName rootFiles
  -- traceM "rootNames"
  -- traceM rootNames
  -- traceM rootFiles

  -- let
  --   x = for (rootFiles >>= getChildren) \(child :: Node "" ())-> do
  --    traceM "child"
  --    traceM child
  --    Nothing

  Array.catMaybes $ do
    let
      allChildren = do
        fileNode <- rootFiles
        child <- getChildren fileNode
        childOfAChild <- getChildren child
        [ child, childOfAChild ]

    -- rootFiles >>= getChildren >>= getChildren <#> \(node :: Node "" ()) -> do
    allChildren <#> \(node :: Node "" ()) -> do
      void $ for (NodeTests.asVariableStatement node <#> Nodes.interface) \{ declarationList } -> do
        let
          { declarations } = Nodes.interface declarationList
        for declarations \decl -> do
          let
            { name } = Nodes.interface decl
          for (NodesTests.asIdentifier name <#> Nodes.interface) \{ escapedText } -> do
            traceM "declaration:"
            traceM escapedText


      -- | Ignore non exported declarations and "semicolons"
      if isNodeExported checker node && isNothing (asEmptyStatement node) then do
        asDeclarationStatement node
      else
        Nothing

-- This is a sketch of a hyphotetical approach which uses file symbol and exports
-- from that. We could in theory use this to get all exports from file as well.
-- getRootDeclarationNodes :: Program -> Array Nodes.DeclarationStatement
-- getRootDeclarationNodes program = do
--     roots' = do
--       let
--         getExportsOfSourceFile sourceFile = do
--           moduleSymbol <- getSymbolAtLocation checker sourceFile
--           pure $ getExportsOfModule checker moduleSymbol
--         exports = map getExportsOfSourceFile rootFiles
--         getExportDeclaration symbol = do
--           let
--             { declarations } = Symbol.interface symbol
--           catMaybes $ declarations <#> \declarationNode -> do
--             if isNodeExported checker node && isNothing (asEmptyStatement node) then do
--               asDeclarationStatement node
--             else
--               Nothing
-- 
--         pure $ getDeclarationStatement checker symbol
-- 

readRootVariableStatements :: Program -> Array (Nodes.VariableStatement /\ _)
readRootVariableStatements program = do
  let
    checker = getTypeChecker program
    rootFiles = getSourceFiles program
  Array.catMaybes $ do
    rootFiles >>= getChildren >>= getChildren <#> \(node :: Node "" ()) -> do
      -- | Ignore non exported declarations and "semicolons"
      if isNodeExported checker node && isNothing (asEmptyStatement node) then do
        variableStatement <- NodesTests.asVariableStatement node
        declaration <- variableStatement `onProps` _.declarationList `onProps` _.declarations # Array.head

        typeNode <- NoProblem.toMaybe $ declaration `onProps` _."type"
        typeReferenceNode <- NodeTests.asTypeReferenceNode typeNode
        type_ <- getTypeAtLocation checker typeNode
        -- let
        --   arguments = typeReferenceNode `onProps` _.typeArguments ! []
        -- arguments' <- for arguments (getTypeAtLocation checker)
        pure $ variableStatement /\ type_ -- { type_,  arguments: arguments' }
      else
        Nothing

      -- void $ for (NodeTests.asVariableStatement node <#> Nodes.interface) \{ declarationList } -> do
      --   let
      --     { declarations } = Nodes.interface declarationList
      --   for declarations \decl -> do
      --     let
      --       { name } = Nodes.interface decl
      --     for (NodesTests.asIdentifier name <#> Nodes.interface) \{ escapedText } -> do
      --       traceM "declaration:"
      --       traceM escapedText

type Declarations = Map FullyQualifiedName { typ :: Typ (), params :: Maybe (Params (Typ ())) }

readRootDeclarations
  :: Program
  -> Declarations
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
            fqn = getExportedFullyQualifiedName checker s
          -- | TODO: Type parameters can be extracted from the `Signature`
          -- | when function type is constructed (they aren't at the moment
          -- | because I don't know the exact meaning of them.
          pure { fqn, params: Nothing, typ }
      | Just n <- Nodes.interface <$> asInterfaceDeclaration node = do
          typ <- getTypeAtLocation checker node
          s <- getSymbol typ
          let
            -- t' = readType checker t (params n) visit.onType
            fqn = getExportedFullyQualifiedName checker s
          pure { fqn, params: params n, typ } -- /\ visit.onDeclaration fqn t'
      | Just n <- Nodes.interface <$> asClassDeclaration node = do
          typ <- getTypeAtLocation checker node
          s <- getSymbol typ
          let
            -- t' = readType checker t (params n) visit.onType
            fqn = getExportedFullyQualifiedName checker s
          -- pure $ fqn /\ visit.onDeclaration fqn t'
          pure { fqn, params: params n, typ } -- /\ visit.onDeclaration fqn t'
      | Just n <- Nodes.interface <$> asTypeAliasDeclaration node = do
          typ <- getTypeAtLocation checker node
          s <- getSymbolAtLocation checker n.name
          let
            -- t' = readType checker t (params n) visit.onType
            fqn = getExportedFullyQualifiedName checker s
          -- pure $ fqn /\ visit.onDeclaration fqn t'
          pure { fqn, params: params n, typ } -- /\ visit.onDeclaration fqn t'
      | otherwise = do
          traceM "XXXXXXXXXXXXXXXXXXXXXXXXX: Trying as variable declaration"
          traceM $ NodeTests.asVariableDeclarationImpl node
          traceM node
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

-- Merging:
--   * interface type + interface type = interface type - no special handling required
--   * class + interface = merge - requires special handling
--   * namespace + namespace = ?

-- | * This "type detection" pattern matching is
-- |  order sensitive and verified by the tests suite.
-- |  There is no one to one mapping between the
-- |  internal Ts compiler types and the constructors of `Typ`.
-- |
-- | * This function is unable to detect parametric types.
-- |  It doesn't handle type references either. Why?
-- |  Because I was not able to distinguish type references from
-- |  parametric type. The simplest example is something like:
-- |
-- |  ```typescript
-- |  export class Y {};"
-- |  export class X<param=number>{ y: Y<string> }"
-- |  ```
-- |
-- |  The ts type representation (flags, symbol etc.) is
-- |  the same for `X` and `Y` in this context as far as I was
-- |  able to detect.
-- |
-- |  It is just easier to handle these two cases separatly during
-- |  unfolding.
-- |
-- | TODO: Is enum: https://stackoverflow.com/a/55406883/194614
readTopLevelType :: forall t. TypeChecker -> Typ () -> OnType t -> t
readTopLevelType checker t onType
  | isAnyType checker t = onType.any
  | Just e <- getElementTypeOfArrayType checker t = onType.array e
  | isBooleanType checker t = onType.boolean
  | Just bl <- reflectBooleanLiteralType t = onType.booleanLiteral bl
  | Just i <- asIntersectionType t = onType.intersection $ Typs.interface i # _.types
  -- | Take closer look what the type checker provides from merged types.
  -- | Just s <- asInterfaceType t *> asClassType t *> getSymbol t = do
  --     let
  --       declarations = getDeclarations s
  --       -- FIXME: I'm not sure what to do when we have
  --       -- declarations without associated symbol so
  --       -- I'm ignoring them for now.
  --       typs = catMaybes $ declarations <#> \decl -> do
  --         getTypeOfSymbolAtLocation checker s decl
  --     onType.merge typs
  | Just i <- asInterfaceType t = do
      let
        props = readProperties checker i
        bases = getBaseTypes i

      onType.interface bases (fromMaybe [] props)
  | Just i <- asClassType t = do
      let
        bases = getBaseTypes i
        props = readProperties checker i

        constructors = fromMaybe [] do
          s <- getSymbol i
          d <- Array.head $ getDeclarations s
          ct <- getTypeOfSymbolAtLocation checker s d
          let
            cs = getConstructSignatures ct
            mkConstructor sig = do
              { args } <- readSignature checker sig
              pure $ args
          for cs mkConstructor
      onType.class bases constructors (fromMaybe [] props)

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

asClassTypeRef checker t = do
  i <- asClassType t <#> Typs.interface
  let
    n = Symbol.getName i.symbol
    decls = Symbol.getDeclarations i.symbol
    fqn = getFullyQualifiedName checker i.symbol
  d <- Array.head decls >>= asDeclarationStatement
  pure (fqn /\ d)

asInterfaceTypeRef checker t = do
  i <- asInterfaceType t <#> Typs.interface
  let
    n = Symbol.getName i.symbol
    decls = Symbol.getDeclarations i.symbol
    fqn = getFullyQualifiedName checker i.symbol
  d <- Array.head decls >>= asDeclarationStatement
  pure (fqn /\ d)

readInnerType :: forall t. TypeChecker -> Typ () -> OnType t -> t
readInnerType checker t onType
  | isAnyType checker t = onType.any
  | Just e <- getElementTypeOfArrayType checker t = onType.array e
  | isBooleanType checker t = onType.boolean
  | Just bl <- reflectBooleanLiteralType t = onType.booleanLiteral bl
  | Just t' <- asTypeRef checker t onType = t'
  | Just i <- asIntersectionType t = onType.intersection $ Typs.interface i # _.types
  | Just (fqn /\ decl) <- asInterfaceTypeRef checker t = do
      onType.typeRef fqn decl
  | Just (fqn /\ decl) <- asClassTypeRef checker t = do
      onType.typeRef fqn decl
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
  readSignature checker sig

readSignature
  :: TypeChecker
  -> Signature
  -> Maybe
       { args ::
           Array
             { name :: String
             , optional :: Boolean
             , type :: Typ ()
             }
       , returnType :: Typ ()
       }
readSignature checker sig = do
  let
    param s = do
      { head: decl } <- Array.uncons <<< Symbol.getDeclarations $ s
      pd <- Nodes.interface <$> asParameterDeclaration decl
      pt <- getTypeOfSymbolAtLocation checker s decl
      pure
        { name: Symbol.getName s
        , type: pt
        , optional:
            (isJust $ NoProblem.toMaybe pd.questionToken)
              || (isJust $ NoProblem.toMaybe pd.initializer)
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
  traverse step $ getApparentProperties t
