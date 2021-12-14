module ReadDTS where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (execState, modify_)
import Control.Monad.State.Trans (execStateT)
import Data.Array (elem, filter) as Array
import Data.Either (Either)
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, for, sequence, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console (info)
import ReadDTS.TypeScript (isNodeExported, showSyntaxKind)
import TypeScript.Compiler.Checker (getFullyQualifiedName, getSymbolAtLocation, getTypeAtLocation)
import TypeScript.Compiler.Checker.Internal (getElementTypeOfArrayType, isAnyType, isArrayType, isBooleanType, isNullType, isNumberType, isStringType, isUndefinedType)
import TypeScript.Compiler.Factory.NodeTests (asClassDeclaration, asInterfaceDeclaration, asTypeAliasDeclaration)
import TypeScript.Compiler.Program (getRootFileNames, getSourceFiles, getTypeChecker)
import TypeScript.Compiler.Types (FullyQualifiedName(..), Node, Program, Typ, TypeChecker)
import TypeScript.Compiler.Types.Nodes (getChildren)
import TypeScript.Compiler.Types.Nodes (interface) as Node
import TypeScript.Compiler.Types.Typs (asNumberLiteralType, asObjectType, asStringLiteralType)
import TypeScript.Compiler.Types.Typs (interface) as Typs
import TypeScript.Compiler.Types.Typs.Internal (reflectBooleanLiteralType)

foreign import data TsSourceFile :: Type

foreign import data TsDeclaration :: Type

type Application t =
  { params :: Array t
  , t :: t
  }

type Declaration =
  { fqn :: FullyQualifiedName
  , ref :: TsDeclaration
  }

type Function t =
  { params :: Array { name :: String, "type" :: t }
  , return :: t
  }

type Param :: (Type -> Type) -> Type -> Type
type Param nullable t =
  { name :: String
  , default :: nullable t
  }

sequenceParam :: forall m nullable ref t. Traversable nullable => Applicative m => Traversable t => Param nullable (t (m ref)) -> m (Param nullable (t ref))
sequenceParam { name, default: t } = { default: _, name } <$> (sequence <<< map sequence) t

type Parametric :: (Type -> Type) -> Type -> Type
type Parametric nullable t =
  { body :: t
  , params :: Array (Param nullable t)
  }

type Prop t =
  { name :: String
  , type :: t
  , optional :: Boolean
  }

sequenceProp :: forall m ref. Applicative m => Prop (m ref) -> m (Prop ref)
sequenceProp { name, type: t, optional } = { type: _, name, optional } <$> t

type Props t = Array (Prop t)

-- | XXX: Is there a way to pass `Maybe` constructors to FFI
-- | but preserving typechecking on typescript side and drop this
-- | `nullable` parameter?
type OnDeclaration d t = FullyQualifiedName -> t -> d

type OnType :: Type -> Type
type OnType t =
  { any :: t
  , array :: Typ () -> t
  , boolean :: t
  -- ,application ∷ Application t → t
  -- , array ∷ t → t
  , booleanLiteral ∷ Boolean → t
  -- , class ∷ Props t → t
  -- , function ∷ Function t → t
  -- , interface ∷ Props TsTypeNode → t
  -- , intersection ∷ Array t → t
  , object :: Props (Typ ()) -> t
  , number :: t
  , numberLiteral ∷ Number → t
  , null :: t
  -- , parametric ∷ Parametric nullable t → t
  -- , primitive ∷ String → t
  -- , ref ∷ Declaration → t
  , string :: t
  , stringLiteral ∷ String → t
  -- , tuple ∷ Array t → t
  , undefined ∷ t
  -- , union ∷ Array t → t
  , unknown ∷ Typ () → t
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

type Declarations d =
  { topLevel :: Array d
  , readDeclaration :: TsDeclaration -> Effect d
  }

-- type TypRef =
--   { ref ∷ TS.Typ
--   , topLevel ∷ Boolean
--   }

readRootsDeclarations :: forall d t. Program -> Visit d t -> Map FullyQualifiedName d
readRootsDeclarations program visit = do
  let
    checker = getTypeChecker program
    rootNames = getRootFileNames program
    fileName = Node.interface >>> _.fileName
    rootFiles = Array.filter ((\fn -> fn `Array.elem` rootNames) <<< fileName) $ getSourceFiles program
  -- | `SourceFile` "usually" has as a single root child of type `SyntaxList`.
  -- | * We are not interested in this particular child.
  -- | * We should probably recurse into any container like
  -- node (`Block`, `ModuleDeclaration` etc.) down the stream too.
  flip execState Map.empty $ (rootFiles >>= (getChildren >=> getChildren)) # traverse_ \node -> do
      when (isNodeExported checker node) do
        traceM $ "Reading node: " <> showSyntaxKind node
        case readDeclaration checker node visit of
          Just (fqn /\ d) -> modify_ (Map.insert fqn d)
          Nothing -> do
            traceM "Unable to parse node as declaration. Skipping node..."

readDeclaration :: forall r d t. TypeChecker -> Node r -> Visit d t -> Maybe (FullyQualifiedName /\ d)
readDeclaration checker node visit
  | Just n <- Node.interface <$> asInterfaceDeclaration node = do
      traceM "Trying to parse interface"
      Nothing
  | Just c <- Node.interface <$> asClassDeclaration node = do
      traceM "Trying to class"
      Nothing
  | Just n <- Node.interface <$> asTypeAliasDeclaration node = do
      case getSymbolAtLocation checker n.name, getTypeAtLocation checker node of
        Just s, Just t -> do
          traceM $ "Type alias with symbol..."
          let
            t' = readType checker t visit.onType
            fqn = getFullyQualifiedName checker s
          pure $ fqn /\ visit.onDeclaration fqn t'
        _, _ -> Nothing
  | otherwise = do
      Nothing

readType :: forall t. TypeChecker -> Typ () -> OnType t -> t
readType checker t onType
  | isAnyType checker t = onType.any
  | Just e <- getElementTypeOfArrayType checker t = onType.array e
  | Just e <- getElementTypeOfArrayType checker t = onType.array e
  | isBooleanType checker t = onType.boolean
  | Just b <- reflectBooleanLiteralType t = onType.booleanLiteral b
  | isNullType checker t = onType.null
  | isNumberType checker t = onType.number
  | Just o <- asObjectType t = onType.object []
  | Just n <- asNumberLiteralType t = onType.numberLiteral (Typs.interface n # _.value)
  | isStringType checker t = onType.string
  | Just s <- asStringLiteralType t = onType.stringLiteral (Typs.interface s # _.value)
  | isUndefinedType checker t = onType.undefined
  | otherwise = onType.unknown t


-- readDTS
--   ∷ ∀ d t
--   . Options
--   → Visit d t
--   → Array RootName
--   → Array InMemoryFile
--   → TypeScript.Types.CompilerHost
--   → Effect (Either (Array String) (Declarations d))
-- readDTS opts visit rootNames inMemoryFiles host =
--   (runEffectFn1 _readTypes)
--     { options: opts
--     , visit: visit'
--     , rootNames
--     , inMemoryFiles
--     , compilerHost: host
--     , either: { left: Left, right: Right }
--     }
--   where
--     _source = prop (SProxy ∷ SProxy "source")
-- 
--     visit' = visit
--     --   = over (_onTypeNodeL <<< _parametricL) (lcmap (over (_params <<< traversed <<< _defaultL) toMaybe))
--     --   $ visit
-- 
--     _onTypeNodeL ∷ ∀ a b r. Lens { onTypeNode ∷ a | r } { onTypeNode ∷ b | r } a b
--     _onTypeNodeL = prop (SProxy ∷ SProxy "onTypeNode")
-- 
--     _parametricL ∷ ∀ a b r. Lens { parametric ∷ a | r } { parametric ∷ b | r } a b
--     _parametricL = prop (SProxy ∷ SProxy "parametric")
-- 
--     _defaultL ∷ ∀ a b r. Lens { default ∷ a | r } { default ∷ b | r } a b
--     _defaultL = prop (SProxy ∷ SProxy "default")
-- 
--     _params ∷ ∀ a b r. Lens { params ∷ a | r } { params ∷ b | r } a b
--     _params = prop (SProxy ∷ SProxy "params")
-- 
-- -- type EitherConstructors =
-- --   { left ∷ ∀ err a. err → Either err a
-- --   , right ∷ ∀ err a. a → Either err a
-- --   }
-- -- foreign import _readTypes
-- --   ∷ ∀ d t
-- --   . EffectFn1
-- --       { options ∷ Options
-- --       , visit ∷ (VisitBase Nullable d t)
-- --       , rootNames ∷ Array String
-- --       , inMemoryFiles ∷ Array InMemoryFile
-- --       , compilerHost ∷ TypeScript.Types.CompilerHost
-- --       , either ∷ EitherConstructors
-- --       }
-- --       (Either (Array String) (Declarations d))
