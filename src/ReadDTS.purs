module ReadDTS where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.State.Trans (execStateT)
import Data.Array (elem)
import Data.Map (Map)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse_)
import Data.Tuple.Nested (type (/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console (info)
import ReadDTS.TypeScript (isNodeExported, showSyntaxKind)
import TypeScript.Compiler.Checker (getFullyQualifiedName, getSymbolAtLocation, getTypeAtLocation)
import TypeScript.Compiler.Program (getRootFileNames, getSourceFiles, getTypeChecker)
import TypeScript.Compiler.Types (Program, Node, Typ, TypeChecker)
import TypeScript.Compiler.Types.Nodes (getChildren)
import TypeScript.Compiler.Types.Nodes (interface) as Node
import TypeScript.Compiler.Factory.NodeTests (asClassDeclaration, asInterfaceDeclaration, asTypeAliasDeclaration)

foreign import data TsSourceFile :: Type

foreign import data TsDeclaration :: Type

newtype FullyQualifiedName = FullyQualifiedName String

derive instance eqFullyQualifiedName :: Eq FullyQualifiedName
derive instance ordFullyQualifiedName :: Ord FullyQualifiedName
derive newtype instance showFullyQualifiedName :: Show FullyQualifiedName

fqnToString :: FullyQualifiedName -> String
fqnToString (FullyQualifiedName s) = s

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
  -- ,application ∷ Application t → t
  -- , array ∷ t → t
  -- , booleanLiteral ∷ Boolean → t
  -- , class ∷ Props t → t
  -- , function ∷ Function t → t
  -- , interface ∷ Props TsTypeNode → t
  -- , intersection ∷ Array t → t
  , object :: Props (Typ ()) -> t
  -- -- , null ∷ t
  -- , parametric ∷ Parametric nullable t → t
  -- , primitive ∷ String → t
  -- , ref ∷ Declaration → t
  -- , numberLiteral ∷ Number → t
  -- , stringLiteral ∷ String → t
  -- , tuple ∷ Array t → t
  -- -- , undefined ∷ t
  -- , union ∷ Array t → t
  -- , unknown ∷ String → t
  }

type Visit d t =
  { onTyp :: OnType t
  , onDeclarationNode :: OnDeclaration d t
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

readRootsDeclarations :: forall d t. Program -> Visit d t -> ExceptT String Effect (Map FullyQualifiedName d)
readRootsDeclarations program visit = do
  let
    checker = getTypeChecker program
    rootNames = getRootFileNames program
  flip execStateT Map.empty $ getSourceFiles program # traverse_ \sourceFile -> do
    let
      fileName = Node.interface sourceFile # _.fileName
    when (fileName `elem` rootNames) do
      -- `SourceFile` "usually" has as a single root child of type `SyntaxList`.
      -- * We are not interested in this particular child.
      -- * We should probably recurse into any container like
      -- node (`Block`, `ModuleDeclaration` etc.) down the stream too.
      (getChildren sourceFile >>= getChildren) # traverse_ \node -> do
        traceM $ node
        when (isNodeExported checker node) do
          info $ "Trying to read node as declaration: " <> showSyntaxKind node
          case readDeclaration checker node visit of
            Just d -> do
              traceM "Declaration parsed successfully"
            Nothing -> traceM "Declaration parse failure"

readDeclaration :: forall r d t. TypeChecker -> Node r -> Visit d t -> Maybe (FullyQualifiedName /\ d)
readDeclaration checker node visit
  | Just n <- Node.interface <$> asInterfaceDeclaration node = do
      traceM "Trying to parse interface"
      Nothing
  | Just c <- Node.interface <$> asClassDeclaration node = do
      traceM "Trying to class"
      Nothing
  | Just n <- Node.interface <$> asTypeAliasDeclaration node = do
      traceM "Trying to parse type alias"
      case getSymbolAtLocation checker n.name, getTypeAtLocation checker node of
        Just s, Just t -> do
          traceM $ "Type alias with symbol: " <> getFullyQualifiedName checker s
          t' <- readType checker t visit
          Nothing
        _, _ -> traceM "No symbol?"
      Nothing
  | otherwise = do
      Nothing

readType :: forall d r t. TypeChecker -> Typ r -> Visit d t -> Maybe t
readType checker t visit = do
  Nothing
--
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
