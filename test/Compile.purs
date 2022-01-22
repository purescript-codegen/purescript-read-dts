module Test.Compile where

import Prelude

import Data.List (filter, head) as List
import Data.Map (Map)
import Data.Map (toUnfoldableUnordered) as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Data.Undefined.NoProblem (Opt, (!))
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as Closed
import Data.Undefined.NoProblem.Closed (coerce) as NoProblem
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import ReadDTS (Param, readRootDeclarations)
import TypeScript.Compiler.Parser (SourceCode(..))
import TypeScript.Compiler.Program (createProgram)
import TypeScript.Compiler.Types (CompilerOptions, moduleKind, scriptTarget, FullyQualifiedName(..), Program, Typ)
import TypeScript.Testing.InMemory (File, compilerHost, mkFile) as InMemory

type SourceLine = String

mkFile' :: FilePath -> Array SourceLine -> InMemory.File
mkFile' name = InMemory.mkFile name <<< SourceCode <<< String.joinWith "\n"

newtype TypeName = TypeName String

type CompileOpts =
  { roots :: Array FilePath
  , modules :: Array InMemory.File
  , strictNullChecks :: Opt Boolean
  }

compile
  :: forall opts
  . Closed.Coerce opts CompileOpts
  => opts
  -> Effect Program
compile opts = do
  let
    opts' = Closed.coerce opts :: CompileOpts
    compilerOpts :: CompilerOptions
    compilerOpts = NoProblem.coerce
      { module: moduleKind."ES2015"
      , target: scriptTarget."ES5"
      , strictNullChecks: opts'.strictNullChecks ! false
      }

  -- | We have to load es library because without it we are not
  -- | able to handle even `Array` type.
  host <- liftEffect $ InMemory.compilerHost { files: opts'.modules }
  createProgram opts'.roots compilerOpts (Just host)

newtype StrictNullChecks = StrictNullChecks Boolean

-- | Compile source code and extract particular type information.
compileType ::
  TypeName ->
  SourceCode ->
  Aff
    { program :: Program
    , type :: Maybe
      { typ :: Typ ()
      , params :: Array (Param (Typ ()))
      }
    }
compileType (TypeName name) source = do
  let
    rootName = "Root.ts"
    fqn = FullyQualifiedName $ "\"Root\"." <> name

    getType ::
      forall t.
      Map FullyQualifiedName t ->
      Maybe (FullyQualifiedName /\ t)
    getType
      = List.head
      <<< List.filter (eq fqn <<< fst)
      <<< Map.toUnfoldableUnordered

    rootFile = InMemory.mkFile rootName source
  program <- liftEffect $ compile
    { roots: [rootName]
    , modules: [rootFile]
    , strictNullChecks: false
    }
  let
    decls = readRootDeclarations program
  pure { type: snd <$> getType decls, program }

