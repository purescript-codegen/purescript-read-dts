module Test.Compile where

import Prelude

import Data.List (filter, head) as List
import Data.Map (Map)
import Data.Map (toUnfoldableUnordered) as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Data.Undefined.NoProblem (Opt, opt, (!))
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as Closed
import Data.Undefined.NoProblem.Closed (coerce) as NoProblem
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Path (FilePath)
import ReadDTS (Params, Declarations, readRootDeclarations)
import TypeScript.Class.Compiler.Program as Program
import TypeScript.Compiler.Parser (SourceCode(..))
import TypeScript.Compiler.Program (createProgram)
import TypeScript.Compiler.Types (CompilerOptions, FullyQualifiedName(..), ModuleResolutionKind, Program, Typ, moduleKind, scriptTarget)
import TypeScript.Compiler.Types.Nodes as Nodes
import TypeScript.Testing.InMemory (compilerHost) as InMemory
import TypeScript.Testing.Types (InMemoryFile, mkInMemoryFile)

type SourceLine = String

mkFile' :: FilePath -> Array SourceLine -> InMemoryFile
mkFile' name = mkInMemoryFile name <<< SourceCode <<< String.joinWith "\n"

newtype TypeName = TypeName String

type CompileOpts =
  { allowSyntheticDefaultImports :: Opt Boolean
  , roots :: Array FilePath
  , modules :: Array InMemoryFile
  , moduleResolution :: Opt ModuleResolutionKind
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
      , moduleResolution: opts'.moduleResolution
      , rootDirs: ["./src"]
      , target: scriptTarget."ES5"
      , strictNullChecks: opts'.strictNullChecks ! false
      , allowSyntheticDefaultImports: opt true
      }

  host <- liftEffect $ InMemory.compilerHost
    { files: opts'.modules
    , defaultLib: Nothing
    , subhost: Nothing
    }

  -- Experimenting with default compiler host now
  -- host <- liftEffect $ Program.createCompilerHost compilerOpts
  createProgram opts'.roots compilerOpts (Just host)

newtype StrictNullChecks = StrictNullChecks Boolean

-- | Compile source code and extract particular type information.
compileType
  :: TypeName
  -> SourceCode
  -> Aff
       { program :: Program
       , type :: Maybe
           { typ :: Typ ()
           , params :: Maybe (Params (Typ ()))
           }
        , decls :: Declarations
       }
compileType (TypeName name) source = do
  let
    rootName = "Root.ts"
    fqn = FullyQualifiedName $ "\"Root\"." <> name

    getType
      :: forall t
       . Map FullyQualifiedName t
      -> Maybe (FullyQualifiedName /\ t)
    getType = List.head
      <<< List.filter (eq fqn <<< fst)
      <<< Map.toUnfoldableUnordered

    rootFile = mkInMemoryFile rootName source
  program <- liftEffect $ compile
    { roots: [ rootName ]
    , modules: [ rootFile ]
    , strictNullChecks: false
    }
  let
    decls = readRootDeclarations program
  pure { type: snd <$> getType decls, program, decls }
