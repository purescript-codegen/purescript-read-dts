module Test.Golden.ThreeJs where

-- | Testing version: "@types/three": "^0.137.0"
-- |
-- | Currenly testing a subset of three.js modules.
-- |
-- | These files were drastically simplified to cut out
-- | inclusion of additional ts modules:
-- |
-- | * `core/Object3D.d.ts`
-- | * `objects/Sprite.d.ts`

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (opt)
import Debug (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Path (FilePath)
import ReadDTS.AST (types) as ReadDTS.AST
import ReadDTS.AST.Printer (pprint)
import Test.Unit (TestSuite, failure)
import Test.Unit (suite, test) as Test
import TypeScript.Compiler.Program (createProgram)
import TypeScript.Compiler.Types (CompilerOptions, Program, moduleKind, scriptTarget)
import TypeScript.Testing.Subdir (DirName(..))
import TypeScript.Testing.Subdir (compilerHost) as InSubdir
import TypeScript.Testing.Types (InMemoryFile)

type CompileOpts =
  { roots :: Array FilePath
  , modules :: Array InMemoryFile
  , dir :: DirName
  }

compile
  :: CompileOpts
  -> Effect Program
compile opts = do
  let
    compilerOpts :: CompilerOptions
    compilerOpts =
      { module: opt moduleKind."ES2015"
      , target: opt scriptTarget."ES5"
      , strictNullChecks: false
      }

  -- | We have to load es library because without it we are not
  -- | able to handle even `Array` type.
  host <- liftEffect $ InSubdir.compilerHost { files: opts.modules, defaultLib: Nothing, dir: opts.dir }
  createProgram opts.roots compilerOpts (Just host)

suite :: TestSuite
suite = Test.suite "Cross module definitions" do
  Test.test "read modules" do
    -- program <- liftEffect $ compile { dir: DirName "test/Libs/ts", roots: [ "three/math/Vector3.d.ts", "three/math/Matrix3.d.ts" ], modules: [] }
    program <- liftEffect $ compile { dir: DirName "test/Libs/ts", roots: [ "three/math/Matrix3.d.ts" ], modules: [] }

    case ReadDTS.AST.types program of
      Right types -> do
        for_ types \(fqn /\ t) -> do
          log $ show fqn
          log $ pprint t
      Left err -> failure $ "FAILURE: " <> show err
