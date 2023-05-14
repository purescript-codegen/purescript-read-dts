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
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (opt)
import Debug (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Path (FilePath)
import ReadDTS.AST (TypeRepr(..))
import ReadDTS.AST (types) as ReadDTS.AST
import ReadDTS.AST.Printer (pprint)
import Test.Unit (TestSuite, failure)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (shouldEqual)
import Test.Unit.Console (print)
import TypeScript.Compiler.Program (createProgram)
import TypeScript.Compiler.Types (CompilerOptions, FullyQualifiedName(..), Program, moduleKind, moduleResolutionKind, scriptTarget)
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
    -- compilerOpts :: CompilerOptions
    compilerOpts =
      { module: opt moduleKind."ES2015"
      , moduleResolution: opt moduleResolutionKind."Bundler"
      , noEmitOnError: opt true
      , noImplicitAny: opt true
      , rootDirs: ["."]
      , target: opt scriptTarget."ES5"
      , strictNullChecks: false
      , allowSyntheticDefaultImports: opt true
      }

  -- | We have to load es library because without it we are not
  -- | able to handle even `Array` type.
  host <- liftEffect $ InSubdir.compilerHost
    { files: opts.modules
    , defaultLib: Nothing
    , dir: opts.dir
    , subhost: Nothing
    }
  createProgram opts.roots compilerOpts (Just host)

suite :: TestSuite
suite = Test.suite "Golden.ThreeJs" do
  -- Test.test "read modules of old three.js" do
  --   program <- liftEffect $ compile
  --     { dir: DirName "test/Golden/ts"
  --     , roots:
  --       [ "three/math/Matrix3.d.ts"
  --       , "three/math/Vector3.d.ts"
  --       ]
  --     , modules: []
  --     }

  --   case ReadDTS.AST.types program of
  --     Right types -> do
  --       traceM $ "Successfully read types:"
  --       -- traceM $ types -- Object.fromFoldable types
  --       for_ types \(TypeRepr t) -> do
  --         --log $ show fqn
  --         log $ pprint t
  --     Left err -> failure $ "FAILURE: " <> show err

  Test.test "read mesh module" do
    program <- liftEffect $ compile
      { dir: DirName "test/Golden/ts/three@0.152/src"
      , roots:
        [ "materials/MeshBasicMaterial.d.ts"
        -- , "three@0.156/math/Vector3.d.ts"
        ]
      , modules: []
      }

    case ReadDTS.AST.types program of
      Right types -> do
        traceM $ "Successfully read types:"
        -- traceM $ types -- Object.fromFoldable types
        -- for_ types \(TypeRepr t) -> do
        --   --log $ show fqn
        --   log $ pprint t
        log $ show $ Map.keys types
        -- (FullyQualifiedName "MeshBasicMaterialParameters" `Map.lookup` types) `shouldEqual` Nothing
        for_ (Map.lookup (FullyQualifiedName "\"materials/MeshBasicMaterial\".MeshBasicMaterialParameters") types) \(TypeRepr t) -> do
          log $ pprint t
        for_ (Map.lookup (FullyQualifiedName "\"materials/MeshBasicMaterial\".MeshBasicMaterial") types) \(TypeRepr t) -> do
          log $ pprint t
      Left err -> failure $ "FAILURE: " <> show err

