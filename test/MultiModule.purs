module Test.MultiModule where

import Prelude

import Data.Array (cons, singleton) as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Functor.Mu (roll)
import Data.Lens (_1, over)
import Data.List (List)
import Data.Map (fromFoldable) as Map
import Data.String (Pattern(..), Replacement(..), joinWith, replace) as String
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (fromHomogeneous, toUnfoldable) as Object
import ReadDTS.AST (TsType(..)) as AST
import ReadDTS.AST (TypeRepr(..))
import ReadDTS.AST (types) as ReadDTS.AST
import Test.Compile (compile)
import Test.Unit (TestSuite, failure)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (shouldEqual)
import TypeScript.Compiler.Parser (SourceCode(..))
import TypeScript.Compiler.Types (FullyQualifiedName(..))
import TypeScript.Testing.Types (mkInMemoryFile)

testOnTypes
  :: { modules ::
         Array
           { path :: String
           , source :: String
           }
     , roots :: Array String
     }
  -> (List (FullyQualifiedName /\ TypeRepr) -> Aff Unit)
  -> TestSuite
testOnTypes opts test = do
  let
    labelStep ({ path, source: source }) = "/* "
      <> path
      <> " */"
      <> String.replace (String.Pattern "\n") (String.Replacement ";") source

    label = String.joinWith ";" $ map labelStep opts.modules

    toFile { path, source } = mkInMemoryFile path (SourceCode source)
    opts' = opts
      { modules = map toFile opts.modules
      , roots = opts.roots
      }

  Test.test label $ do
    program <- liftEffect $ compile opts'
    case ReadDTS.AST.types program of
      Right types -> test types
      Left err -> failure $ "FAILURE: " <> show err

suite :: TestSuite
suite = Test.suite "Cross module definitions" do
  let
    fromHomogeneous = (Map.fromFoldable :: Array _ -> _)
      <<< map (over _1 FullyQualifiedName)
      <<< Object.toUnfoldable
      <<< Object.fromHomogeneous

    mkModule path source = { path, source }

    testFromRootTypes rootSource modules test = do
      let
        root = { source: rootSource, path: "Root.ts" }
      testOnTypes { roots: [ "Root.ts" ], modules: Array.cons root modules } test
  do
    let
      modules = Array.singleton $ mkModule
        "A.ts"
        "export type A = { p1: number, p2: string };"

    testFromRootTypes """import { A } from "A"; export type B = { a: A }""" modules \types -> do
      shouldEqual 2 (length types)
      Map.fromFoldable types `flip shouldEqual` fromHomogeneous
        { "\"A\".A": TypeRepr $ roll $ AST.TsObject
            [ { type: roll AST.TsNumber, name: "p1", optional: false }
            , { type: roll AST.TsString, name: "p2", optional: false }
            ]
        , "\"Root\".B": TypeRepr $ roll $ AST.TsObject $ Array.singleton $
            { type: roll $ AST.TsTypeRef $ FullyQualifiedName "\"A\".A"
            , name: "a"
            , optional: false
            }
        }

-- do
--   let
--     modules = Array.singleton $ mkModule
--       "A.ts"
--       "export type A = { p1: number, p2: string };"
--   testFromRootTypes """import { A } from "A"; export type B<a=A> = { a: a }""" modules \types -> do
--     -- shouldEqual 2 (length types)
--     Map.fromFoldable types `shouldEqual` fromHomogeneous
--       { "\"A\".A": roll $ AST.TsObject
--         [ { type: roll AST.TsNumber, name: "p1", optional: false }
--         , { type: roll AST.TsString, name: "p2", optional: false }
--         ]
--       , "\"Root\".B": roll $ AST.TsObject $ Array.singleton $
--         { type: roll $ AST.TsTypeRef $ FullyQualifiedName "\"A\".A"
--         , name: "a"
--         , optional: false
--         }
--       }
