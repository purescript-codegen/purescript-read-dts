module Test.MultiModule where

import Prelude

import Data.Array (cons, singleton) as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Functor.Mu (roll)
import Data.Lens (_1, over)
import Data.Map (empty, fromFoldable) as Map
import Data.String (Pattern(..), Replacement(..), joinWith, replace) as String
import Debug (traceM)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object (fromHomogeneous, toUnfoldable) as Object
import ReadDTS.AST (RootDeclarations, TypeRepr(..))
import ReadDTS.AST (TsType(..)) as AST
import ReadDTS.AST (types) as ReadDTS.AST
import Test.Compile (compile)
import Test.Unit (TestSuite, failure)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (shouldEqual)
import TypeScript.Compiler.Parser (SourceCode(..))
import TypeScript.Compiler.Program (getPreEmitDiagnostics)
import TypeScript.Compiler.Types (FullyQualifiedName(..))
import TypeScript.Compiler.Types.Diagnostics (Diagnostic(..))
import TypeScript.Testing.Types (mkInMemoryFile)

testOnTypes
  :: { modules ::
         Array
           { path :: String
           , source :: String
           }
     , roots :: Array String
     }
  -> ({ types :: RootDeclarations, diagnostics :: Array Diagnostic } -> Aff Unit)
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
    let
      diagnostics = getPreEmitDiagnostics program

    case ReadDTS.AST.types program of
      Right types -> test { types, diagnostics }
      Left err -> failure $ "FAILURE: " <> show err

testFromRootTypes rootSource modules test = do
  let
    root = { source: rootSource, path: "Root.ts" }
  testOnTypes { roots: [ "Root.ts" ], modules: Array.cons root modules } test

-- We need this polymorphic
fromHomogeneous = (Map.fromFoldable :: Array _ -> _)
  <<< map (over _1 FullyQualifiedName)
  <<< Object.toUnfoldable
  <<< Object.fromHomogeneous

suite :: TestSuite
suite = Test.suite "Cross module definitions" do
  let
    mkModule path source = { path, source }
  do
    let
      modules = Array.singleton $ mkModule
        "A.ts"
        "export type A = { p1: number, p2: string };"

    testFromRootTypes """import { A } from "A"; export type B = { a: A }""" modules \{ types } -> do
      length types `shouldEqual` 2
      types `flip shouldEqual` fromHomogeneous
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

  do
    let
      -- Don't ask me why... but there is non standard way to export stuff and react
      -- actually uses this approach :-(
      -- https://gist.github.com/chrisgervang/59ed046c0a8d7c3a1be1b3416f8f2466
      modules = Array.singleton $ mkModule "React.d.ts" $ String.joinWith " "
        [ "export = React; "
        , "export as namespace React;"
        , "declare namespace React {"
        , "  type Key = string | number;"
        , "  type Irrelevant = number;"
        , "};"
        ]
    testFromRootTypes """import React from "React"; export type X = { key: React.Key }""" modules \{ types } -> do
      length types `shouldEqual` 2
      types `flip shouldEqual` fromHomogeneous
        { "React.Key": TypeRepr $ roll $ AST.TsUnion [ roll AST.TsString, roll AST.TsNumber ]
        , "\"Root\".X": TypeRepr $ roll $ AST.TsObject $ Array.singleton $
            { type: roll $ AST.TsTypeRef $ FullyQualifiedName "React.Key"
            , name: "key"
            , optional: false
            }
        }

  -- Value declation / export is not supported yet
  -- do
  --   testFromRootTypes """export default { prop: "test" }""" [] \{ types } -> do
  --     length types `shouldEqual` 1
  --     types `flip shouldEqual` fromHomogeneous
  --       { "React.Key": TypeRepr $ roll $ AST.TsUnion [ roll AST.TsString, roll AST.TsNumber ]
  --       , "\"Root\".X": TypeRepr $ roll $ AST.TsObject $ Array.singleton $
  --           { type: roll $ AST.TsTypeRef $ FullyQualifiedName "React.Key"
  --           , name: "key"
  --           , optional: false
  --           }
  --       }

  do
    let
      -- expected = roll $ AST.TsInterface $ Array.singleton { name: "x", optional: false, type: roll (AST.TsArray $ roll AST.TsNumber) }
      source = String.joinWith " "
        [ "namespace X {"
        , "  export interface Y { y: number[] }"
        , "}"
        , "export class X {"
        , " m: X.Y; "
        , "}"
        ]

    testFromRootTypes source [] \{ types } -> do
      types `flip shouldEqual` fromHomogeneous
        { "\"Root\".X": TypeRepr $ roll $ AST.TsClass
            { bases: []
            , constructors: [[]]
            , props: [ { name: "m", optional: false, type: roll $ AST.TsTypeRef $ FullyQualifiedName "X.Y" } ]
            }
        , "X.Y": TypeRepr $ roll $ AST.TsInterface
            { bases: []
            , props: [ { name: "y", optional: false, type: roll $ AST.TsArray $ roll AST.TsNumber } ]
            }
        }

-- | FIXME: merging of `class` and `namespace` is not merged automatically.
-- do
--   let
--     -- expected = roll $ AST.TsInterface $ Array.singleton { name: "x", optional: false, type: roll (AST.TsArray $ roll AST.TsNumber) }
--     source = String.joinWith " "
--       [ "export namespace X {"
--       , "  export interface Y { y: number[] }"
--       , "}"
--       , "export class X extends X.Y {"
--       , "}"
--       ]
--   testFromRootTypes source [] \types -> do
--     let
--       -- FIXME: 
--       expected = fromHomogeneous
--         { "\"Root\".X": TypeRepr $ roll $ AST.TsObject
--             [ { type: roll AST.TsNumber, name: "p1", optional: false }
--             , { type: roll AST.TsString, name: "p2", optional: false }
--             ]
--         , "\"Root\".X.Y": TypeRepr $ roll $ AST.TsObject $ Array.singleton $
--             { type: roll $ AST.TsTypeRef $ FullyQualifiedName "\"Root\".X.Y"
--             , name: "a"
--             , optional: false
--             }
--         }
--     types `shouldEqual` expected

-- -- do
-- --   let
-- --     modules = Array.singleton $ mkModule
-- --       "A.ts"
-- --       "export type A = { p1: number, p2: string };"
-- --   testFromRootTypes """import { A } from "A"; export type B<a=A> = { a: a }""" modules \types -> do
-- --     -- shouldEqual 2 (length types)
-- --     Map.fromFoldable types `shouldEqual` fromHomogeneous
-- --       { "\"A\".A": roll $ AST.TsObject
-- --         [ { type: roll AST.TsNumber, name: "p1", optional: false }
-- --         , { type: roll AST.TsString, name: "p2", optional: false }
-- --         ]
-- --       , "\"Root\".B": roll $ AST.TsObject $ Array.singleton $
-- --         { type: roll $ AST.TsTypeRef $ FullyQualifiedName "\"A\".A"
-- --         , name: "a"
-- --         , optional: false
-- --         }
-- --       }
