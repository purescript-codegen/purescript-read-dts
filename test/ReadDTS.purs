module Test.ReadDTS where

import Prelude

import Control.Monad.Except (runExcept, runExceptT)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, for_)
import Data.List ((:))
import Data.List (List(..), singleton) as List
import Data.Map (empty, fromFoldable, singleton, values) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Set (fromFoldable) as Set
import Data.String (joinWith)
import Data.String (joinWith) as String
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Undefined.NoProblem (undefined)
import Data.Undefined.NoProblem.Closed (coerce) as NoProblem
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import JS.Unsafe.Stringify (unsafeStringify)
import Matryoshka (cata)
import ReadDTS (Declarations, FullyQualifiedName(..), OnDeclaration, OnType, TsDeclaration, Visit, fqnToString, readRootsDeclarations)
import ReadDTS (FullyQualifiedName(..), defaults) as ReadDTS
import ReadDTS.AST (KnownDeclarations) as AST
import ReadDTS.TypeScript.Testing (handleMemoryFiles, inMemoryCompilerHost) as Testing
import Test.Unit (failure, suite, test) as Test
import Test.Unit.Assert (equal) as Assert
import TypeScript.Class.Compiler.Program (createCompilerHost, createProgram)
import TypeScript.Compiler.Types (CompilerOptions, scriptTarget)
import Unsafe.Coerce (unsafeCoerce)

file name source =
  { path: name
  , source: source
  }

file' name = file name <<< String.joinWith "\n"

type RootModule = String

-- readTopLevel ∷ CompilerHost → RootModule → Array ReadDTS.InMemoryFile → Aff _ -- (Array _ /\ AST.KnownDeclarations)
-- readTopLevel host root files = do
--   liftEffect (ReadDTS.AST.visit (ReadDTS.defaults { debug = false }) [root] files host) >>= case _ of
--     Right result → do
--       pure result
--     Left errs → do
--       Test.failure ("Compilation failed: " <> String.joinWith "\n" errs)
--       pure { topLevel: [], declarations: Map.empty }

-- https://www.typescriptlang.org/docs/handbook/declaration-files/templates/module-d-ts.html
suite compilerHost = do
  Test.suite "Simple types" do
    Test.test "export type X = {};\nexport type Y = number;" do
      let
        opts :: CompilerOptions
        opts = NoProblem.coerce { module: undefined, target: scriptTarget."ES5", strictNullChecks: true }
        rootName = "Root.ts"
        rootFile = file rootName "export type X = number"
        visit =
          { onDeclarationNode: \_ _ -> unit
          , onTyp:
              { any: "any"
              , object: \_ -> "object"
              }
          }
      res <- liftEffect $ do
        host <- Testing.inMemoryCompilerHost [ rootFile ]
        program <- createProgram [ rootName ] opts (Just host)

        runExceptT $ readRootsDeclarations program visit
      traceM res
      pure unit

--   let
--     readTopLevel' = readTopLevel compilerHost
--   Test.suite "Simple types" do
--     Test.test "type X = any" do
--       let
--         root = file "Root.ts" "export type X = any"
--         fullyQualifiedName = ReadDTS.FullyQualifiedName "\"Root\".X"
--         expected = Map.singleton fullyQualifiedName $ AST.TypeDeclaration
--           { fullyQualifiedName
--           , type: AST.Any
--           }
--       { topLevel, declarations } ← readTopLevel' "Root.ts" [root]
--       Assert.equal expected declarations
--       Assert.equal [ fullyQualifiedName ] topLevel
-- 
--    Test.test "interface X {}" do
--      let
--        root = file "Root.ts" "export interface X {}"
--        expected = List.singleton $ AST.Interface
--          { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"Root\".X"
--          , props: []
--          }
--      readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
--     Test.test "single non empty declaration" do
--       let
--         file = dts "export interface NonEmpty { x : string }"
--         expected = List.singleton $ AST.Interface
--           { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"inMemory\".NonEmpty"
--           , name: "NonEmpty"
--           , properties: [{ name: "x", optional: false, type: AST.String }]
--           , typeParameters: []
--           }
--       readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
--
--    Test.test "type X = Y" do
--      let
--        root = file'
--          "Root.ts"
--          [ "export type Y = any"
--          , "export type X = External.Y"
--          ]
--        fullyQualifiedName = ReadDTS.FullyQualifiedName "\"Root\".X"
--        expected = Map.singleton fullyQualifiedName $ AST.TypeDeclaration
--          { fullyQualifiedName
--          , type: AST.Any
--          }
--      { topLevel, declarations } ← readTopLevel' "Root.ts" [root]
--      Assert.equal expected declarations
--      Assert.equal [ fullyQualifiedName ] topLevel
--
--    Test.test "type X = External.Y" do
--      let
--        ext = file "External.ts" "export type Y = any"
--        root = file'
--          "Root.ts"
--          [ "import * as External from External"
--          , "type X = External.Y"
--          ]
--        xFqn = ReadDTS.FullyQualifiedName "\"Root\".X"
--        yFqn = ReadDTS.FullyQualifiedName "\"Root\".Y"
--        expected = Map.fromFoldable
--          $ (xFqn /\ AST.TypeDeclaration { fullyQualifiedName: xFqn, type: AST.Any })
--          : (yFqn /\ AST.TypeDeclaration { fullyQualifiedName: yFqn, type: AST.Any })
--          : List.Nil
--
--      { topLevel, declarations } ← readTopLevel' "Root.ts" [root, ext]
--      Assert.equal expected declarations
--      Assert.equal [ xFqn, yFqn ] topLevel
--
--  Test.suite "Exported symbol" do
--    Test.suite "is a type alias declaration" $ do
--      Test.test "of empty object type" do
--        let
--          file = dts "export type Empty = {}"
--          fullyQualifiedName = ReadDTS.FullyQualifiedName "\"inMemory\".Empty"
--          decl = AST.TypeAlias
--            { fullyQualifiedName
--            , name: "Empty"
--            , type: AST.AnonymousObject []
--            , typeParameters: []
--            }
--        topLevel /\ declarations ← readTopLevel' file
--        Assert.equal topLevel [ fullyQualifiedName ]
--        Assert.equal declarations $ Map.singleton fullyQualifiedName decl
--
--       Test.test "of not empty object type" do
--         let
--           file = dts'
--             [ "export type Point ="
--             , " { x: number"
--             , " , y: number"
--             , " }"
--             ]
--           fullyQualifiedName = ReadDTS.FullyQualifiedName "\"inMemory\".Point"
--           expected = List.singleton $ AST.TypeAlias
--             { fullyQualifiedName
--             , name: "Point"
--             , type: AST.AnonymousObject
--                 [ { name: "x", optional: false, type: AST.Number }
--                 , { name: "y", optional: false, type: AST.Number }
--                 ]
--             , typeParameters: []
--             }
--         readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
--       Test.test "of union of pritive types" do
--         let
--           file = dts "export type U = string | number"
--           fullyQualifiedName = ReadDTS.FullyQualifiedName "\"inMemory\".U"
--           expected = List.singleton $ AST.TypeAlias
--             { name: "U"
--             , fullyQualifiedName
--             , type: AST.Union [AST.String, AST.Number]
--             , typeParameters: []
--             }
--         readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
-- 
--       Test.test "of union of pritive types and object" do
--         let
--           file = dts "export type U = string | number | { x: number }"
--           fullyQualifiedName = ReadDTS.FullyQualifiedName "\"inMemory\".U"
--           expected = List.singleton $ AST.TypeAlias
--             { name: "U"
--             , fullyQualifiedName
--             , type: AST.Union
--                 [ AST.String
--                 , AST.Number
--                 , AST.AnonymousObject
--                   [{ name: "x", optional: false, type: AST.Number }]
--                 ]
--             , typeParameters: []
--             }
--         readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
-- 
--       Test.test "of union of type literals" do
--         let
--           fullyQualifiedName = ReadDTS.FullyQualifiedName "\"inMemory\".U"
--           file = dts """export type U = "stringLiteral" | 8"""
--           expected = List.singleton $ AST.TypeAlias
--             { name: "U"
--             , fullyQualifiedName
--             , type: AST.Union
--                 [ AST.StringLiteral "stringLiteral"
--                 , AST.NumberLiteral 8.0
--                 ]
--             , typeParameters: []
--             }
--         readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
-- 
--       Test.test "of union containing direct type recursion" do
--         let
--           file = dts "export type A = { self: A, x: number }"
--           expected
--             = Set.fromFoldable
--             $ ( AST.TypeAlias
--                 { fullyQualifiedName: FullyQualifiedName "\"inMemory\".A"
--                 , name: "A"
--                 , type: AST.AnonymousObject
--                   [ { name: "self"
--                     , optional: false
--                     , type: AST.Application
--                       { constructor: FullyQualifiedName "\"inMemory\".A"
--                       , params: []
--                       }
--                     }
--                   , { name: "x"
--                     , optional: false
--                     , type: AST.Number
--                     }
--                   ]
--                 , typeParameters: []
--                 }
--               )
--             : List.Nil
-- 
--         readTopLevel' file >>= snd >>> Map.values >>> Set.fromFoldable >>> Assert.equal expected
-- 
--       Test.test "of union with type alias reference" do
--         let
--           file = dts $ String.joinWith "\n"
--             [ "export type A = { x: number }"
--             , "export type B = A | number"
--             ]
--           expected
--             = Set.fromFoldable
--             $ ( AST.TypeAlias
--                 { fullyQualifiedName: FullyQualifiedName "\"inMemory\".A"
--                 , name: "A"
--                 , type: AST.AnonymousObject [{ name: "x", optional: false, type: AST.Number }]
--                 , typeParameters: []
--                 }
--               )
--             : ( AST.TypeAlias
--                 { fullyQualifiedName: FullyQualifiedName "\"inMemory\".B"
--                 , name: "B"
--                 , type: AST.Union [ AST.Number, AST.Application { constructor: FullyQualifiedName "\"inMemory\".A", params: [] }]
--                 , typeParameters: []
--                 }
--               )
--             : List.Nil
-- 
--         readTopLevel' file >>= snd >>> Map.values >>> Set.fromFoldable >>> Assert.equal expected
-- 
--       Test.test "of union with parametric type alias reference" do
--         let
--           file = dts $ String.joinWith "\n"
--             [ "export type A<X> = { x: X }"
--             , "export type B = A<string> | number"
--             ]
--           expected = Set.fromFoldable
--             [ AST.TypeAlias
--               { fullyQualifiedName: FullyQualifiedName "\"inMemory\".A"
--               , name: "A"
--               , type: AST.AnonymousObject
--                 [{ name: "x", optional: false, type: AST.TypeParameter { default: Nothing, name: "X" } }]
--               , typeParameters: [{ default: Nothing, name: "X" }]
--               }
--             , AST.TypeAlias
--               { fullyQualifiedName: FullyQualifiedName "\"inMemory\".B"
--               , name: "B"
--               , type: AST.Union
--                 [ AST.Number
--                 , AST.Application
--                   { constructor: FullyQualifiedName "\"inMemory\".A"
--                   , params: [ AST.String ]
--                   }
--                 ]
--               , typeParameters: []
--               }
--             ]
-- 
-- 
--         readTopLevel' file >>= snd >>> Map.values >>> Set.fromFoldable >>> Assert.equal expected
-- 
-- 
--        -- | TODO: We should "somehow" handle type reference in this test
--        -- | so it makes more sens.
--       Test.test "of union of self recursive monomorphic type" do
--         let
--          treeFqn = ReadDTS.FullyQualifiedName "\"inMemory\".Tree"
--          file = dts "export type Tree = { left: Tree, right: Tree } | number"
--          expected
--           = AST.TypeAlias
--             { fullyQualifiedName: treeFqn
--             , name: "Tree"
--             , type: AST.Union
--               [ AST.Number
--               , AST.AnonymousObject
--                 [ { name: "left"
--                   , optional: false
--                   , type: AST.Application
--                     { constructor: treeFqn, params: [] }
--                   }
--                 , { name: "right"
--                   , optional: false
--                   , type: AST.Application
--                     { constructor: treeFqn, params: [] }
--                   }
--                 ]
--               ]
--             , typeParameters: []
--             }
--           : List.Nil
-- 
--         readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
-- 
--   Test.suite "Class declaration" $ do
--     Test.test "which is empty" do
--       let
--         file = dts "export class Empty {}"
--         expected = List.singleton $ AST.Class
--           { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"inMemory\".Empty"
--           , name: "Empty"
--           , properties: []
--           , typeParameters: []
--           }
--       readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
--     Test.test "which is non empty" do
--       let
--         file = dts "export class NonEmpty { x : string }"
--         expected = List.singleton $ AST.Class
--           { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"inMemory\".NonEmpty"
--           , name: "NonEmpty"
--           , properties: [{ name: "x", optional: false, type: AST.String }]
--           , typeParameters: []
--           }
--       readTopLevel' file >>= snd >>> Map.values >>> Assert.equal expected
-- 
--   Test.suite "Interface" $ do
-- -- --    Test.test "merge of two non empty declarations" do
-- -- --      let
-- -- --        file = dts'
-- -- --          [ "export interface SomeInterface { y: string }"
-- -- --          , "export interface SomeInterface { x: number }"
-- -- --          ]
-- -- --        expected = AST.Interface
-- -- --          { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"inMemory\".SomeInterface"
-- -- --          , name: "SomeInterface"
-- -- --          , properties:
-- -- --              [{ name: "x", optional: false, type: AST.String }
-- -- --              ,{ name: "y", optional: false, type: AST.Number }
-- -- --              ]
-- -- --          , typeParameters: []
-- -- --          }
-- -- --      readTopLevel' file >>= Assert.equal [ expected ]
-- -- -- 
-- -- --       Test.test "two polymorphic interfaces" do
-- -- --         let
-- -- --           file = dts'
-- -- --             [ "export interface SomeInterface<x, z> { y: x }"
-- -- --             , "export interface SomeInterface<x, z> { x: x }"
-- -- --             ]
-- -- --           expected = AST.Interface
-- -- --             { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"inMemory\".SomeInterface"
-- -- --             , name: "SomeInterface"
-- -- --             , properties:
-- -- --                 [ { name: "y"
-- -- --                   , optional: false
-- -- --                   , type: (AST.TypeParameter { default: Nothing, name: "x" })
-- -- --                   }
-- -- --                 , { name: "x"
-- -- --                   , optional: false
-- -- --                   , type: (AST.TypeParameter { default: Nothing, name: "x" })
-- -- --                   }
-- -- --                 ]
-- -- --             , typeParameters:
-- -- --                 [ { default: Nothing
-- -- --                   , name: "x"
-- -- --                   }
-- -- --                 , { default: Nothing
-- -- --                   , name: "z"
-- -- --                   }
-- -- --                 ]
-- -- --             }
-- -- --         readTopLevel' file >>= Assert.equal [ expected ]
