module Test.ReadDTS where

import Prelude

import Control.Monad.Except (runExcept, runExceptT)
import Data.Array (head) as Array
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap, for_)
import Data.Functor.Mu (roll)
import Data.Lens (over) as Lens
import Data.Lens.Record (prop)
import Data.Lens.Record (prop) as Lens.Record
import Data.List ((:))
import Data.List (List(..), head, singleton) as List
import Data.Map (Map)
import Data.Map (empty, fromFoldable, singleton, values) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Set (Set)
import Data.Set (empty, fromFoldable, singleton) as Set
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
import ReadDTS (Declarations, OnDeclaration, OnType, TsDeclaration, Visit, readRootsDeclarations)
import ReadDTS (defaults) as ReadDTS
import ReadDTS.AST (KnownDeclarations, TSTyp(..), TsType(..)) as AST
import ReadDTS.TypeScript.Testing (handleMemoryFiles, inMemoryCompilerHost) as Testing
import Test.Unit (failure, suite, test) as Test
import Test.Unit.Assert (equal) as Assert
import Type.Prelude (Proxy(..))
import TypeScript.Class.Compiler.Program (createCompilerHost, createProgram)
import TypeScript.Compiler.Types (CompilerOptions, FullyQualifiedName(..), scriptTarget)
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

visit =
  { onDeclaration: \fqn t -> { fqn, "type": t }
  , onType:
      { any: AST.TsAny
      , array: AST.TsArray
      , boolean: AST.TsBoolean
      , booleanLiteral: AST.TsBooleanLiteral
      , class: AST.TsClass
      , intersection: AST.TsIntersection
      , interface: AST.TsInterface
      , object: AST.TsObject
      , null: AST.TsNull
      , number: AST.TsNumber
      , numberLiteral: AST.TsNumberLiteral
      , string: AST.TsString
      , stringLiteral: AST.TsStringLiteral
      , tuple: AST.TsTuple
      , undefined: AST.TsUndefined
      , unknown: AST.TsUnknown <<< AST.TSTyp
      , union: AST.TsUnion
      }
  }

_type = Lens.Record.prop (Proxy :: Proxy "type")

type TypeDeclaration t = { fqn :: FullyQualifiedName, "type" :: t }

-- https://www.typescriptlang.org/docs/handbook/declaration-files/templates/module-d-ts.html
suite compile = do
  Test.suite "Simple types" do
    let
      xShouldEqual :: forall d. String -> AST.TsType Unit Unit -> _
      xShouldEqual source expected = do
        let
          singleDeclaration :: forall d t. Ord d => Map _ (TypeDeclaration (AST.TsType d t)) -> Maybe (TypeDeclaration (AST.TsType d Unit))
          singleDeclaration = List.head <<< map (Lens.over _type (map $ const unit)) <<< Map.values

          rootName = "Root.ts"
          rootFile = file rootName source
          expected' =
            { fqn: FullyQualifiedName "\"Root\".X"
            , type: expected
            }
        program <- compile [rootName] [rootFile]
        let
          decls = readRootsDeclarations program visit
        Assert.equal (Just expected') (singleDeclaration decls)

      testXShouldEqual source expected = Test.test source do
        xShouldEqual source expected

    testXShouldEqual "export type X = any" AST.TsAny
    testXShouldEqual "export type X = Array<number>;" (AST.TsArray unit)
    testXShouldEqual "export type X = boolean" AST.TsBoolean
    testXShouldEqual "export type X = true" (AST.TsBooleanLiteral true)
    testXShouldEqual "export type X = false" (AST.TsBooleanLiteral false)
    testXShouldEqual "export type X = null" AST.TsNull
    testXShouldEqual "export type X = number" AST.TsNumber
    testXShouldEqual "export type X = 8" (AST.TsNumberLiteral 8.0)
    testXShouldEqual "export type X = string" AST.TsString
    testXShouldEqual "export type X = \"symbol\"" (AST.TsStringLiteral "symbol")
    testXShouldEqual "export type X = [number, string, boolean]" (AST.TsTuple [unit, unit, unit])
    testXShouldEqual "export type X = undefined" AST.TsUndefined
    testXShouldEqual "export type X = string | number" (AST.TsUnion [unit, unit])
    testXShouldEqual "export type X = { x: string } & { y: number }" (AST.TsIntersection [unit, unit])
    testXShouldEqual "export type X = {}" (AST.TsObject [])
    testXShouldEqual "export interface X{}" (AST.TsInterface [])
    testXShouldEqual "export class X{}" (AST.TsClass [])

    -- testXShouldEqual "export type X<arg=number> = {prop: arg}"

--   let
--     readTopLevel' = readTopLevel compilerHost
--   Test.suite "Simple types" do
--     Test.test "type X = any" do
--       let
--         root = file "Root.ts" "export type X = any"
--         fullyQualifiedName = ReadDTS.FullyQualifiedName "\"Root\".X"
--         expected = Map.singleton fullyQualifiedName $ AST.TypeDeclaration
--           { fullyQualifiedName
--           , type: AST.TsAny
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
--          , type: AST.TsAny
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
--          $ (xFqn /\ AST.TypeDeclaration { fullyQualifiedName: xFqn, type: AST.TsAny })
--          : (yFqn /\ AST.TypeDeclaration { fullyQualifiedName: yFqn, type: AST.TsAny })
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
--            , type: AST.AnonymousTsObject []
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
--             , type: AST.AnonymousTsObject
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
--                 , AST.AnonymousTsObject
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
--                 , type: AST.AnonymousTsObject
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
--                 , type: AST.AnonymousTsObject [{ name: "x", optional: false, type: AST.Number }]
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
--               , type: AST.AnonymousTsObject
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
--               , AST.AnonymousTsObject
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
