module Test.ReadDTS where

import Prelude

import Data.Array (singleton) as Array
import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.Bifunctor (bimap)
import Data.Lens (Lens')
import Data.Lens.Record (prop) as Lens.Record
import Data.List (filter, head) as List
import Data.Map (Map)
import Data.Map (toUnfoldableUnordered) as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Node.Path (FilePath)
import ReadDTS (readRootsDeclarations, readType)
import ReadDTS.AST (TsType(..), TypeDeclaration, visitor) as AST
import ReadDTS.AST (TsType)
import ReadDTS.TypeScript.Testing (InMemoryFile)
import Test.Unit (TestSuite, failure)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (shouldEqual)
import Type.Prelude (Proxy(..))
import TypeScript.Compiler.Parser (SourceCode)
import TypeScript.Compiler.Program (getTypeChecker)
import TypeScript.Compiler.Types (FullyQualifiedName(..), Program, Typ)
import TypeScript.Compiler.Types.Nodes (Declaration) as Nodes

file :: FilePath -> SourceCode -> InMemoryFile
file name source =
  { path: name
  , source: source
  }

type SourceLine = String

file' :: FilePath -> Array SourceLine -> InMemoryFile
file' name = file name <<< String.joinWith "\n"

type RootModule = String

_type :: forall a. Lens' { "type" :: a } a
_type = Lens.Record.prop (Proxy :: Proxy "type")

type Compile = Array FilePath -> Array InMemoryFile -> Aff Program

-- https://www.typescriptlang.org/docs/handbook/declaration-files/templates/module-d-ts.html
suite :: Compile -> TestSuite
suite compile = do
  Test.suite "Simple types" do
    let
      xDeclaration ::
        String ->
        Aff { program :: Program, x :: Maybe (AST.TsType Nodes.Declaration (Typ ())) }
      xDeclaration source = do
        let
          rootName = "Root.ts"
          fqn = FullyQualifiedName "\"Root\".X"

          getX ::
            forall d t.
            Map FullyQualifiedName (AST.TypeDeclaration (AST.TsType d t)) ->
            Maybe (AST.TypeDeclaration (AST.TsType d t))
          getX
            = List.head
            <<< map snd
            <<< List.filter (eq fqn <<< fst)
            <<< Map.toUnfoldableUnordered

          rootFile = file rootName source
        program <- compile [rootName] [rootFile]
        let
          decls = readRootsDeclarations program AST.visitor
        pure { x: _.type <$> getX decls, program }

      testOnX source test = Test.test source do
        r <- xDeclaration source
        case r of
          { x: Nothing } -> failure "Unable to find exported type X"
          { x: Just x, program } -> test { x, program }

      testXShouldEqual source expected = testOnX source \{ x } -> do
        let
          x' = bimap (const unit) (const unit) x
        x' `shouldEqual` expected

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
    testXShouldEqual "export interface X{ m: number }"
      (AST.TsInterface [{ name: "m", optional: false, type: unit }])
    testXShouldEqual "export interface X{ m?: number }"
      (AST.TsInterface [{ name: "m", optional: true, type: unit }])
    testXShouldEqual "export class X{}" (AST.TsClass [])
    -- | This is interesting case
    testXShouldEqual "export class X<param=number>{ }"
      ( AST.TsParametric
        unit
        (Array.NonEmpty.singleton { default: Just unit, name: "param" })
      )
    testXShouldEqual "export interface X<param=number>{ }"
      ( AST.TsParametric
        unit
        (Array.NonEmpty.singleton { default: Just unit, name: "param" })
      )
    testXShouldEqual
      "export type X<param> = number | param"
      (AST.TsParametric
        unit
        (Array.NonEmpty.singleton { default: Nothing, name: "param" })
      )
    testXShouldEqual
      "export type X<param=string> = number | param"
      (AST.TsParametric
        unit
        (Array.NonEmpty.singleton { default: Just unit, name: "param" })
      )
    testXShouldEqual "class C<p> {}; export type X = C<number>"
      ( AST.TsApplication
        unit
        (Array.NonEmpty.singleton unit)
      )

    testOnX "export type X<arg=number> = {prop: arg}" \{ program, x } -> do
      let
        checker = getTypeChecker program
        readType' t = readType checker t [] AST.visitor.onType

        x' :: TsType Unit (TsType Unit (TsType Unit Unit))
        x'
          = bimap
            (const unit)
            (bimap (const unit) (bimap (const unit) (const unit)))
          <<< map (map readType')
          <<< map readType'
          $ x
        body = AST.TsObject $ Array.singleton
          { name: "prop"
          , optional: false
          , type: AST.TsParameter "arg"
          }
        params = Array.NonEmpty.singleton { default: Just AST.TsNumber, name: "arg" }
        expected = AST.TsParametric body params
      x' `shouldEqual` expected

    -- testOnX "export type X<ARG> = ARG" \{ program, x } -> do
    --   let
    --     checker = getTypeChecker program
    --     x' :: TsType Unit (TsType Unit Unit)
    --     x' = map (map (const unit) <<< \t -> readType checker t [] visit.onType) x
    --   x' `shouldEqual` AST.TsAny

    -- testOnX "export type X<ARG> = ARG" \{ program, x } -> do
    --   let
    --     checker = getTypeChecker program
    --     x' :: TsType Unit (TsType Unit Unit)
    --     x' = map (map (const unit) <<< \t -> readType checker t [] visit.onType) x
    --   x' `shouldEqual` AST.TsAny


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
