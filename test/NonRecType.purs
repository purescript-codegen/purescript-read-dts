module Test.NonRecType where

import Prelude

import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Aff (Aff)
import ReadDTS (Params, readTopLevelType)
import ReadDTS.AST (TsType)
import ReadDTS.AST as AST
import Test.Compile (TypeName(..), compileType)
import Test.Unit (TestSuite, failure)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (shouldEqual)
import TypeScript.Compiler.Parser (SourceCode(..))
import TypeScript.Compiler.Program (getTypeChecker)
import TypeScript.Compiler.Types (FullyQualifiedName, Node, Program, Typ)

extractTyp
  :: TypeName
  -> String
  -> Aff
       ( Maybe
           { params :: Maybe (Params (Typ ()))
           , program :: Program
           , typ :: Typ ()
           }
       )
extractTyp typeName source = do
  r <- compileType typeName (SourceCode source)
  case r of
    { type: Nothing } -> pure Nothing
    { type: Just { typ, params }, program } -> do
      pure $ Just { typ, params, program }

extractType
  :: TypeName
  -> String
  -> Aff
       ( Maybe
           { program :: Program
           , type ::
               TsType
                 { fullyQualifiedName :: FullyQualifiedName
                 , ref :: Node "DeclarationStatement" ()
                 }
                 (Typ ())
           }
       )
extractType typeName source = do
  t <- extractTyp typeName source
  for t \{ typ, program } -> do
    let
      checker = getTypeChecker program
      type' = readTopLevelType checker typ AST.visitor.onType
    pure { type: type', program }

testOnType
  :: TypeName
  -> String
  -> ( { program :: Program
       , type ::
           TsType
             { fullyQualifiedName :: FullyQualifiedName
             , ref :: Node "DeclarationStatement" ()
             }
             (Typ ())
       }
       -> Aff Unit
     )
  -> TestSuite
testOnType typeName source test = Test.test (source <> "; /* non rec version */") do
  extractType typeName source >>= case _ of
    Nothing -> failure "Unable compile program or find type X"
    Just r -> test r

testTypeShouldEqual :: TypeName -> String -> TsType Unit Unit -> TestSuite
testTypeShouldEqual typeName source expected = testOnType typeName source \{ type: t } -> do
  let
    type' = bimap (const unit) (const unit) t
  type' `shouldEqual` expected

suite :: TestSuite
suite = Test.suite "Non recursive ts type layer" do
  let
    testXShouldEqual = testTypeShouldEqual (TypeName "X")

  testXShouldEqual "export function X(nonOpt: number, opt?: string): number { return 0; }" $
    AST.TsFunction
      [ { name: "nonOpt", type: unit, optional: false }
      , { name: "opt", type: unit, optional: true }
      ]
      unit

  testXShouldEqual "export type X = Array<number>;" (AST.TsArray unit)
  testXShouldEqual "export type X = boolean" AST.TsBoolean
  testXShouldEqual "export type X = true" (AST.TsBooleanLiteral true)
  testXShouldEqual "export type X = false" (AST.TsBooleanLiteral false)
  testXShouldEqual "export type X = null" AST.TsNull
  testXShouldEqual "export type X = number" AST.TsNumber
  testXShouldEqual "export type X = 8" (AST.TsNumberLiteral 8.0)
  testXShouldEqual "export type X = string" AST.TsString
  testXShouldEqual "export type X = \"symbol\"" (AST.TsStringLiteral "symbol")
  testXShouldEqual "export type X = [number, string, boolean]" (AST.TsTuple [ unit, unit, unit ])
  testXShouldEqual "export type X = undefined" AST.TsUndefined
  testXShouldEqual "export type X = string | number" (AST.TsUnion [ unit, unit ])
  testXShouldEqual "export type X = { x: string } & { y: number }" (AST.TsIntersection [ unit, unit ])
  testXShouldEqual "export type X = {}" (AST.TsObject [])
  testXShouldEqual "export interface X{}" (AST.TsInterface [])
  testXShouldEqual "export interface X{ m: number }"
    (AST.TsInterface [ { name: "m", optional: false, type: unit } ])
  testXShouldEqual "export interface X{ m?: number }"
    (AST.TsInterface [ { name: "m", optional: true, type: unit } ])
  testXShouldEqual "export class X{}" (AST.TsClass [])

  testXShouldEqual "class C<p> {}; export type X = C<number>"
    ( AST.TsApplication
        unit
        (Array.NonEmpty.singleton unit)
    )

-- export class Matrix3 implements Matrix {
-- let
--   source = String.joinWith ";\n"
--     [ "export interface Y { elements: number[]; }"
--     , "export class X implements Y { elements: number[]; }"
--     ]

-- testXShouldEqual source (AST.TsAny)

-- FIXME: Parametric types are not handled anymore by `readTyp` which is mainly
-- tested here. We should:
--
-- * improve `unfoldType` so it return `TsUnkown` (or our internal represent
-- tation for unread type).
-- * test this cases using `unfoldType` with `maxDepth` 1

-- testXShouldEqual "export class X<param=number>{ }"
--   ( AST.TsParametric
--       unit
--       (Array.NonEmpty.singleton { default: Just unit, name: "param" })
--   )
-- testXShouldEqual "export interface X<param=number>{ }"
--   ( AST.TsParametric
--       unit
--       (Array.NonEmpty.singleton { default: Just unit, name: "param" })
--   )
-- testXShouldEqual
--   "export type X<param> = number | param"
--   ( AST.TsParametric
--       unit
--       (Array.NonEmpty.singleton { default: Nothing, name: "param" })
--   )
-- testXShouldEqual
--   "export type X<param=string> = number | param"
--   ( AST.TsParametric
--       unit
--       (Array.NonEmpty.singleton { default: Just unit, name: "param" })
--   )

--  let
--      testOnX = testOnType (TypeName "X")
-- testOnX "export type X<arg=number> = {prop: arg}" \{ program, type: t } -> do
--   let
--     checker = getTypeChecker program
--     readType' s = readType checker s (AllowTypeRef true) AST.visitor.onType

--     t' :: TsType Unit (TsType Unit (TsType Unit Unit))
--     t' =
--       bimap
--         (const unit)
--         (bimap (const unit) (bimap (const unit) (const unit)))
--         <<< map (map readType')
--         <<< map readType'
--         $ t
--     body = AST.TsObject $ Array.singleton
--       { name: "prop"
--       , optional: false
--       , type: AST.TsParameter "arg"
--       }
--     params = Array.NonEmpty.singleton { default: Just AST.TsNumber, name: "arg" }
--     expected = AST.TsParametric body params
--   t' `shouldEqual` expected
