module Test.NonRecType where

import Prelude

import Data.Array (singleton) as Array
import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import ReadDTS (readType)
import ReadDTS.AST (TsType)
import ReadDTS.AST as AST
import Test.Compile (TypeName(..), compileType)
import Test.Unit (TestSuite, failure)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (shouldEqual)
import TypeScript.Compiler.Parser (SourceCode(..))
import TypeScript.Compiler.Program (getTypeChecker)
import TypeScript.Compiler.Types (FullyQualifiedName, Node, Program, Typ)

testOnType
  :: TypeName
  -> String
  -> ({ program :: Program
     , type :: TsType
                  { fullyQualifiedName :: FullyQualifiedName
                  , ref :: Node "DeclarationStatement" ()
                  }
                  (Typ ())
     }
     -> Aff Unit
    )
  -> TestSuite
testOnType typeName source test = Test.test (source <> "; /* non rec version */") do
  r <- compileType typeName (SourceCode source)
  case r of
    { type: Nothing } -> failure "Unable to find exported type X"
    { type: Just { typ, params }, program } -> do
      let
        checker = getTypeChecker program
        type' = readType checker typ params AST.visitor.onType
      test { type: type', program }

testTypeShouldEqual :: TypeName -> String -> TsType Unit Unit -> TestSuite
testTypeShouldEqual typeName source expected = testOnType typeName source \{ type: t } -> do
  let
    type' = bimap (const unit) (const unit) t
  type' `shouldEqual` expected

suite :: TestSuite
suite = Test.suite "Non recursive ts type layer" do
  let
    testXShouldEqual = testTypeShouldEqual (TypeName "X")
    testOnX = testOnType (TypeName "X")

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

  testOnX "export type X<arg=number> = {prop: arg}" \{ program, type: t } -> do
    let
      checker = getTypeChecker program
      readType' s = readType checker s [] AST.visitor.onType

      t' :: TsType Unit (TsType Unit (TsType Unit Unit))
      t'
        = bimap
          (const unit)
          (bimap (const unit) (bimap (const unit) (const unit)))
        <<< map (map readType')
        <<< map readType'
        $ t
      body = AST.TsObject $ Array.singleton
        { name: "prop"
        , optional: false
        , type: AST.TsParameter "arg"
        }
      params = Array.NonEmpty.singleton { default: Just AST.TsNumber, name: "arg" }
      expected = AST.TsParametric body params
    t' `shouldEqual` expected

