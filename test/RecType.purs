module Test.RecType where

import Prelude

import Data.Array (singleton) as Array
import Data.Array.NonEmpty (singleton) as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (Mu(..), roll)
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Effect.Aff (Aff)
import Matryoshka (cata)
import ReadDTS (Param(..), Params(..))
import ReadDTS.AST (TsType(..))
import ReadDTS.AST as AST
import Test.Compile (TypeName(..), compileType)
import Test.Unit (TestSuite, failure)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (shouldEqual)
import TypeScript.Compiler.Parser (SourceCode(..))
import TypeScript.Compiler.Program (getTypeChecker)
import TypeScript.Compiler.Types (FullyQualifiedName(..), Program)

testOnType
  :: TypeName
  -> String
  -> ( { program :: Program
       , type :: Mu (TsType FullyQualifiedName)
       }
       -> Aff Unit
     )
  -> TestSuite
testOnType typeName source test = Test.test source do
  r <- compileType typeName (SourceCode source)
  case r of
    { type: Nothing } -> failure "Unable to find exported type X"
    { type: Just { typ, params }, program } -> do
      let
        checker = getTypeChecker program
        stripTypeRefs = cata (Mu.In <<< lmap _.fullyQualifiedName)

      case AST.unfoldType checker params { level: 0, ref: typ } of
        Right (type'@(In _)) -> test { type: stripTypeRefs type', program }
        Left err -> failure $ "FAILURE: " <> show err

testTypeShouldEqual :: TypeName -> String -> Mu (TsType FullyQualifiedName) -> TestSuite
testTypeShouldEqual typeName source expected = testOnType typeName source \{ type: t } ->
  t `shouldEqual` expected

suite :: TestSuite
suite = Test.suite "Recursive type repr" do
  let
    testXShouldEqual = testTypeShouldEqual (TypeName "X")

  testXShouldEqual "export interface X{ m: { n: number }}" do
    roll
      $ AST.TsInterface
      $ Array.singleton
          { name: "m"
          , optional: false
          , type: roll $ AST.TsObject $ Array.singleton
              { name: "n"
              , optional: false
              , type: roll AST.TsNumber
              }
          }

  do
    let
      program = String.joinWith " "
        [ "export type Y<n> = { m: number, n: n };"
        , "export interface X{ y: Y<number> }"
        ]
    testXShouldEqual program
      $ roll
      $ AST.TsInterface
      $ Array.singleton
          { name: "y"
          , optional: false
          , type: roll $ AST.TsApplication
              (FullyQualifiedName "\"Root\".Y")
              (Array.NonEmpty.singleton $ roll AST.TsNumber)
          }

  do
    let
      program = String.joinWith " "
        [ "export class Y<n>{ m: number, n: n };"
        , "export interface X{ y: Y<number> }"
        ]
    testXShouldEqual
      program
      $ roll
      $ AST.TsInterface
      $ Array.singleton
          { name: "y"
          , optional: false
          , type: roll $ AST.TsApplication
              (FullyQualifiedName "\"Root\".Y")
              (Array.NonEmpty.singleton $ roll AST.TsNumber)
          }
  do
    let
      program = String.joinWith " "
        [ "export interface Y<n>{ m: number, n: n }"
        , "export interface X{ y: Y<number> }"
        ]
    testXShouldEqual
      program
      $ roll
      $ AST.TsInterface
      $ Array.singleton
          { name: "y"
          , optional: false
          , type: roll $ AST.TsApplication
              (FullyQualifiedName "\"Root\".Y")
              (Array.NonEmpty.singleton $ roll AST.TsNumber)
          }

  do
    let
      program = String.joinWith " "
        [ "export type Y = { m: number, n: number };"
        , "export interface X{ y: Y }"
        ]
    testXShouldEqual
      program
      $ roll
      $ AST.TsInterface
      $ Array.singleton
          { name: "y"
          , optional: false
          , type: roll $ AST.TsTypeRef $ FullyQualifiedName "\"Root\".Y"
          }

  do
    let
      source = String.joinWith " "
        [ "export interface Y { m: number, n: number }"
        , "export interface X{ y: Y }"
        ]
    testXShouldEqual
      source
      $ roll
      $ AST.TsInterface
      $ Array.singleton
          { name: "y"
          , optional: false
          , type: roll $ AST.TsTypeRef $ FullyQualifiedName "\"Root\".Y"
          }

  do
    let
      source = String.joinWith " "
        [ "export class Y { m: number, n: number };"
        , "export interface X{ y: Y };"
        ]
    testXShouldEqual
      source
      $ roll
      $ AST.TsInterface
      $ Array.singleton
          { name: "y"
          , optional: false
          , type: roll $ AST.TsTypeRef $ FullyQualifiedName "\"Root\".Y"
          }

  testXShouldEqual "export type Y = { yp: string }; export interface X{ xp: { xpp: Y }}"
    $ roll
    $ AST.TsInterface
    $ Array.singleton
        { name: "xp"
        , optional: false
        , type: roll $ AST.TsObject $ Array.singleton
            { name: "xpp"
            , optional: false
            , type: roll $ AST.TsTypeRef $ FullyQualifiedName "\"Root\".Y"
            }
        }

  do
    let
      source = String.joinWith " "
        [ "export type Y = { m: number }"
        , "export interface X extends Y { n: string }"
        ]
    testXShouldEqual
      source
      $ roll
      $ AST.TsInterface
          [ { name: "n"
            , optional: false
            , type: roll $ AST.TsString
            }
          , { name: "m"
            , optional: false
            , "type": roll AST.TsNumber
            }
          ]

  do
    let
      source = String.joinWith " "
        [ "export class Y = { m: number };"
        , "export class X extends Y { n: string }"
        ]
    testXShouldEqual
      source
      $ roll
      $ AST.tsClass
          [ roll $ TsTypeRef $ FullyQualifiedName "\"Root\".Y" ]
          [ [] ]
          [ { name: "n"
            , optional: false
            , type: roll $ AST.TsString
            }
          ]
  do
    let
      source = String.joinWith " "
        [ "export class X {"
        , " u: string; "
        , " constructor() { this.u = 'X'; }; "
        , " constructor(s: string, u='default') { this.u = s + u; };"
        , "}"
        ]
    testXShouldEqual
      source
      $ roll
      $ AST.tsClass
          []
          [ []
          , [ { name: "s", optional: false, type: roll TsString }
            , { name: "u", optional: true, type: roll TsString }
            ]
          ]
          [ { name: "u"
            , optional: false
            , type: roll $ AST.TsString
            }
          ]
  do
    let
      program =
        "export type MonoTree = { left: MonoTree, right: MonoTree } | number;"
      fqn = FullyQualifiedName "\"Root\".MonoTree"

    testTypeShouldEqual (TypeName "MonoTree") program
      $ roll
      $ AST.TsUnion
          [ roll AST.TsNumber
          , roll $ AST.TsObject
              [ { name: "left", optional: false, type: roll $ AST.TsTypeRef fqn }
              , { name: "right", optional: false, type: roll $ AST.TsTypeRef fqn }
              ]
          ]
  testXShouldEqual "export function X(nonOpt: number, opt?: string): number { return 0; }"
    $ roll
    $ AST.TsFunction
        [ { name: "nonOpt", type: roll AST.TsNumber, optional: false }
        , { name: "opt", type: roll AST.TsString, optional: true }
        ]
        (roll AST.TsNumber)

  do
    let
      body = roll $ AST.TsInterface $ Array.singleton { name: "x", optional: false, type: roll (AST.TsParameter "param") }
      params = Params (Array.NonEmpty.singleton (Param { default: Just $ roll AST.TsNumber, name: "param" }))
      expected = roll $ TsParametric body params

    testXShouldEqual "export interface X<param=number>{ x: param }" $ expected

  do
    let
      expected = roll $ AST.TsInterface $ Array.singleton { name: "x", optional: false, type: roll (AST.TsArray $ roll AST.TsNumber) }
    testXShouldEqual "export interface X{ x: number[] }" expected

-- | This fails but on our unfold recursion...
-- | It should probably anyway or maybe...
-- | we want to handle this invalid type?
-- testXShouldEqual "export interface X{ m: { n: X }}" $
--   roll $ AST.TsInterface $ Array.singleton
--     { name: "m"
--     , optional: false
--     , type: roll $ AST.TsObject $ Array.singleton
--         { name: "n"
--         , optional: false
--         , type: roll AST.TsNumber
--         }
--     }
