module Test.ReadDTS.Instantiation where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (roll)
import Data.Map (fromFoldable, singleton) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..)) as Tuple
import Effect.Class (liftEffect)
import ReadDTS.AST (build) as AST
import ReadDTS.Instantiation (TypeF(..), instantiate)
import Test.Unit (TestSuite)
import Test.Unit (failure, suite, test) as Test
import Test.Unit.Assert (equal) as Assert

lines :: forall f. Foldable f => f String -> String
lines = intercalate "\n"

suite :: TestSuite
suite = Test.suite "ReadDTS.Instantiation" $ do
  Test.test "Non overlapping intersection of objects" do
    let
      source = "export type X = {s: string} & { n: number } & { b: boolean }"
      props = Map.fromFoldable
        [ Tuple.Tuple "n" { type: roll Number, optional: false }
        , Tuple.Tuple "s" { type: roll String, optional: false }
        , Tuple.Tuple "b" { type: roll Boolean, optional: false }
        ]

      file =
        { path: "test/test.module.d.ts"
        , source: Just source
        }
      compilerOptions = { strictNullChecks: true }

    liftEffect (AST.build compilerOptions file) >>= case _ of
      Right [tc] → runExcept (instantiate tc []) # case _ of
        Right (Mu.In obj@(Object n o)) → do
          Assert.equal obj (Object "__type & __type & __type" props)
        Right _ → Test.failure "Expecting an object"
        Left err → Test.failure err
      Right _ → Test.failure "Expecting single constructor"
      Left err → Test.failure $ lines err

  Test.test "Intersection of monomorphic string unions" do
    let
      source = lines
        [ "type X = { color: string } & { color: 'red' | 'green' | 'blue' }" ]
      file =
        { path: "test/test.module.d.ts"
        , source: Just source
        }
      compilerOptions = { strictNullChecks: true }
      colorProperty = { optional: false, "type": _ } $ roll $ Union $
        [ roll $ StringLiteral "red"
        , roll $ StringLiteral "green"
        , roll $ StringLiteral "blue"
        ]
      expected = roll $ Object "__type & __type" $ Map.fromFoldable [ Tuple.Tuple "color" colorProperty ]

    liftEffect (AST.build compilerOptions file) >>= case _ of
      Right [tc] → runExcept (instantiate tc []) # case _ of
        Right t → Assert.equal expected t
        Left err → Test.failure $ err
      Right _ → Test.failure $ "Expecting single constructor"
      Left err → Test.failure $ lines err

  Test.test "Non overlapping intersection of objects" do
    let
      source = "export type X = {s: string} & { n: number } & { b: boolean }"
      props = Map.fromFoldable
        [ Tuple.Tuple "n" { type: roll Number, optional: false }
        , Tuple.Tuple "s" { type: roll String, optional: false }
        , Tuple.Tuple "b" { type: roll Boolean, optional: false }
        ]
      file =
        { path: "test/test.module.d.ts"
        , source: Just source
        }
      compilerOptions = { strictNullChecks: true }

    liftEffect (AST.build compilerOptions file) >>= case _ of
      Right [tc] → runExcept (instantiate tc []) # case _ of
        Right (Mu.In obj@(Object n o)) → do
          Assert.equal obj (Object "__type & __type & __type" props)
        Right _ → Test.failure "Expecting an Object"
        Left err → Test.failure err
      Right _ → Test.failure $ "Expecting single constructor"
      Left err → Test.failure $ lines err

  Test.test "Overlapping intersection of non overlapping objects" do
    let
      source = "export type X = { o: { s: string }} & { o: { n: number }}"
      sub = roll $ Object "__type & __type" $ Map.fromFoldable
        [ Tuple.Tuple "n" { type: roll Number, optional: false }
        , Tuple.Tuple "s" { type: roll String, optional: false }
        ]
      expected = Object "__type & __type" $
        Map.singleton "o" $ { optional: false, type: sub }

      file =
        { path: "test/test.module.d.ts"
        , source: Just source
        }
      compilerOptions = { strictNullChecks: true }

    liftEffect (AST.build compilerOptions file) >>= case _ of
      Right [tc] → runExcept (instantiate tc []) # case _ of
        Right (Mu.In obj@(Object n o)) → do
          Assert.equal expected obj
        Right _ → Test.failure "Expecting an object type"
        Left err → Test.failure err
      Right _ → Test.failure $ "Expecting single constructor"
      Left err → Test.failure $ lines err

