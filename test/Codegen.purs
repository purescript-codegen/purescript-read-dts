module Test.Codegen where

import Prelude

import Data.Maybe (Maybe(..))
import TS.Codegen.PS.ModulePath (consDir)
import TS.Codegen.PS.ModulePath (empty) as ModulePath
import TS.Codegen.PS.Types (Fqn(..), fromTsFqn)
import Test.Unit (TestSuite)
import Test.Unit (suite, test) as Test
import Test.Unit.Assert (shouldEqual)
import Tidy.Codegen.Types (SymbolName(..))
import TypeScript.Compiler.Types (FullyQualifiedName(..))

suite :: TestSuite
suite = Test.suite "Codegen" do
  Test.test "TS fqn conversion" do
    let
      tsFqn = FullyQualifiedName """"three/math/Matrix4".Matrix4"""
      expected = Just $ Fqn
        (consDir "Three" $ consDir "Math" $ consDir "Matrix4" ModulePath.empty)
        (SymbolName "Matrix4")

    fromTsFqn tsFqn `shouldEqual` expected
