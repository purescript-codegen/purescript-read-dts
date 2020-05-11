module Test.Main where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import ReadDTS.AST (build)
import ReadDTS.Instantiation (instantiate) as I
import ReadDTS.Instantiation.Pretty (pprint)
import Test.ReadDTS.Instantiation (suite) as Test.ReadDTS.Instantiation
import Test.Unit.Console (print)
import Test.Unit.Main (runTest) as Test


-- main :: Effect Unit
-- main = do
--   Test.runTest Test.ReadDTS.Instantiation.suite

source = """
declare module 'vscode' {

  // /**
  //  * The version of the editor.
  //  */
  // export const version: string;

  // /**
  //  * Represents a reference to a command. Provides a title which
  //  * will be used to represent a command in the UI and, optionally,
  //  * an array of arguments which will be passed to the command handler
  //  * function when invoked.
  //  */
  // export interface Command {
  //   /**
  //    * Title of the command, like `save`.
  //    */
  //   title: string;

  //   /**
  //    * The identifier of the actual command handler.
  //    * @see [commands.registerCommand](#commands.registerCommand).
  //    */
  //   command: string;

  //   /**
  //    * A tooltip for the command, when represented in the UI.
  //    */
  //   tooltip?: string;

  //   /**
  //    * Arguments that the command handler should be
  //    * invoked with.
  //    */
  //   arguments?: any[];
  // }
}
"""

main :: Effect Unit
main = do
  -- Test.runTest Test.ReadDTS.Instantiation.suite
  e <- build { strictNullChecks: true } { path: "test/vscode.d.ts", source: Just source }
  case e of
    Right r -> do
      let
        mts = extract $ runExceptT $ traverse (\tc -> I.instantiate tc []) r
      case mts of
        Right ts -> traverse_ print $ map pprint ts
        Left e -> print $ show e
    Left e -> print $ show e
