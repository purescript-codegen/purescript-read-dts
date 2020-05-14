module Test.Main where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import ReadDTS (defaults) as ReadDTS
import ReadDTS.AST (build)
import ReadDTS.Instantiation (instantiate) as I
import ReadDTS.Instantiation.Pretty (pprint)
import Test.ReadDTS (suite) as Test.ReadDTS
import Test.ReadDTS.Instantiation (suite) as Test.ReadDTS.Instantiation
import Test.Unit.Console (print)
import Test.Unit.Main (runTest)
import Test.Unit.Main (runTest) as Test


-- main :: Effect Unit
-- main = do
--   Test.runTest Test.ReadDTS.Instantiation.suite

source = """
// declare module 'vscode' {

  // // export const version: string;
  // // /**
  // //  * Represents a reference to a command. Provides a title which
  // //  * will be used to represent a command in the UI and, optionally,
  // //  * an array of arguments which will be passed to the command handler
  // //  * function when invoked.
  // //  */
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

  export class Position {

    /**
     * The zero-based line value.
     */
    readonly line: number;

    /**
     * The zero-based character value.
     */
    readonly character: number;

    /**
     * @param line A zero-based line value.
     * @param character A zero-based character value.
     */
    constructor(line: number, character: number);

    // /**
    //  * Check if this position is before `other`.
    //  *
    //  * @param other A position.
    //  * @return `true` if position is on a smaller line
    //  * or on the same line on a smaller character.
    //  */
    isBefore(other: Position): boolean;

    // /**
    //  * Check if this position is before or equal to `other`.
    //  *
    //  * @param other A position.
    //  * @return `true` if position is on a smaller line
    //  * or on the same line on a smaller or equal character.
    //  */
    isBeforeOrEqual(other: Position): boolean;

    // /**
    //  * Check if this position is after `other`.
    //  *
    //  * @param other A position.
    //  * @return `true` if position is on a greater line
    //  * or on the same line on a greater character.
    //  */
    // isAfter(other: Position): boolean;

    // /**
    //  * Check if this position is after or equal to `other`.
    //  *
    //  * @param other A position.
    //  * @return `true` if position is on a greater line
    //  * or on the same line on a greater or equal character.
    //  */
    // isAfterOrEqual(other: Position): boolean;

    // /**
    //  * Check if this position is equal to `other`.
    //  *
    //  * @param other A position.
    //  * @return `true` if the line and character of the given position are equal to
    //  * the line and character of this position.
    //  */
    // isEqual(other: Position): boolean;

    // /**
    //  * Compare this to `other`.
    //  *
    //  * @param other A position.
    //  * @return A number smaller than zero if this position is before the given position,
    //  * a number greater than zero if this position is after the given position, or zero when
    //  * this and the given position are equal.
    //  */
    // compareTo(other: Position): number;

    // /**
    //  * Create a new position relative to this position.
    //  *
    //  * @param lineDelta Delta value for the line value, default is `0`.
    //  * @param characterDelta Delta value for the character value, default is `0`.
    //  * @return A position which line and character is the sum of the current line and
    //  * character and the corresponding deltas.
    //  */
    // translate(lineDelta?: number, characterDelta?: number): Position;

    // /**
    //  * Derived a new position relative to this position.
    //  *
    //  * @param change An object that describes a delta to this position.
    //  * @return A position that reflects the given delta. Will return `this` position if the change
    //  * is not changing anything.
    //  */
    // translate(change: { lineDelta?: number; characterDelta?: number; }): Position;

    // /**
    //  * Create a new position derived from this position.
    //  *
    //  * @param line Value that should be used as line value, default is the [existing value](#Position.line)
    //  * @param character Value that should be used as character value, default is the [existing value](#Position.character)
    //  * @return A position where line and character are replaced by the given values.
    //  */
    // with(line?: number, character?: number): Position;

    // /**
    //  * Derived a new position from this position.
    //  *
    //  * @param change An object that describes a change to this position.
    //  * @return A position that reflects the given change. Will return `this` position if the change
    //  * is not changing anything.
    //  */
    // with(change: { line?: number; character?: number; }): Position;
  }

// }
"""

-- https://www.typescriptlang.org/docs/handbook/declaration-files/templates/module-d-ts.html

main :: Effect Unit
main = do
  runTest Test.ReadDTS.suite
  -- -- Test.runTest Test.ReadDTS.Instantiation.suite
  -- let
  --   compilerOptions = ReadDTS.defaults { strictNullChecks = true }
  -- log "BUILDING"
  -- e <- build compilerOptions { path: "test/vscode.d.ts", source: Just source }
  -- log "BUILT"
  -- case e of
  --   Right r -> do
  --     let
  --       mts = extract $ runExceptT $ traverse (\tc -> I.instantiate tc []) r
  --     case mts of
  --       Right ts -> traverse_ print $ map pprint ts
  --       Left e -> print $ show e
  --   Left e -> print $ show e
