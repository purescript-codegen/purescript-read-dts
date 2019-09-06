module Main where

import Prelude

import Control.Monad.Except (ExceptT(..), except)
import Control.Monad.State (get)
import Data.Array (filter, foldMap, head)
import Data.Either (Either, note)
import Data.Foldable (foldl)
import Data.Functor.Coproduct (left)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as O
import Node.Path (FilePath)
import ReadDTS (compilerOptions, readDTS)
import ReadDTS.Types (InterfaceMember, TopLevelDec(..), Type(..), onType, onVisit)

fileName ∷ String
fileName = "test/simple.d.ts"

main ∷ Effect Unit
main = do
  -- declarations ← readDTS fileName compilerOptions onVisit onType
  pure unit
