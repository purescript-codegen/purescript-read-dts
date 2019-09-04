module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import ReadDTS (OnType, compilerOptions, readDTS)

stringOnType ∷ OnType String
stringOnType =
  { interface: \{ name, members } →
      let
        onMember r = joinWith " "
          [r.name, if r.optional then "?:" else ":", r.type]
      in
        "interface " <> name <> ": " <> joinWith "; " (map onMember members)
  , typeAlias: \r → "typeAlias: " <> joinWith " " ["type", r.name, ":", r.type]
  , unionOrIntersection: append "union: " <<< joinWith " | "
  , primitive: append "primitive: " <<< show
  , stringLiteral: append "stringLiteral: " <<< show
  , numberLiteral: append "numberLiteral: " <<< show
  , unknown: append "unkown: " <<< show
  }

fileName ∷ String
fileName = "test/simple.d.ts"

main ∷ Effect Unit
main = do
  res ← readDTS fileName compilerOptions stringOnType
  for_ res log
