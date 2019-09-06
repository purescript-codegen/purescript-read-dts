module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Class.Console (log)
import ReadDTS (OnType, OnVisit, compilerOptions, readDTS)

stringOnVisit ∷ OnVisit String String
stringOnVisit =
  { interface: \{ name, members } →
      let
        onMember r = joinWith " "
          [r.name, if r.optional then "?:" else ":", r.type]
      in
        "Interface " <> name <> ": " <> joinWith "; " (map onMember members)
  , typeAlias: \r → joinWith " " ["type", r.name, ":", r.type]
  }

stringOnType ∷ OnType String
stringOnType =
  { unionOrIntersection: joinWith " | "
  , primitive: show
  , stringLiteral: show
  , numberLiteral: show
  , unknown: show
  }

fileName ∷ String
fileName = "test/simple.d.ts"

main ∷ Effect Unit
main = do
  res ← readDTS fileName compilerOptions stringOnVisit stringOnType
  for_ res log
