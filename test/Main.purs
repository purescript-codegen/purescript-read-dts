module Test.Main where

import Prelude

import Data.Foldable (fold, foldMap, for_)
import Data.Map (Map)
import Data.Map (insert, lookup) as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.String (joinWith)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (modify, modify', new, read) as Ref
import Global.Unsafe (unsafeStringify)
import ReadDTS (OnDeclaration, OnType, compilerOptions, readDTS)

logOnDeclartion ∷ OnDeclaration String
logOnDeclartion =
  { interface: log <<< serInterface
  , typeAlias: \r → log $
      "typeAlias: " <> joinWith " " ["type", r.name, ":", r.type]
  }

serInterface { name, fullyQualifiedName, members, typeParameters }
  = "interface "
  <> fullyQualifiedName
  <> " <" <> joinWith ", " typeParameters <> "> : \n\t"
  <> joinWith ";\n\t" (map onMember members)
  where
    onMember r = joinWith " " [r.name, if r.optional then "?:" else ":", r.type]

stringOnType ∷ OnType String
stringOnType =
  { intersection: append "intersection: " <<< joinWith " & "
  , interfaceReference: append "interfaceReference: " <<< _.fullyQualifiedName
  , primitive: append "primitive: " <<< show
  , stringLiteral: append "stringLiteral: " <<< show
  , numberLiteral: append "numberLiteral: " <<< show
  , typeParameter: unsafeStringify
  , union: append "union: " <<< joinWith " | "
  , unknown: append "unknown: " <<< show
  }

accumDeclaration ∷ Effect Unit
accumDeclaration = do
  cache ← Ref.new (mempty ∷ Map String String)
  let
    interfaceReference { fullyQualifiedName, read, typeArguments } = do
      c ← Ref.read cache
      void $ case Map.lookup fullyQualifiedName c of
        Nothing → read
        Just interface → pure unit
      args ← sequence typeArguments
      pure $ fullyQualifiedName <> " <" <> joinWith ", " args <> ">"
    onType ∷ OnType (Effect String)
    onType =
      { intersection: pure <<< append "intersection: " <<< joinWith " & " <=< sequence
      , interfaceReference: pure <<< append "interfaceReference: " <=< interfaceReference
      , primitive: pure <<< append "primitive: " <<< show
      , stringLiteral: pure <<< append "stringLiteral: " <<< show
      , numberLiteral: pure <<< append "numberLiteral: " <<< show
      , typeParameter: pure <<< unsafeStringify
      , union: pure <<< append "union: " <<< joinWith " | " <=< sequence
      , unknown: pure <<< append "unknown: " <<< show
      }
    onDeclaration ∷ OnDeclaration (Effect String)
    onDeclaration =
      { interface: \{ name, fullyQualifiedName, members, typeParameters } → do
          tps ← sequence typeParameters
          ms ← for members \{ name: n, optional, type: t } → do
            t' ← t
            pure { name: n, optional, type: t' }
          let
            onMember r = joinWith " "
              [r.name, if r.optional then "?:" else ":", r.type]
            value = serInterface { name, fullyQualifiedName, members: ms, typeParameters: tps }
          void $ Ref.modify (Map.insert fullyQualifiedName value) cache
      , typeAlias: const $ pure unit
        -- \r → do
        --   t ← r.type
        --   let
        --     value = "typeAlias: " <> joinWith " " ["type", r.name, ":", t ]
        --   void $ Ref.modify (Map.insert fullyQualifiedName value) cache
      }
  readDTS compilerOptions { onDeclaration,  onType } fileName
  res ← Ref.read cache
  log $ foldMap (\v → v <> "\n\n") res

fileName ∷ String
fileName = "test/simple.d.ts"

main ∷ Effect Unit
main = do
  -- readDTS compilerOptions { onDeclaration: logOnDeclartion, onType: stringOnType } fileName
  -- for_ res log
  accumDeclaration
