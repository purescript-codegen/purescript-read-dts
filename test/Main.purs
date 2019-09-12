module Test.Main where

import Prelude

import Data.Array (catMaybes, (:))
import Data.Foldable (fold, foldM, foldMap, for_)
import Data.Map (Map)
import Data.Map (fromFoldable, insert, lookup) as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (modify, modify', new, read) as Ref
import Global.Unsafe (unsafeStringify)
import ReadDTS (FullyQualifiedName(..), OnDeclaration, OnType, TypeReference, compilerOptions, readDTS)
import Unsafe.Coerce (unsafeCoerce)

type ReadDeclaration = { fullyQualifiedName ∷ FullyQualifiedName, read ∷ Effect DeclarationRepr }
type TypeRepr = { repr ∷ String, reads ∷ Array ReadDeclaration }
newtype DeclarationRepr = DeclarationRepr
  { fullyQualifiedName ∷ Maybe FullyQualifiedName
  , repr ∷ String
  , reads ∷ Array ReadDeclaration
  }
derive instance newtypeDeclarationRepr ∷ Newtype DeclarationRepr _

stringOnDeclaration ∷ OnDeclaration DeclarationRepr TypeRepr
stringOnDeclaration =
  { interface: \i → DeclarationRepr
      { fullyQualifiedName: Just i.fullyQualifiedName
      , repr: serInterface i
      , reads: foldMap _.reads i.typeParameters <> foldMap _.type.reads i.members
      }
  -- | It seems that type aliases don't introduce names so they don't
  -- | have fullyQualifiedName... but of course I can be wrong.
  , typeAlias: \r@{ type: t, typeParameters } → DeclarationRepr
      { fullyQualifiedName: Nothing
      , repr: serTypeAlias r
      , reads: t.reads <> foldMap _.reads typeParameters
      }
  , unknown: \u → DeclarationRepr
      { repr: serUnknown u
      , fullyQualifiedName: u.fullyQualifiedName
      , reads: []
      }
  }

serUnknown r = "unkownDeclaration " <> show r.fullyQualifiedName <> ": " <> r.msg

serTypeAlias r
  = "typeAlias "
  <> r.name
  <> " <" <> joinWith ", " (map _.repr r.typeParameters) <> "> : "
  <> r.type.repr

serInterface { name, fullyQualifiedName, members, typeParameters }
  = "interface "
  <> show fullyQualifiedName
  <> " <" <> joinWith ", " (map _.repr typeParameters) <> "> : \n\t"
  <> joinWith ";\n\t" (map onMember members)
  where
    onMember r = joinWith " " [r.name, if r.optional then "?:" else ":", r.type.repr ]

noReads ∷ String → TypeRepr
noReads repr = { repr, reads: [] }

stringOnType ∷ OnType DeclarationRepr TypeRepr
stringOnType =
  { intersection: \ts →
      { repr: append "intersection: " <<< joinWith " & " <<< map _.repr $ ts
      , reads: foldMap _.reads ts
      }
  , primitive: noReads <<< append "primitive: " <<< show
  , stringLiteral: noReads <<< append "stringLiteral: " <<< show
  , numberLiteral: noReads <<< append "numberLiteral: " <<< show
  , typeParameter: case _ of
      { default: Nothing, identifier } → noReads $ unsafeStringify identifier
      { default: Just d, identifier } →
          { repr: unsafeStringify identifier <> " = " <> d.repr
          , reads: d.reads
          }
  , typeReference: \{ fullyQualifiedName, read, typeArguments } →
      { repr: "typeReference: " <> show fullyQualifiedName <> "<" <> joinWith ", " (map _.repr typeArguments) <> ">"
      , reads: { fullyQualifiedName, read } : foldMap _.reads typeArguments
      }
  , union: \ts →
      { repr: append "union: " <<< joinWith " | " <<< map _.repr $ ts
      , reads: foldMap _.reads ts
      }
  , unknown: noReads <<< append "unknown: " <<< show
  }

fileName ∷ String
fileName = "test/simple.d.ts"

main ∷ Effect Unit
main = do
  declarations ← readDTS compilerOptions { onDeclaration: stringOnDeclaration, onType: stringOnType } fileName
  for_ declarations \(DeclarationRepr r) → do
     log r.repr
     log "\n"

  -- | Single pass of loading... We should test exhaustive loading too.
  let
    initCache = Map.fromFoldable <<< catMaybes <<< map case _ of
      d@(DeclarationRepr { fullyQualifiedName: Just fullyQualifiedName }) → Just (Tuple fullyQualifiedName d)
      otherwise → Nothing
    cache = initCache declarations

    step c { fullyQualifiedName, read } = case fullyQualifiedName `Map.lookup` c of
      Nothing → read >>= flip (Map.insert fullyQualifiedName) c >>> pure
      Just _ → pure c

  log "Single pass of loading declarations...\n\n"

  cache' ← foldM step cache (foldMap (unwrap >>> _.reads) declarations)

  log "Collected declarations:\n\n"

  for_ cache' \(DeclarationRepr r) → do
     log r.repr
     log "\n"

  pure unit
