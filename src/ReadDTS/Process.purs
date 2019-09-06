module ReadDTS.Process where

import Prelude

import Control.Monad.Except (class MonadError, throwError)
import Data.Array (foldMap, head)
import Data.Either (Either, either, note)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (Object)
import Foreign.Object as O
import Node.Path (FilePath)
import ReadDTS (compilerOptions, readDTS)
import ReadDTS.Types (InterfaceMember, TopLevelDec(..), Type(..), onType, onVisit)

processDTSWith
  ∷ ∀ a m
  . MonadError String m
  ⇒ MonadEffect m
  ⇒ (Type → a)
  → FilePath 
  → String 
  → Object a 
  → m { classKey ∷ Array Type, props ∷ Object a }
processDTSWith f path name partialObj = do
  declarations ← liftEffect $ readDTS path compilerOptions onVisit onType
  -- classKey
  let classKeyName = name <> "ClassKey"
  classKey ← findClassKey classKeyName declarations # either throwError pure
  -- props
  let interfaceName = name <> "Props"
  members ← findInterfaceMembers interfaceName declarations # either throwError pure
  let props = fillMissingMembersWith f partialObj members
  -- return
  pure { classKey, props }

findClassKey
  ∷ String
  → Array (TopLevelDec Type)
  → Either String (Array Type)
findClassKey name xs = 
  foldMap f xs # head # note ("class key: " <> name <> " not found")
  where
    f = case _ of
      TypeAlias { name: n, type: Union types } | n == name → [types]
      otherwise → []

findInterfaceMembers
  ∷ String
  → Array (TopLevelDec Type)
  → Either String (Array (InterfaceMember Type))
findInterfaceMembers name xs =
  foldMap f xs # head # note ("interface: " <> name <> " not found")
  where
    f = case _ of
      Interface r | r.name == name → [r.members]
      otherwise → []

fillMissingMembersWith
  ∷ ∀ a
  . (Type → a)
  → Object a
  → Array (InterfaceMember Type)
  → Object a
fillMissingMembersWith f partialObj members =
  let
    tryInsertMember obj member =
      case O.lookup member.name obj of
        Just _ → obj
        Nothing → O.insert member.name (f member.type) obj
  in
  foldl tryInsertMember partialObj members
