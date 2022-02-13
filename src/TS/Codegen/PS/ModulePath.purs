module TS.Codegen.PS.ModulePath where

import Prelude

import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), fromFoldable, singleton) as List
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split) as String
import Data.String.Extra (upperCaseFirst)
import Node.Path (FilePath)
import Node.Path (sep) as Path
import PureScript.CST.Types (ModuleName(..))

newtype ModulePath = ModulePath (List String)

derive instance Newtype ModulePath _
derive newtype instance Eq ModulePath
derive newtype instance Ord ModulePath
derive instance Generic ModulePath _
instance Show ModulePath where
  show = genericShow

consDir :: String -> ModulePath -> ModulePath
consDir n (ModulePath mp) = ModulePath $ List.Cons n mp

segments :: ModulePath -> List String
segments (ModulePath p) = p

empty :: ModulePath
empty = ModulePath mempty

fromString :: String -> ModulePath
fromString = ModulePath <<< List.singleton

toModuleName :: ModulePath -> ModuleName
toModuleName (ModulePath mp) = ModuleName $ intercalate "." mp

fromPath :: FilePath -> ModulePath
fromPath = ModulePath <<< List.fromFoldable <<< map upperCaseFirst <<< String.split (String.Pattern Path.sep)
