module TS.Codegen.PS.Types where

import Prelude

import Data.Array.NonEmpty (toArray) as NonEmtpy
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (match) as Regex
import Data.String.Regex.Unsafe (unsafeRegex)
import Debug (traceM)
import TS.Codegen.PS.ModulePath (ModulePath)
import TS.Codegen.PS.ModulePath (fromPath) as ModulePath
import Tidy.Codegen.Types (SymbolName(..))
import TypeScript.Compiler.Types (FullyQualifiedName(..))

data Fqn = Fqn ModulePath SymbolName

derive instance Eq Fqn
derive instance Ord Fqn
derive instance Generic Fqn _
instance Show Fqn where
  show (Fqn mp (SymbolName n)) = "(Fqn " <> show mp <> " (SymbolName " <> n <> "))"

-- | Ts compiler gives quite limited access to the fqn structure.
-- | https://stackoverflow.com/questions/67702391/typescript-compiler-api-how-to-get-fully-qualified-name-of-type-without-absolut#comment119681802_67702391
-- | Let's use what we have at the moment.
fromTsFqn :: FullyQualifiedName -> Maybe Fqn
fromTsFqn (FullyQualifiedName s) = do
  let
    regex = unsafeRegex """^"(.*)"\.(.*)$""" mempty

  matches <- Regex.match regex s
  case NonEmtpy.toArray matches of
    [ Just _, Just path, Just symbol ] -> do
      pure $ Fqn (ModulePath.fromPath path) (SymbolName symbol)
    r -> do
      traceM r
      Nothing
