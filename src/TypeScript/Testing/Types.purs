module TypeScript.Testing.Types where

import Data.Newtype (class Newtype)
import Node.Path (FilePath)
import TypeScript.Compiler.Parser (SourceCode)

newtype InMemoryFile = InMemoryFile { path :: FilePath, source :: SourceCode }

derive instance Newtype InMemoryFile _

mkInMemoryFile :: FilePath -> SourceCode -> InMemoryFile
mkInMemoryFile name source = InMemoryFile
  { path: name
  , source: source
  }
