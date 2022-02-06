module TypeScript.Testing.DefaultLib where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Buffer (toString) as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readFile)
import Node.Path (FilePath)
import TypeScript.Compiler.Parser (SourceCode(..), createSourceFile)
import TypeScript.Compiler.Types (scriptTarget)
import TypeScript.Compiler.Types.Nodes (SourceFile) as Nodes
import TypeScript.Testing.Types (InMemoryFile(..), mkInMemoryFile)

defaultLibPath :: FilePath
defaultLibPath = "node_modules/typescript/lib/lib.es5.d.ts"

defaultLibSrc :: Maybe FilePath -> Effect InMemoryFile
defaultLibSrc path = do
  let
    path' = fromMaybe defaultLibPath path

  exists path' >>=
    if _ then do
      b <- readFile path'
      mkInMemoryFile path' <<< SourceCode <$> Buffer.toString UTF8 b
    else
      throw
        $ "InSubdir.compilerHost: Unable to find default `CompilerHost` library file:"
            <> path'

load
  :: Maybe FilePath
  -> Effect
       { inMemoryFile :: InMemoryFile
       , sourceFile :: Nodes.SourceFile
       }
load path = do
  let
    path' = fromMaybe defaultLibPath path

  i@(InMemoryFile { source }) <- defaultLibSrc (Just path')
  sourceFile <- createSourceFile path' source scriptTarget."ES5" true
  pure { inMemoryFile: i, sourceFile }
