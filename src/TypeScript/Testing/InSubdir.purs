module TypeScript.Testing.Subdir where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (opt)
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Node.Buffer (toString) as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (exists, readFile, stat)
import Node.Path (concat) as Path
import TypeScript.Compiler.Parser (SourceCode(..), createSourceFile)
import TypeScript.Compiler.Types (CompilerHost, scriptTarget)
import TypeScript.Testing.BoundedCompilerHost (BoundedCompilerHost)
import TypeScript.Testing.BoundedCompilerHost (toCompilerHost) as BoundedCompilerHost
import TypeScript.Testing.InMemory (Opts, boundedCompilerHost) as InMemory

newtype DirName = DirName String

type Opts r = InMemory.Opts (dir :: DirName | r)

compilerHost :: forall r. Opts r -> Effect CompilerHost
compilerHost opts = BoundedCompilerHost.toCompilerHost <$> boundedCompilerHost opts

boundedCompilerHost :: forall r. Opts r -> Effect BoundedCompilerHost
boundedCompilerHost opts@{ dir: DirName dirName } = do
  let
    fullPath p = Path.concat [ dirName, p ]
  host <- InMemory.boundedCompilerHost opts
  let
    readSource p = do
      e <- exists p
      if e then do
        b <- readFile p
        source <- Buffer.toString UTF8 b
        pure $ Just (SourceCode source)
      else do
        pure Nothing

    host' :: BoundedCompilerHost
    host' = host
      { fileExists = mkEffectFn1 \p -> do
          (||) <$> exists (fullPath p) <*> runEffectFn1 host.fileExists p
      , directoryExists = opt $ mkEffectFn1 \d -> do
          res <- (isDirectory <$> stat (fullPath d)) `catchError` \_ -> pure false
          case res, NoProblem.toMaybe host.directoryExists of
            true, _ -> pure true
            _, Just fallback -> runEffectFn1 fallback d
            _, Nothing -> pure false

      , getCurrentDirectory = opt $ pure dirName
      , getDirectories = opt $ mkEffectFn1 $ const (pure [ dirName ])
      , getSourceFile = mkEffectFn2 \p x -> do
          let
            p' = fullPath p
          readSource p' >>= case _ of
            Just source -> do
              sf <- createSourceFile p source scriptTarget."ES5" true
              pure $ opt sf
            Nothing -> runEffectFn2 host.getSourceFile p x
      , readFile = mkEffectFn1 \p -> do
          readSource p >>= case _ of
            Just source -> pure $ opt source
            Nothing -> runEffectFn1 host.readFile p
      }
  pure host'
