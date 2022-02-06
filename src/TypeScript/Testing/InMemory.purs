module TypeScript.Testing.InMemory where

import Prelude

import Data.Array (any, cons) as Array
import Data.Array (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Data.Undefined.NoProblem (opt, undefined)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2, mkEffectFn3)
import Node.Path (FilePath)
import TypeScript.Compiler.Parser (createSourceFile)
import TypeScript.Compiler.Types (CompilerHost, scriptTarget)
import TypeScript.Testing.BoundedCompilerHost (BoundedCompilerHost)
import TypeScript.Testing.BoundedCompilerHost (toCompilerHost) as BoundedCompilerHost
import TypeScript.Testing.DefaultLib (load) as DefaultLib
import TypeScript.Testing.Types (InMemoryFile(..))

type Opts r =
  { files :: Array InMemoryFile
  , defaultLib :: Maybe FilePath
  | r
  }

compilerHost :: forall r. Opts r -> Effect CompilerHost
compilerHost opts = BoundedCompilerHost.toCompilerHost <$> boundedCompilerHost opts

boundedCompilerHost :: forall r. Opts r -> Effect BoundedCompilerHost
boundedCompilerHost opts = do
  let
    { defaultLib, files } = opts
    defaultLibFile = fromMaybe "node_modules/typescript/lib/lib.es5.d.ts" defaultLib

  sourceFiles <- for files \(InMemoryFile { path, source }) -> do
    file <- createSourceFile path source scriptTarget."ES5" true
    pure { path, source, file }

  defaultLib <- DefaultLib.load defaultLib

  let
    sourceFiles' = do
      let
        InMemoryFile { path, source } = defaultLib.inMemoryFile
      Array.cons
        { path
        , file: defaultLib.sourceFile
        , source
        }
        sourceFiles
  let
    host :: BoundedCompilerHost
    host =
      { fileExists: mkEffectFn1 \p -> do
          pure (Array.any (eq p <<< _.path) sourceFiles')
      , directoryExists: opt $ mkEffectFn1 \d -> do
          pure (d == "")
      , getCurrentDirectory: opt $ pure ""
      , getDirectories: opt $ mkEffectFn1 $ const (pure [ "" ])
      , getCanonicalFileName: mkEffectFn1 pure
      , getNewLine: pure "\n"
      , getDefaultLibFileName: mkEffectFn1 (const $ pure defaultLibFile)
      , getSourceFile: mkEffectFn2 \fileName _ -> do
          case find (eq fileName <<< _.path) sourceFiles' of
            Just { file } -> pure $ opt file
            Nothing -> pure undefined
      , readFile: mkEffectFn1 \fileName -> do
          case find (eq fileName <<< _.path) sourceFiles' of
            Just { source } -> pure $ opt source
            Nothing -> pure undefined
      , useCaseSensitiveFileNames: pure true
      , writeFile: mkEffectFn3 (\_ _ _ -> pure unit)
      }
  pure host
