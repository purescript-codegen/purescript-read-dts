module TypeScript.Testing.InMemory where

import Prelude

import Data.Array (any, cons) as Array
import Data.Array (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Data.Undefined.NoProblem (opt, undefined)
import Data.Undefined.NoProblem as NoProblem
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2)
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
  -- You can provide a fallback compiler host to handle files that are not in memory.
  -- It is a bit speculative if the fallback wokrs as expected.
  , subhost :: Maybe BoundedCompilerHost
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
    inMemoryHost =
      { fileExists: mkEffectFn1 \p -> do
          pure (Array.any (eq p <<< _.path) sourceFiles')
      , directoryExists: opt $ mkEffectFn1 \d -> do
          pure (d == "")
      , getCurrentDirectory: NoProblem.undefined -- opt $ pure ""
      , getDirectories: NoProblem.undefined -- opt $ mkEffectFn1 $ const (pure [ "" ])
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
    host :: BoundedCompilerHost
    host = case opts.subhost of
      Nothing -> inMemoryHost
      Just subhost ->
        { fileExists: mkEffectFn1 \p -> do
            (||) <$> runEffectFn1 subhost.fileExists p <*> runEffectFn1 inMemoryHost.fileExists p
        , directoryExists: opt $ mkEffectFn1 \d -> do
            (||) <$> runOptEffectFn1 subhost.directoryExists false d <*> runOptEffectFn1 inMemoryHost.directoryExists false d
        , getCanonicalFileName: subhost.getCanonicalFileName
        , getDefaultLibFileName: subhost.getDefaultLibFileName
        , getNewLine: subhost.getNewLine
        , useCaseSensitiveFileNames: subhost.useCaseSensitiveFileNames
        , writeFile: subhost.writeFile
        , getCurrentDirectory: subhost.getCurrentDirectory
        , getDirectories: subhost.getDirectories
        , getSourceFile: mkEffectFn2 \p x -> do
            runEffectFn2 subhost.getSourceFile p x >>= NoProblem.toMaybe >>> case _ of
              Just sourceFile -> pure $ opt sourceFile
              Nothing -> runEffectFn2 inMemoryHost.getSourceFile p x
        , readFile: mkEffectFn1 \p -> do
            runEffectFn1 subhost.readFile p >>= NoProblem.toMaybe >>> case _ of
              Just source -> pure $ opt source
              Nothing -> runEffectFn1 inMemoryHost.readFile p
        }

  pure host

runOptEffectFn1 :: forall i o. NoProblem.Opt (EffectFn1 i o) -> o -> i -> Effect o
runOptEffectFn1 optFnEff1 def = NoProblem.pseudoMap runEffectFn1 optFnEff1 NoProblem.! (const $ pure def)


