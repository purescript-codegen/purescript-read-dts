module TypeScript.Testing.InMemory where

import Prelude

import Data.Array (any, cons) as Array
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Traversable (for)
import Data.Undefined.NoProblem (Opt, opt, undefined, (!))
import Data.Undefined.NoProblem.Closed (class Coerce, coerce) as Closed
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2)
import Node.Buffer (toString) as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readFile)
import Node.Path (FilePath)
import TypeScript.Compiler.Parser (FileName, SourceCode(..), createSourceFile)
import TypeScript.Compiler.Types (CompilerHost, CompilerOptions, ScriptTarget, scriptTarget)
import TypeScript.Compiler.Types.Nodes (SourceFile)
import TypeScript.Compiler.Types.Nodes (interface) as Node
import Unsafe.Coerce (unsafeCoerce)

type DirName = String
type WriteByteOrderMark = Boolean

type WriteFileCallback = EffectFn3 FileName SourceCode WriteByteOrderMark Unit

type CompilerHost' =
  { directoryExists :: Opt (EffectFn1 DirName Boolean)
  , fileExists :: EffectFn1 FileName Boolean
  , getCanonicalFileName :: EffectFn1 FileName FileName
  , getCurrentDirectory :: Opt (Effect String)
  , getDefaultLibFileName :: EffectFn1 CompilerOptions FileName
  , getDirectories :: Opt (EffectFn1 String (Array String))
  , getNewLine :: Effect String
  , getSourceFile :: EffectFn2 FileName ScriptTarget (Opt SourceFile)
  , readFile :: EffectFn1 FileName (Opt SourceCode)
  , useCaseSensitiveFileNames :: Effect Boolean
  , writeFile :: WriteFileCallback
  }

foreign import bindCompilerHost :: CompilerHost -> CompilerHost'

toCompilerHost :: CompilerHost' -> CompilerHost
toCompilerHost = unsafeCoerce

newtype File = File { path :: FileName, source :: SourceCode }
derive instance Newtype File _

mkFile :: FilePath -> SourceCode -> File
mkFile name source = File
  { path: name
  , source: source
  }

handleMemoryFiles :: CompilerHost -> Array File -> Effect CompilerHost
handleMemoryFiles realHost inMemoryFiles = do
  sourceFiles <- for inMemoryFiles \(File { path, source }) -> do
    createSourceFile path source scriptTarget."ES5" true

  let
    paths = map (_.path <<< un File) inMemoryFiles
    realHost' = bindCompilerHost realHost

    host :: CompilerHost'
    host = realHost'
      { fileExists = mkEffectFn1 \p -> do
          ((Array.any (eq p) paths) || _) <$> runEffectFn1 realHost'.fileExists p
      , getSourceFile = mkEffectFn2 \fileName scriptTarget -> do
          case find (eq fileName <<< _.fileName <<< Node.interface) sourceFiles of
            Just sourceFile -> pure $ opt sourceFile
            Nothing -> runEffectFn2 realHost'.getSourceFile fileName scriptTarget
      , readFile = mkEffectFn1 \fileName -> do
          case find (eq fileName <<< _.fileName <<< Node.interface) sourceFiles of
            Just sourceFile -> pure $ opt (Node.interface sourceFile # SourceCode <<< _.text)
            Nothing -> runEffectFn1 realHost'.readFile fileName
      , writeFile = mkEffectFn3 (\_ _ _ -> pure unit)
      }
  pure $ toCompilerHost host

type Opts =
  { files :: Array File
  , defaultLib :: Opt FilePath
  }

compilerHost :: forall opts. Closed.Coerce opts Opts => opts -> Effect CompilerHost
compilerHost opts = do
  let
    { defaultLib, files } = Closed.coerce opts :: Opts
    defaultLibFile = defaultLib ! "node_modules/typescript/lib/lib.es5.d.ts"

  defaultLibFileSrc <- exists defaultLibFile >>= if _
    then do
      b <- readFile defaultLibFile
      Buffer.toString UTF8 b
    else
      throw $
        "inMemoryCompilerHost: Unable to find default `CompilerHost` library file:" <> defaultLibFile
  let
    files' = Array.cons
      (File { path: defaultLibFile, source: SourceCode defaultLibFileSrc })
      files

  sourceFiles <- for files' \(File { path, source }) -> do
    file <- createSourceFile path source scriptTarget."ES5" true
    pure { path, source, file }
  let
    host :: CompilerHost'
    host =
      { fileExists: mkEffectFn1 \p -> do
          pure (Array.any (eq p <<< _.path) sourceFiles)
      , directoryExists: opt $ mkEffectFn1 \d -> do
          pure (d == "")
      , getCurrentDirectory: opt $ pure ""
      , getDirectories: opt $ mkEffectFn1 $ const (pure [""])
      , getCanonicalFileName: mkEffectFn1 pure
      , getNewLine: pure "\n"
      , getDefaultLibFileName: mkEffectFn1 (const $ pure defaultLibFile)
      , getSourceFile: mkEffectFn2 \fileName _ -> do
          case find (eq fileName <<< _.path) sourceFiles of
            Just { file } -> pure $ opt file
            Nothing -> pure undefined
      , readFile: mkEffectFn1 \fileName -> do
          case find (eq fileName <<< _.path) sourceFiles of
            Just { source } -> pure $ opt source
            Nothing -> pure undefined
      , useCaseSensitiveFileNames: pure true
      , writeFile: mkEffectFn3 (\_ _ _ -> pure unit)
      }
  pure $ toCompilerHost host

