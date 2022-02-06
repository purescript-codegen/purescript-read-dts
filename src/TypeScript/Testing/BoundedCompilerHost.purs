module TypeScript.Testing.BoundedCompilerHost where

import Prelude

import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Node.Path (FilePath)
import TypeScript.Compiler.Parser (SourceCode)
import TypeScript.Compiler.Types (CompilerHost, CompilerOptions, ScriptTarget)
import TypeScript.Compiler.Types.Nodes (SourceFile)
import Unsafe.Coerce (unsafeCoerce)

type DirName = String
type WriteByteOrderMark = Boolean

type WriteFileCallback = EffectFn3 FilePath SourceCode WriteByteOrderMark Unit

-- | Record of compiler host proper functions - methods bounded to `this` compiler host instance
type BoundedCompilerHost =
  { directoryExists :: Opt (EffectFn1 DirName Boolean)
  , fileExists :: EffectFn1 FilePath Boolean
  , getCanonicalFileName :: EffectFn1 FilePath FilePath
  , getCurrentDirectory :: Opt (Effect String)
  , getDefaultLibFileName :: EffectFn1 CompilerOptions FilePath
  , getDirectories :: Opt (EffectFn1 String (Array String))
  , getNewLine :: Effect String
  , getSourceFile :: EffectFn2 FilePath ScriptTarget (Opt SourceFile)
  , readFile :: EffectFn1 FilePath (Opt SourceCode)
  , useCaseSensitiveFileNames :: Effect Boolean
  , writeFile :: WriteFileCallback
  }

foreign import fromCompilerHost :: CompilerHost -> BoundedCompilerHost

toCompilerHost :: BoundedCompilerHost -> CompilerHost
toCompilerHost = unsafeCoerce
