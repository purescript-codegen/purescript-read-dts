module TypeScript.Testing.BoundedCompilerHost where

import Prelude

import Data.Undefined.NoProblem (Opt)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, mkEffectFn2, mkEffectFn3, runEffectFn3)
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

-- newtype BoundedCompilerHost' = BoundedCompilerHost' BoundedCompilerHost
-- 
-- appendEffectFn1With :: forall a b. (b -> b -> b) -> EffectFn1 a b -> EffectFn1 a b -> EffectFn1 a b
-- appendEffectFn1With f f1 f2 = mkEffectFn1 \a -> f <$> (runEffectFn1 f1 a) <*> (runEffectFn1 f2 a)
-- 
-- appendEffectFn1 :: forall a b. Semigroup b => EffectFn1 a b -> EffectFn1 a b -> EffectFn1 a b
-- appendEffectFn1 = appendEffectFn1With (<>)
-- 
-- appendEffectFn2With :: forall a b c. (c -> c -> c) -> EffectFn2 a b c -> EffectFn2 a b c -> EffectFn2 a b c
-- appendEffectFn2With f f1 f2 = mkEffectFn2 \a b -> f <$> (runEffectFn2 f1 a b) <*> (runEffectFn2 f2 a b)
-- 
-- appendEffectFn2 :: forall a b c. Semigroup c => EffectFn2 a b c -> EffectFn2 a b c -> EffectFn2 a b c
-- appendEffectFn2 = appendEffectFn2With (<>)
-- 
-- instance Semigroup BoundedCompilerHost' where
--   append (BoundedCompilerHost' a) (BoundedCompilerHost' b) = do
--     let
--       directoryExists = appendEffectFn1With (||) a.directoryExists b.directoryExists
--       fileExists = appendEffectFn1With (||) a.fileExists b.fileExists

--     let
--       directoryExists = a.directoryExists <> b.directoryExists
--     BoundedCompilerHost'
--       { directoryExists: a.directoryExists <> b.directoryExists
--       , fileExists: a.fileExists <> b.fileExists
--       , getCanonicalFileName: a.getCanonicalFileName <> b.getCanonicalFileName
--       , getCurrentDirectory: a.getCurrentDirectory <> b.getCurrentDirectory
--       , getDefaultLibFileName: a.getDefaultLibFileName <> b.getDefaultLibFileName
--       , getDirectories: a.getDirectories <> b.getDirectories
--       , getNewLine: a.getNewLine <> b.getNewLine
--       , getSourceFile: a.getSourceFile <> b.getSourceFile
--       , readFile: a.readFile <> b.readFile
--       , useCaseSensitiveFileNames: a.useCaseSensitiveFileNames <> b.useCaseSensitiveFileNames
--       , writeFile: a.writeFile <> b.writeFile
--       }
-- 
