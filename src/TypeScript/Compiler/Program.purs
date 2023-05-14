module TypeScript.Compiler.Program where

import TypeScript.Compiler.Types

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import TypeScript.Compiler.Parser (FileName)
import TypeScript.Compiler.Types.Diagnostics (Diagnostic)
import TypeScript.Compiler.Types.Nodes (SourceFile)

createCompilerHost :: CompilerOptions -> Effect CompilerHost
createCompilerHost = runEffectFn1 createCompilerHostImpl

foreign import createCompilerHostImpl :: EffectFn1 CompilerOptions CompilerHost

type RootNames = Array String

createProgram :: Array String -> CompilerOptions -> Maybe CompilerHost -> Effect Program
createProgram rootNames opts host = runEffectFn3 createProgramImpl rootNames opts (toNullable host)

foreign import createProgramImpl :: EffectFn3 (Array String) CompilerOptions (Nullable CompilerHost) Program

foreign import getTypeChecker :: Program -> TypeChecker

foreign import getRootFileNames :: Program -> Array FileName

foreign import getSourceFiles :: Program -> Array SourceFile

foreign import getFileName :: SourceFile -> String

foreign import emit :: Program -> Effect EmitResult

foreign import emitResultDiagnostics :: EmitResult -> Array Diagnostic

foreign import getPreEmitDiagnostics :: Program -> Array Diagnostic
