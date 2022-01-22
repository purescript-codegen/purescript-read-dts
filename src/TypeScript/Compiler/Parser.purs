module TypeScript.Compiler.Parser where

import Effect (Effect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import TypeScript.Compiler.Types (ScriptTarget)
import TypeScript.Compiler.Types.Nodes (SourceFile)

type FileName = String
newtype SourceCode = SourceCode String
type SetParentNode = Boolean

-- export function createSourceFile(fileName: string, sourceText: string, languageVersion: ScriptTarget, setParentNodes = false, scriptKind?: ScriptKind): SourceFile ∷
createSourceFile :: FileName -> SourceCode -> ScriptTarget -> SetParentNode -> Effect SourceFile
createSourceFile = runEffectFn4 createSourceFileImpl

foreign import createSourceFileImpl :: EffectFn4 FileName SourceCode ScriptTarget SetParentNode SourceFile

