module TypeScript.Compiler.Types.Diagnostics where

import Prelude

import Data.Newtype (class Newtype)
import Data.Undefined.NoProblem (Opt)
import JS.Object.Immutable (Immutable)
import JS.Variant.Untagged (Untagged, StringRow)
import Type.Row (type (+))
import TypeScript.Compiler.Types.Nodes (SourceFile)
import Unsafe.Reference (unsafeRefEq)

foreign import data EmitResult :: Type

foreign import data DiagnosticCategory :: Type

instance Eq DiagnosticCategory where
  eq = unsafeRefEq

foreign import diagnosticCategory :: Immutable
  ( "Warning" :: DiagnosticCategory
  , "Error" :: DiagnosticCategory
  , "Suggestion" :: DiagnosticCategory
  , "Message" :: DiagnosticCategory
  )

newtype DiagnosticMessageChain = DiagnosticMessageChain
  ( Immutable
    ( messageText :: String
    , category :: DiagnosticCategory
    , code :: Int
    , next :: Opt (Array DiagnosticMessageChain)
    )
  )
derive instance Newtype DiagnosticMessageChain _

type MessageText = Untagged (StringRow + (diagnosticMessageChain :: DiagnosticMessageChain))


-- export interface Diagnostic extends DiagnosticRelatedInformation {
--     /** May store more in future. For now, this will simply be `true` to indicate when a diagnostic is an unused-identifier diagnostic. */
--     reportsUnnecessary?: {};
-- 
--     reportsDeprecated?: {}
--     source?: string;
--     relatedInformation?: DiagnosticRelatedInformation[];
--     /** @internal */ skippedOn?: keyof CompilerOptions;
-- }
-- 
-- export interface DiagnosticRelatedInformation {
--     category: DiagnosticCategory;
--     code: number;
--     file: SourceFile | undefined;
--     start: number | undefined;
--     length: number | undefined;
--     messageText: string | DiagnosticMessageChain;
-- }

type DiagnosticRelatedInformationRow =
  ( category :: DiagnosticCategory
  , code :: Int
  , file ::  Opt SourceFile
  , start :: Opt Int
  , length :: Opt Int
  , messageText :: MessageText
  )

newtype DiagnosticRelatedInformation =
  DiagnosticRelatedInformation (Immutable DiagnosticRelatedInformationRow)
derive instance Newtype DiagnosticRelatedInformation _

newtype Diagnostic = Diagnostic
  ( Immutable
    ( source :: Opt String
    , relatedInformation :: Opt (Array DiagnosticRelatedInformation)
    | DiagnosticRelatedInformationRow
    )
  )
derive instance Newtype Diagnostic _
