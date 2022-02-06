module TypeScript.Debug where

import Prelude

import TypeScript.Compiler.Types (Node, Symbol_, Typ, TypeFlags)
import TypeScript.Compiler.Types.Nodes (NodeFlags, SyntaxKind)
import TypeScript.Compiler.Types.Nodes (interface) as Nodes
import TypeScript.Compiler.Types.Symbol (SymbolFlags)
import TypeScript.Compiler.Types.Symbol (getFlags) as Symbol
import TypeScript.Compiler.Types.Typs (ObjectFlags)
import TypeScript.Compiler.Types.Typs (interface) as Typs

-- | This module exposes internal Debug namespace. Please
-- | be aware that like any other `internal` modules we are not able
-- | to typecheck on the ts side correctness of the exposed interface.
-- | We just hardcode and enforce the type.
-- | In other words we *should* provide test coverage for this module ;-)

foreign import formatSyntaxKind :: SyntaxKind -> String
foreign import formatNodeFlags :: NodeFlags -> String
foreign import formatSymbolFlags :: SymbolFlags -> String
foreign import formatTypeFlags :: TypeFlags -> String
foreign import formatObjectFlags :: ObjectFlags -> String

formatTypeFlags' :: forall i. Typ i -> String
formatTypeFlags' = formatTypeFlags <<< _.flags <<< Typs.interface

formatNodeFlags' :: forall l i. Node l i -> String
formatNodeFlags' = formatNodeFlags <<< _.nodeFlags <<< Nodes.interface

formatSymbolFlags' :: Symbol_ -> String
formatSymbolFlags' = formatSymbolFlags <<< Symbol.getFlags

-- We don't expose `EmitNode` at the moment
-- foreign import formatEmitFlags :: EmitFlags -> String
-- We don't expose `FlowNode` at the moment
-- foreign import formatFlowFlags :: FlowFlags -> String
-- These flags printers are possibly there but flags types
-- are not exposed by ts.
-- foreign import formatModifierFlags :: ModifierFlags -> String
-- foreign import :: formatTransformFlags :: TransformFlags -> String
-- foreign import :: formatSignatureFlags :: SignatureFlags -> String
