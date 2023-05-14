import ts from "typescript";

// This is internal API `compiler/debug.ts`
type Debug = {
  formatSyntaxKind: (kind: ts.SyntaxKind | undefined) => string,
  formatNodeFlags: (flags: ts.NodeFlags | undefined) => string,
  // `ModifierFlags` type is exposed but its field in the `Node` is `internal`.
  formatModifierFlags: (flags: ts.ModifierFlags | undefined) => string,
  formatEmitFlags: (flags: ts.EmitFlags | undefined) => string,
  formatSymbolFlags: (flags: ts.SymbolFlags | undefined) => string,
  formatTypeFlags: (flags: ts.TypeFlags | undefined) => string,
  formatObjectFlags: (flags: ts.ObjectFlags | undefined) => string,
  formatFlowFlags: (flags: ts.FlowFlags | undefined) => string,

  // These flags types are not exposed:
  // formatTransformFlags: (flags: ts.TransformFlags | undefined) => string,
  // formatSignatureFlags: (flags: ts.SignatureFlags | undefined) => string,
}

var TsDebug = (<{ Debug: Debug }>(<unknown>ts)).Debug;

export const formatSyntaxKind = (kind: ts.SyntaxKind) => TsDebug.formatSyntaxKind(kind);
export const formatNodeFlags = (flags: ts.NodeFlags) => TsDebug.formatNodeFlags(flags);
export const formatModifierFlags = (flags: ts.ModifierFlags) => TsDebug.formatModifierFlags(flags);
export const formatEmitFlags = (flags: ts.EmitFlags) => TsDebug.formatEmitFlags(flags);
export const formatSymbolFlags = (flags: ts.SymbolFlags) => TsDebug.formatSymbolFlags(flags);
export const formatTypeFlags = (flags: ts.TypeFlags) => TsDebug.formatTypeFlags(flags);
// export const formatTransformFlags = (flags: ts.TransformFlags) => TsDebug.formatTransformFlags(flags);
// export const formatSignatureFlags = (flags: ts.SignatureFlags) => TsDebug.formatSignatureFlags(flags);
export const formatObjectFlags = (flags: ts.ObjectFlags) => TsDebug.formatObjectFlags(flags);
export const formatFlowFlags = (flags: ts.FlowFlags) => TsDebug.formatFlowFlags(flags);
