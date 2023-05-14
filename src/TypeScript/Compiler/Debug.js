import ts from "typescript";
var TsDebug = ts.Debug;
export const formatSyntaxKind = (kind) => TsDebug.formatSyntaxKind(kind);
export const formatNodeFlags = (flags) => TsDebug.formatNodeFlags(flags);
export const formatModifierFlags = (flags) => TsDebug.formatModifierFlags(flags);
export const formatEmitFlags = (flags) => TsDebug.formatEmitFlags(flags);
export const formatSymbolFlags = (flags) => TsDebug.formatSymbolFlags(flags);
export const formatTypeFlags = (flags) => TsDebug.formatTypeFlags(flags);
// export const formatTransformFlags = (flags: ts.TransformFlags) => TsDebug.formatTransformFlags(flags);
// export const formatSignatureFlags = (flags: ts.SignatureFlags) => TsDebug.formatSignatureFlags(flags);
export const formatObjectFlags = (flags) => TsDebug.formatObjectFlags(flags);
export const formatFlowFlags = (flags) => TsDebug.formatFlowFlags(flags);
//# sourceMappingURL=Debug.js.map