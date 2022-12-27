"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.formatFlowFlags = exports.formatObjectFlags = exports.formatTypeFlags = exports.formatSymbolFlags = exports.formatEmitFlags = exports.formatModifierFlags = exports.formatNodeFlags = exports.formatSyntaxKind = void 0;
var ts = __importStar(require("typescript"));
var TsDebug = ts.Debug;
var formatSyntaxKind = function (kind) { return TsDebug.formatSyntaxKind(kind); };
exports.formatSyntaxKind = formatSyntaxKind;
var formatNodeFlags = function (flags) { return TsDebug.formatNodeFlags(flags); };
exports.formatNodeFlags = formatNodeFlags;
var formatModifierFlags = function (flags) { return TsDebug.formatModifierFlags(flags); };
exports.formatModifierFlags = formatModifierFlags;
var formatEmitFlags = function (flags) { return TsDebug.formatEmitFlags(flags); };
exports.formatEmitFlags = formatEmitFlags;
var formatSymbolFlags = function (flags) { return TsDebug.formatSymbolFlags(flags); };
exports.formatSymbolFlags = formatSymbolFlags;
var formatTypeFlags = function (flags) { return TsDebug.formatTypeFlags(flags); };
exports.formatTypeFlags = formatTypeFlags;
// export const formatTransformFlags = (flags: ts.TransformFlags) => TsDebug.formatTransformFlags(flags);
// export const formatSignatureFlags = (flags: ts.SignatureFlags) => TsDebug.formatSignatureFlags(flags);
var formatObjectFlags = function (flags) { return TsDebug.formatObjectFlags(flags); };
exports.formatObjectFlags = formatObjectFlags;
var formatFlowFlags = function (flags) { return TsDebug.formatFlowFlags(flags); };
exports.formatFlowFlags = formatFlowFlags;
//# sourceMappingURL=Debug.js.map