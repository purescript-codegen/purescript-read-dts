"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
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
exports.toDeclarationStatementImpl = exports.formatObjectFlags = exports.formatSyntaxKind = exports.formatTypeFlags = exports.showSyntaxKind = exports.isNodeExportedImpl = void 0;
var ts = __importStar(require("typescript"));
function isNodeExportedImpl(checker, node) {
    var sym = checker.getSymbolAtLocation(node);
    return (sym ? ((sym.valueDeclaration && ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0) : false ||
        (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile && node.kind !== ts.SyntaxKind.EndOfFileToken));
}
exports.isNodeExportedImpl = isNodeExportedImpl;
;
var showSyntaxKind = function (node) { return ts.SyntaxKind[node.kind]; };
exports.showSyntaxKind = showSyntaxKind;
var formatTypeFlags = function (t) {
    var Debug = ts.Debug;
    return Debug.formatTypeFlags(t);
};
exports.formatTypeFlags = formatTypeFlags;
var formatSyntaxKind = function (node) {
    var Debug = ts.Debug;
    return Debug.formatSyntaxKind(node.kind);
};
exports.formatSyntaxKind = formatSyntaxKind;
var formatObjectFlags = function (o) {
    var Debug = ts.Debug;
    var objectFlags = ts.isObject(o) ? o.objectFlags : undefined;
    return Debug.formatObjectFlags(objectFlags);
};
exports.formatObjectFlags = formatObjectFlags;
var toDeclarationStatementImpl = function (n) {
    return n;
};
exports.toDeclarationStatementImpl = toDeclarationStatementImpl;
//# sourceMappingURL=TypeScript.js.map