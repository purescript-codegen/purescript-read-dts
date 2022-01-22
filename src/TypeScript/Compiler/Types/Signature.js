"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getReturnType = exports.getParameters = exports.getTypeParameters = exports.getDeclaration = void 0;
function getDeclaration(s) {
    return s.getDeclaration() || null;
}
exports.getDeclaration = getDeclaration;
function getTypeParameters(s) {
    return s.getTypeParameters() || [];
}
exports.getTypeParameters = getTypeParameters;
function getParameters(s) {
    return s.getParameters();
}
exports.getParameters = getParameters;
function getReturnType(s) {
    return s.getReturnType();
}
exports.getReturnType = getReturnType;
// export function getDocumentationComment(): SymbolDisplayPart[] {
//     return this.documentationComment || (this.documentationComment = getDocumentationComment(singleElementArray(this.declaration), this.checker));
// }
//# sourceMappingURL=Signature.js.map