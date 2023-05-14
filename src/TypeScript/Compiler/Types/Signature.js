export function getDeclaration(s) {
    return s.getDeclaration() || null;
}
export function getTypeParameters(s) {
    return s.getTypeParameters() || [];
}
export function getParameters(s) {
    return s.getParameters();
}
export function getReturnType(s) {
    return s.getReturnType();
}
// export function getDocumentationComment(): SymbolDisplayPart[] {
//     return this.documentationComment || (this.documentationComment = getDocumentationComment(singleElementArray(this.declaration), this.checker));
// }
//# sourceMappingURL=Signature.js.map