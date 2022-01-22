import * as ts from "typescript";

export function getDeclaration(s: ts.Signature): ts.SignatureDeclaration {
    return s.getDeclaration() || null;
}
export function getTypeParameters(s: ts.Signature): ts.TypeParameter[] {
    return s.getTypeParameters() || [];
}

export function getParameters(s: ts.Signature): ts.Symbol[] {
    return s.getParameters();
}

export function getReturnType(s: ts.Signature): ts.Type {
    return s.getReturnType();
}

// export function getDocumentationComment(): SymbolDisplayPart[] {
//     return this.documentationComment || (this.documentationComment = getDocumentationComment(singleElementArray(this.declaration), this.checker));
// }
