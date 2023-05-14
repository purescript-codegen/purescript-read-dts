import ts from "typescript";
export function isNodeExportedImpl(checker, node) {
    let sym = checker.getSymbolAtLocation(node);
    return (sym ? ((sym.valueDeclaration && ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0) : false ||
        (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile && node.kind !== ts.SyntaxKind.EndOfFileToken));
}
;
export const toDeclarationStatementImpl = (n) => {
    return n;
};
//# sourceMappingURL=TypeScript.js.map