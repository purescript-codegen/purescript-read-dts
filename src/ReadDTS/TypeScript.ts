import * as ts from "typescript";

export function isNodeExportedImpl(checker:ts.TypeChecker, node: ts.Node): boolean {
  let sym = checker.getSymbolAtLocation(node);
    return (
      sym? ((ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0):false ||
      (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile && node.kind !== ts.SyntaxKind.EndOfFileToken)
    )
};

export const showSyntaxKind = (node: ts.Node): string => ts.SyntaxKind[node.kind];