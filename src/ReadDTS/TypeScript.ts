import * as ts from "typescript";

export function isNodeExportedImpl(checker:ts.TypeChecker, node: ts.Node): boolean {
  let sym = checker.getSymbolAtLocation(node);
    return (
      sym? ((sym.valueDeclaration && ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0):false ||
      (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile && node.kind !== ts.SyntaxKind.EndOfFileToken)
    )
};

export const showSyntaxKind = (node: ts.Node): string => ts.SyntaxKind[node.kind];

type Debug = {
  formatTypeFlags: (t: ts.Type) => string
  formatSyntaxKind: (kind: ts.SyntaxKind) => string
};

export const formatTypeFlags = (t: ts.Type): string => {
  let Debug = (<{ Debug: Debug }>(<unknown>ts)).Debug;
  return Debug.formatTypeFlags(t);
}

export const formatSyntaxKind = (node: ts.Node): string => {
  let Debug = (<{ Debug: Debug }>(<unknown>ts)).Debug;
  return Debug.formatSyntaxKind(node.kind);
}

export const toDeclarationStatementImpl = (n: ts.ClassDeclaration | ts.InterfaceDeclaration | ts.TypeAliasDeclaration): ts.DeclarationStatement => {
  return n;
}
