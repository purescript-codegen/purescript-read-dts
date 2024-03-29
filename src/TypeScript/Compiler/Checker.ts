import ts from "typescript";

export const getExportsOfModule = (checker: ts.TypeChecker, moduleSymbol: ts.Symbol): ts.Symbol[] => {
  return checker.getExportsOfModule(moduleSymbol);
}

export const typeToStringImpl = (checker: ts.TypeChecker, type: ts.Type): string => checker.typeToString(type)

export const getSymbolAtLocationImpl = (checker: ts.TypeChecker, node: ts.Node): ts.Symbol | null => checker.getSymbolAtLocation(node) || null;

export const getTypeAtLocationImpl = (checker: ts.TypeChecker, node: ts.Node): ts.Type | null => checker.getTypeAtLocation(node) || null;

export const getTypeOfSymbolAtLocationImpl = (checker: ts.TypeChecker, symbol: ts.Symbol, node: ts.Node): ts.Type | null => checker.getTypeOfSymbolAtLocation(symbol, node);

export const getFullyQualifiedNameImpl = (checker: ts.TypeChecker, symbol: ts.Symbol): string => checker.getFullyQualifiedName(symbol);

export const getTypeArgumentsImpl = (checker: ts.TypeChecker, type: ts.TypeReference): readonly ts.Type[] => checker.getTypeArguments(type);

export const getSignaturesOfTypeImpl = (checker: ts.TypeChecker, type: ts.Type, kind: ts.SignatureKind): readonly ts.Signature[] => checker.getSignaturesOfType(type, kind);

export const getExportSymbolOfSymbolImpl = (checker: ts.TypeChecker, symbol: ts.Symbol): ts.Symbol => checker.getExportSymbolOfSymbol(symbol);



