import * as ts from "typescript";

export const typeToStringImpl = (checker: ts.TypeChecker, type: ts.Type): string => checker.typeToString(type)

export const getSymbolAtLocationImpl = (checker: ts.TypeChecker, node: ts.Node): ts.Symbol | null => checker.getSymbolAtLocation(node) || null;

export const getTypeAtLocationImpl = (checker: ts.TypeChecker, node: ts.Node): ts.Type | null => checker.getTypeAtLocation(node) || null;

export const getFullyQualifiedNameImpl = (checker: ts.TypeChecker, symbol: ts.Symbol): string => checker.getFullyQualifiedName(symbol);

export const getTypeArgumentsImpl = (checker: ts.TypeChecker, type: ts.TypeReference): readonly ts.Type[] => checker.getTypeArguments(type);

