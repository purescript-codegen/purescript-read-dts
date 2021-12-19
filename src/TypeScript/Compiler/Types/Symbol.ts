import * as ts from "typescript";

export const symbolFlags = ts.SymbolFlags;

export const getFlagsImpl = (s: ts.Symbol): ts.SymbolFlags => s.getFlags()

export const getNameImpl = (s: ts.Symbol): string => s.getName()

export const getDeclarationsImpl = (s: ts.Symbol): ts.Declaration[] | null => s.getDeclarations() || [];
