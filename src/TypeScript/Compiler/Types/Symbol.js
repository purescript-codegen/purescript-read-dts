import ts from "typescript";
// src/compiller/types.ts:
// export interface Symbol {
//  flags: SymbolFlags;                     // Symbol flags
//  escapedName: __String;                  // Name of symbol
//  declarations?: Declaration[];           // Declarations associated with this symbol
//  valueDeclaration?: Declaration;         // First value declaration of the symbol
//  members?: SymbolTable;                  // Class, interface or object literal instance members
//  exports?: SymbolTable;                  // Module exports
//  globalExports?: SymbolTable;            // Conditional global UMD exports
export const symbolFlags = ts.SymbolFlags;
export const getFlagsImpl = (s) => s.getFlags();
export const getNameImpl = (s) => s.getName();
export const getDeclarationsImpl = (s) => s.getDeclarations() || [];
export const getExportsImpl = (s) => s.exports || [];
//# sourceMappingURL=Symbol.js.map
