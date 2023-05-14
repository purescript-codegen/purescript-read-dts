export const typeToStringImpl = (checker, type) => checker.typeToString(type);
export const getSymbolAtLocationImpl = (checker, node) => checker.getSymbolAtLocation(node) || null;
export const getTypeAtLocationImpl = (checker, node) => checker.getTypeAtLocation(node) || null;
export const getTypeOfSymbolAtLocationImpl = (checker, symbol, node) => checker.getTypeOfSymbolAtLocation(symbol, node);
export const getFullyQualifiedNameImpl = (checker, symbol) => checker.getFullyQualifiedName(symbol);
export const getTypeArgumentsImpl = (checker, type) => checker.getTypeArguments(type);
export const getSignaturesOfTypeImpl = (checker, type, kind) => checker.getSignaturesOfType(type, kind);
export const getExportSymbolOfSymbolImpl = (checker, symbol) => checker.getExportSymbolOfSymbol(symbol);
export const getExportsOfModuleImpl = (checker, symbol) => checker.getExportsOfModule(symbol);
export const getExportsAndPropertiesOfModuleImpl = (checker, symbol) => checker.getExportsAndPropertiesOfModule(symbol);

// export const getPropertiesOfTypeImpl = (checker, type) => checker.getPropertiesOfType(type);
//# sourceMappingURL=Checker.js.map
