"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getFullyQualifiedNameImpl = exports.getTypeAtLocationImpl = exports.getSymbolAtLocationImpl = exports.typeToStringImpl = void 0;
var typeToStringImpl = function (checker, type) { return checker.typeToString(type); };
exports.typeToStringImpl = typeToStringImpl;
var getSymbolAtLocationImpl = function (checker, node) { return checker.getSymbolAtLocation(node) || null; };
exports.getSymbolAtLocationImpl = getSymbolAtLocationImpl;
var getTypeAtLocationImpl = function (checker, node) { return checker.getTypeAtLocation(node) || null; };
exports.getTypeAtLocationImpl = getTypeAtLocationImpl;
var getFullyQualifiedNameImpl = function (checker, symbol) { return checker.getFullyQualifiedName(symbol); };
exports.getFullyQualifiedNameImpl = getFullyQualifiedNameImpl;
//# sourceMappingURL=Checker.js.map