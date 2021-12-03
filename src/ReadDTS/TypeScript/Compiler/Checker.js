"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.isUndefinedTypeImpl = exports.isNullTypeImpl = exports.getTypeOfPropertyOfTypeImpl = exports.isReadonlyArrayTypeImpl = exports.isTupleTypeImpl = exports.isArrayTypeImpl = exports.getElementTypeOfArrayTypeImpl = void 0;
;
var getElementTypeOfArrayTypeImpl = function (checker, type) { return checker.getElementTypeOfArrayType(type) || null; };
exports.getElementTypeOfArrayTypeImpl = getElementTypeOfArrayTypeImpl;
var isArrayTypeImpl = function (checker, type) { return checker.isArrayType(type); };
exports.isArrayTypeImpl = isArrayTypeImpl;
var isTupleTypeImpl = function (checker, type) { return checker.isTupleType(type); };
exports.isTupleTypeImpl = isTupleTypeImpl;
var isReadonlyArrayTypeImpl = function (checker, type) { return checker.isReadonlyArrayType(type); };
exports.isReadonlyArrayTypeImpl = isReadonlyArrayTypeImpl;
var getTypeOfPropertyOfTypeImpl = function (checker, type, propertyName) { return checker.getTypeOfPropertyOfType(type, propertyName) || null; };
exports.getTypeOfPropertyOfTypeImpl = getTypeOfPropertyOfTypeImpl;
var isNullTypeImpl = function (checker, type) { return checker.getNullType() === type; };
exports.isNullTypeImpl = isNullTypeImpl;
var isUndefinedTypeImpl = function (checker, type) { return checker.getUndefinedType() === type; };
exports.isUndefinedTypeImpl = isUndefinedTypeImpl;
//# sourceMappingURL=Checker.js.map