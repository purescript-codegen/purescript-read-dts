;
export const getElementTypeOfArrayTypeImpl = (checker, type) => checker.getElementTypeOfArrayType(type) || null;
export const isAnyTypeImpl = (checker, type) => checker.getAnyType() == type;
export const isArrayTypeImpl = (checker, type) => checker.isArrayType(type);
export const isBooleanTypeImpl = (checker, type) => checker.getBooleanType() == type;
export const isNullTypeImpl = (checker, type) => checker.getNullType() == type;
export const isNumberTypeImpl = (checker, type) => checker.getNumberType() == type;
export const isReadonlyArrayTypeImpl = (checker, type) => checker.isReadonlyArrayType(type);
export const isStringTypeImpl = (checker, type) => checker.getStringType() == type;
export const isTupleTypeImpl = (checker, type) => checker.isTupleType(type);
export const isUndefinedTypeImpl = (checker, type) => checker.getUndefinedType() == type;
export const getTypeOfPropertyOfTypeImpl = (checker, type, propertyName) => checker.getTypeOfPropertyOfType(type, propertyName) || null;
//# sourceMappingURL=Internal.js.map