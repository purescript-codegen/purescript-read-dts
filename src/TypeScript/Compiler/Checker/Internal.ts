import * as ts from "typescript";

export interface TypeChecker extends ts.TypeChecker {
  // I've additionally restricted some signatures.
  getElementTypeOfArrayType: (type: ts.Type) =>  ts.Type | undefined;
  isArrayType: (type: ts.Type) => boolean;
  isTupleType: (type: ts.Type) => boolean;
  isReadonlyArrayType: (type: ts.Type) => boolean;
  getTypeOfPropertyOfType: (type: ts.Type, propertyName: string) => ts.Type | undefined;

  getAnyType: () => ts.Type;
  getBooleanType: () => ts.Type;
  getNumberType: () => ts.Type;
  getNullType: () => ts.Type;
  getStringType: () => ts.Type;
  getUndefinedType: () => ts.Type;

  // Some other possible helpers
  // isTupleLikeType: (type: ts.Type) => boolean;
  // isArrayLikeType: (type: ts.Type) => boolean;
  // isEmptyArrayLiteralType: (type: ts.Type) => boolean;
  // isArrayOrTupleLikeType: (type: ts.Type) => boolean;
  // isNeitherUnitTypeNorNever: (type: ts.Type) => boolean;
  // isUnitType: (type: ts.Type) => boolean;
  // isLiteralType: (type: ts.Type) => boolean;
};

export const getElementTypeOfArrayTypeImpl = (checker: TypeChecker, type: ts.Type): ts.Type | null => checker.getElementTypeOfArrayType(type) || null;

export const isAnyTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.getAnyType() == type;

export const isArrayTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.isArrayType(type);

export const isBooleanTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.getBooleanType() == type;

export const isNullTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.getNullType() == type;

export const isNumberTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.getNumberType() == type;

export const isReadonlyArrayTypeImpl = (checker: TypeChecker, type: ts.Type): boolean  => checker.isReadonlyArrayType(type);

export const isStringTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.getStringType() == type;

export const isTupleTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.isTupleType(type);

export const isUndefinedTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.getUndefinedType() == type;

export const getTypeOfPropertyOfTypeImpl = (checker: TypeChecker, type: ts.Type, propertyName: string): ts.Type | null => checker.getTypeOfPropertyOfType(type, propertyName) || null;

