import * as ts from "typescript";

export interface TypeChecker extends ts.TypeChecker {
  // We don't have an easy access to values like `Checker.globalArrayType`, `Checker.globalStringType`
  // nor to appropriate methods which we are exposing below:
  //
  // I've additionally restricted some signatures.
  getElementTypeOfArrayType: (type: ts.TypeReference) =>  ts.Type | undefined;
  isArrayType: (type: ts.TypeReference) => boolean;
  isTupleType: (type: ts.TypeReference) => boolean;
  isReadonlyArrayType: (type: ts.TypeReference) => boolean;
  getTypeOfPropertyOfType: (type: ts.TypeReference, propertyName: string) => ts.Type | undefined;
  getNullType: () => ts.Type;
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

export const getElementTypeOfArrayTypeImpl = (checker: TypeChecker, type: ts.TypeReference): ts.Type | null => checker.getElementTypeOfArrayType(type) || null;

export const isArrayTypeImpl = (checker: TypeChecker, type: ts.TypeReference): boolean => checker.isArrayType(type);

export const isTupleTypeImpl = (checker: TypeChecker, type: ts.TypeReference): boolean => checker.isTupleType(type);

export const isReadonlyArrayTypeImpl = (checker: TypeChecker, type: ts.TypeReference): boolean  => checker.isReadonlyArrayType(type);

export const getTypeOfPropertyOfTypeImpl = (checker: TypeChecker, type: ts.TypeReference, propertyName: string): ts.Type | null => checker.getTypeOfPropertyOfType(type, propertyName) || null;

export const isNullTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.getNullType() === type;

export const isUndefinedTypeImpl = (checker: TypeChecker, type: ts.Type): boolean => checker.getUndefinedType() === type;


