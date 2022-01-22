import * as ts from "typescript";

export const asNumberLiteralTypeImpl = (t: ts.Type): ts.NumberLiteralType | null => t.isNumberLiteral()?t:null;

export const asIntersectionTypeImpl = (t: ts.Type): ts.IntersectionType | null => t.isIntersection()?t:null;

export interface PlainObjectType extends ts.Type {
  objectFlags: ts.ObjectFlags;
  members?: ts.SymbolTable;             // Properties by name
  properties?: ts.Symbol[];             // Properties
}


export const asObjectTypeImpl = (t: ts.Type): ts.ObjectType | null => {
  if (t.flags & ts.TypeFlags.Object)
    return <ts.ObjectType>t;
  return null;
}

export const asStringLiteralTypeImpl = (t: ts.Type): ts.StringLiteralType | null => t.isStringLiteral()?t:null;

export const asUnionTypeImpl = (t: ts.Type): ts.UnionType | null => t.isUnion()?t:null;

export const asTypeParameterImpl = (t: ts.Type): ts.TypeParameter | null => t.isTypeParameter()?t:null;

export const asTypeReferenceImpl = (t: ts.Type): ts.TypeReference | null => {
  // There is no sens at the moment to expose ObjectType casting I think to the PS side...
  const isObjectType = (t: ts.Type): t is ts.ObjectType => {
    let Nullable = ts.TypeFlags.Undefined | ts.TypeFlags.Null;
    let ObjectFlagsType = ts.TypeFlags.Any | Nullable | ts.TypeFlags.Never | ts.TypeFlags.Object | ts.TypeFlags.Union | ts.TypeFlags.Intersection;
    return !!(t.flags & ObjectFlagsType);
  }

  if(isObjectType(t) && !!(t.objectFlags & ts.ObjectFlags.Reference))
    return <ts.TypeReference>t;

  return null;
}

export const getPropertiesImpl = (t: ts.Type): ts.Symbol[] => t.getProperties();

export const asInterfaceTypeImpl = (t: ts.ObjectType): ts.InterfaceType | null => {
  if(t.objectFlags & ts.ObjectFlags.Interface) {
    return <ts.InterfaceType>t;
  }
  return null;
}

export const asClassTypeImpl = (t: ts.ObjectType): ts.InterfaceType | null => {
  if(t.objectFlags & ts.ObjectFlags.Class) {
    return <ts.InterfaceType>t;
  }
  return null;
}

export const getSymbolImpl = (t: ts.Type): ts.Symbol | null => t.getSymbol() || null;

export const getDefaultImpl = (t: ts.Type): ts.Type | null => t.getDefault() || null;
