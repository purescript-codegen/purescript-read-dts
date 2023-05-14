import ts from "typescript";

export const asNumberLiteralTypeImpl = (t: ts.Type): ts.NumberLiteralType | null => t.isNumberLiteral()?t:null;

export const asIntersectionTypeImpl = (t: ts.Type): ts.IntersectionType | null => t.isIntersection()?t:null;

export interface PlainObjectType extends ts.Type {
  objectFlags: ts.ObjectFlags;
  members?: ts.SymbolTable;             // Properties by name
  properties?: ts.Symbol[];             // Properties
}

export const asObjectTypeImpl = (t: ts.Type): ts.ObjectType | null => {
  let Nullable = ts.TypeFlags.Undefined | ts.TypeFlags.Null;
  let ObjectFlagsType = ts.TypeFlags.Any | Nullable | ts.TypeFlags.Never | ts.TypeFlags.Object | ts.TypeFlags.Union | ts.TypeFlags.Intersection;

  if (t.flags & ObjectFlagsType)
    return <ts.ObjectType>t;
  return null;
}

export const asStringLiteralTypeImpl = (t: ts.Type): ts.StringLiteralType | null => t.isStringLiteral()?t:null;

export const asUnionTypeImpl = (t: ts.Type): ts.UnionType | null => t.isUnion()?t:null;

export const asTypeParameterImpl = (t: ts.Type): ts.TypeParameter | null => t.isTypeParameter()?t:null;

export const asTypeReferenceImpl = (t: ts.Type): ts.TypeReference | null => {
  // There is no sens at the moment to expose ObjectType casting I think to the PS side...
  let s = asObjectTypeImpl(t);
  if(s && s.objectFlags & ts.ObjectFlags.Reference)
    return <ts.TypeReference>s;

  return null;
}

export const getCallSignatures = (t: ts.Type): readonly ts.Signature[] => t.getCallSignatures();

export const getConstructSignatures = (t: ts.Type): readonly ts.Signature[] => t.getConstructSignatures();

export const getPropertiesImpl = (t: ts.Type): ts.Symbol[] => t.getProperties();

export const getApparentPropertiesImpl = (t: ts.Type): ts.Symbol [] => t.getApparentProperties();

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

export const getBaseTypesImpl = (t: ts.Type): ts.BaseType[] => t.getBaseTypes() || [];

