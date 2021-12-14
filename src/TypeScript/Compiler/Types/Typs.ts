import * as ts from "typescript";

export const asNumberLiteralTypeImpl = (t: ts.Type): ts.NumberLiteralType | null => t.isNumberLiteral()?t:null;

export interface PlainObjectType extends ts.Type {
  objectFlags: ts.ObjectFlags;
  members?: ts.SymbolTable;             // Properties by name
  properties?: ts.Symbol[];             // Properties
}

export const asObjectTypeImpl = (t: ts.Type): ts.ObjectType | null => {
  if (!(t.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)))
    return null;
  if(t.isClassOrInterface())
    return null;

  let ot = <ts.ObjectType>t;
  return ot;

  if((ot.objectFlags & ts.ObjectFlags.Mapped) && (ot.objectFlags & ts.ObjectFlags.Instantiated))
    return ot;

  //   let objDeclarations = memObjectType.symbol.getDeclarations();
  //   let props = memObjectType.getProperties().map((sym: ts.Symbol) =>
  //     property(sym, objDeclarations?objDeclarations[0]:sym.declarations?sym.declarations[1]:sym.valueDeclaration)
  //   );
  //   return onTypeNode.object(props);
  return null;
}

export const asStringLiteralTypeImpl = (t: ts.Type): ts.StringLiteralType | null => t.isStringLiteral()?t:null;

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


