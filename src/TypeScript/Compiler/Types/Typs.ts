import * as ts from "typescript";

export const asTypeReferenceImpl = (t: ts.Type): ts.TypeReference | null => {
  // There is no sens at the moment to expose ObjectType casting I think...
  const isObjectType = (t: ts.Type): t is ts.ObjectType => {
    let Nullable = ts.TypeFlags.Undefined | ts.TypeFlags.Null;
    let ObjectFlagsType = ts.TypeFlags.Any | Nullable | ts.TypeFlags.Never | ts.TypeFlags.Object | ts.TypeFlags.Union | ts.TypeFlags.Intersection;
    return !!(t.flags & ObjectFlagsType);
  }

  if(isObjectType(t) && !!(t.objectFlags & ts.ObjectFlags.Reference))
    return <ts.TypeReference>t;

  return null;
}

