import ts from "typescript";
export const asNumberLiteralTypeImpl = (t) => t.isNumberLiteral() ? t : null;
export const asIntersectionTypeImpl = (t) => t.isIntersection() ? t : null;
export const asObjectTypeImpl = (t) => {
    let Nullable = ts.TypeFlags.Undefined | ts.TypeFlags.Null;
    let ObjectFlagsType = ts.TypeFlags.Any | Nullable | ts.TypeFlags.Never | ts.TypeFlags.Object | ts.TypeFlags.Union | ts.TypeFlags.Intersection;
    if (t.flags & ObjectFlagsType)
        return t;
    return null;
};
export const asStringLiteralTypeImpl = (t) => t.isStringLiteral() ? t : null;
export const asUnionTypeImpl = (t) => t.isUnion() ? t : null;
export const asTypeParameterImpl = (t) => t.isTypeParameter() ? t : null;
export const asTypeReferenceImpl = (t) => {
    // There is no sens at the moment to expose ObjectType casting I think to the PS side...
    let s = asObjectTypeImpl(t);
    if (s && s.objectFlags & ts.ObjectFlags.Reference)
        return s;
    return null;
};
export const getCallSignatures = (t) => t.getCallSignatures();
export const getConstructSignatures = (t) => t.getConstructSignatures();
export const getPropertiesImpl = (t) => t.getProperties();
export const getApparentPropertiesImpl = (t) => t.getApparentProperties();
export const asInterfaceTypeImpl = (t) => {
    if (t.objectFlags & ts.ObjectFlags.Interface) {
        return t;
    }
    return null;
};
export const asClassTypeImpl = (t) => {
    if (t.objectFlags & ts.ObjectFlags.Class) {
        return t;
    }
    return null;
};
export const getSymbolImpl = (t) => t.getSymbol() || null;
export const getDefaultImpl = (t) => t.getDefault() || null;
export const getBaseTypesImpl = (t) => t.getBaseTypes() || [];
//# sourceMappingURL=Typs.js.map