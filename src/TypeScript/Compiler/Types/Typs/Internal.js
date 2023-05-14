import ts from "typescript";
export const asIntrinsicTypeImpl = (t) => {
    const isIntrinsicType = (t) => {
        // We use this hacky way to detect intrinsic type because
        // we want to avoid copying of the `Intrinsic`
        // `TypeFlags.Intrinsic` union which can change over time.
        return t.intrinsicName !== undefined;
    };
    return isIntrinsicType(t) ? t : null;
};
export const reflectBooleanLiteralTypeImpl = (t) => {
    const isBooleanLiteral = (t) => {
        return !!(t.flags & ts.TypeFlags.BooleanLiteral);
    };
    return isBooleanLiteral(t) ? t.intrinsicName == "true" : null;
};
//# sourceMappingURL=Internal.js.map