import * as ts from "typescript";

/* Caution: @internal .... */
export interface IntrinsicType extends ts.Type {
    intrinsicName: string;        // Name of intrinsic type
    // objectFlags: ObjectFlags;
}

export const asIntrinsicTypeImpl = (t: ts.Type): IntrinsicType | null => {
  const isIntrinsicType = (t: ts.Type): t is IntrinsicType => {
    // We use this hacky way to detect intrinsic type because
    // we want to avoid copying of the `Intrinsic`
    // `TypeFlags.Intrinsic` union which can change over time.
    return (t as IntrinsicType).intrinsicName !== undefined;
  }
  return isIntrinsicType(t)?t:null;
}

export const reflectBooleanLiteralTypeImpl = (t: ts.Type): boolean | null => {
  const isBooleanLiteral = (t: ts.Type): t is IntrinsicType => {
      return !!(t.flags & ts.TypeFlags.BooleanLiteral);
  }
  return isBooleanLiteral(t)?t.intrinsicName == "true":null;
}

