"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getDefaultImpl = exports.getSymbolImpl = exports.asClassTypeImpl = exports.asInterfaceTypeImpl = exports.getPropertiesImpl = exports.getCallSignatures = exports.asTypeReferenceImpl = exports.asTypeParameterImpl = exports.asUnionTypeImpl = exports.asStringLiteralTypeImpl = exports.asObjectTypeImpl = exports.asIntersectionTypeImpl = exports.asNumberLiteralTypeImpl = void 0;
var ts = __importStar(require("typescript"));
var asNumberLiteralTypeImpl = function (t) { return t.isNumberLiteral() ? t : null; };
exports.asNumberLiteralTypeImpl = asNumberLiteralTypeImpl;
var asIntersectionTypeImpl = function (t) { return t.isIntersection() ? t : null; };
exports.asIntersectionTypeImpl = asIntersectionTypeImpl;
var asObjectTypeImpl = function (t) {
    var Nullable = ts.TypeFlags.Undefined | ts.TypeFlags.Null;
    var ObjectFlagsType = ts.TypeFlags.Any | Nullable | ts.TypeFlags.Never | ts.TypeFlags.Object | ts.TypeFlags.Union | ts.TypeFlags.Intersection;
    if (t.flags & ObjectFlagsType)
        return t;
    return null;
};
exports.asObjectTypeImpl = asObjectTypeImpl;
var asStringLiteralTypeImpl = function (t) { return t.isStringLiteral() ? t : null; };
exports.asStringLiteralTypeImpl = asStringLiteralTypeImpl;
var asUnionTypeImpl = function (t) { return t.isUnion() ? t : null; };
exports.asUnionTypeImpl = asUnionTypeImpl;
var asTypeParameterImpl = function (t) { return t.isTypeParameter() ? t : null; };
exports.asTypeParameterImpl = asTypeParameterImpl;
var asTypeReferenceImpl = function (t) {
    // There is no sens at the moment to expose ObjectType casting I think to the PS side...
    var s = (0, exports.asObjectTypeImpl)(t);
    if (s && s.objectFlags & ts.ObjectFlags.Reference)
        return s;
    return null;
};
exports.asTypeReferenceImpl = asTypeReferenceImpl;
var getCallSignatures = function (t) { return t.getCallSignatures(); };
exports.getCallSignatures = getCallSignatures;
var getPropertiesImpl = function (t) { return t.getProperties(); };
exports.getPropertiesImpl = getPropertiesImpl;
var asInterfaceTypeImpl = function (t) {
    if (t.objectFlags & ts.ObjectFlags.Interface) {
        return t;
    }
    return null;
};
exports.asInterfaceTypeImpl = asInterfaceTypeImpl;
var asClassTypeImpl = function (t) {
    if (t.objectFlags & ts.ObjectFlags.Class) {
        return t;
    }
    return null;
};
exports.asClassTypeImpl = asClassTypeImpl;
var getSymbolImpl = function (t) { return t.getSymbol() || null; };
exports.getSymbolImpl = getSymbolImpl;
var getDefaultImpl = function (t) { return t.getDefault() || null; };
exports.getDefaultImpl = getDefaultImpl;
//# sourceMappingURL=Typs.js.map