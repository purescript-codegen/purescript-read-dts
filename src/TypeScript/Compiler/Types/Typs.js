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
exports.asTypeReferenceImpl = void 0;
var ts = __importStar(require("typescript"));
var asTypeReferenceImpl = function (t) {
    // There is no sens at the moment to expose ObjectType casting I think...
    var isObjectType = function (t) {
        var Nullable = ts.TypeFlags.Undefined | ts.TypeFlags.Null;
        var ObjectFlagsType = ts.TypeFlags.Any | Nullable | ts.TypeFlags.Never | ts.TypeFlags.Object | ts.TypeFlags.Union | ts.TypeFlags.Intersection;
        return !!(t.flags & ObjectFlagsType);
    };
    if (isObjectType(t) && !!(t.objectFlags & ts.ObjectFlags.Reference))
        return t;
    return null;
};
exports.asTypeReferenceImpl = asTypeReferenceImpl;
//# sourceMappingURL=Typs.js.map