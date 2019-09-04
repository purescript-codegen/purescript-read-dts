"use strict";
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
var ts = __importStar(require("typescript"));
exports.compilerOptions = {
    target: ts.ScriptTarget.ES5,
    module: ts.ModuleKind.CommonJS
};
function _readDTS(fileName, options, onType) {
    // Build a program using the set of root file names in fileNames
    var program = ts.createProgram([fileName], options);
    var checker = program.getTypeChecker();
    var output = [];
    // Visit every sourceFile in the program
    for (var _i = 0, _a = program.getSourceFiles(); _i < _a.length; _i++) {
        var sourceFile = _a[_i];
        if (sourceFile.fileName === fileName) {
            ts.forEachChild(sourceFile, visit);
        }
    }
    return output;
    function visit(node) {
        // Only consider exported nodes
        if (!isNodeExported(node)) {
            return;
        }
        if ((node.kind === ts.SyntaxKind.InterfaceDeclaration
            || node.kind === ts.SyntaxKind.TypeAliasDeclaration)
            && node.name) {
            // let symbol = checker.getSymbolAtLocation(node.name);
            // if (symbol) {
            var nodeType = checker.getTypeAtLocation(node);
            if (nodeType.isClassOrInterface()) {
                var members = nodeType.getProperties().map(function (sym) {
                    var optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
                    var memType = checker.getTypeOfSymbolAtLocation(sym, node);
                    return { name: sym.name, type: getTSType(memType), optional: optional };
                });
                var x = { name: node.name.text, members: members };
                // console.log(x);
                output.push(onType.interface(x));
            }
            else {
                var x = { name: node.name.text, type: getTSType(nodeType) };
                // console.log(x);
                output.push(onType.typeAlias(x));
            }
        }
        else if (node.kind === ts.SyntaxKind.ModuleDeclaration) {
            // This is a namespace, visit its children
            ts.forEachChild(node, visit);
        }
    }
    function getTSType(memType) {
        if (memType.isUnionOrIntersection()) {
            var types = memType.types.map(getTSType);
            return onType.unionOrIntersection(types);
        }
        else if (memType.flags & (ts.TypeFlags.String
            | ts.TypeFlags.BooleanLike | ts.TypeFlags.Number
            | ts.TypeFlags.Null | ts.TypeFlags.VoidLike | ts.TypeFlags.Any)) {
            return onType.primitive(checker.typeToString(memType));
        }
        else if (memType.isStringLiteral()) {
            return onType.stringLiteral(memType.value);
        }
        else if (memType.isNumberLiteral()) {
            return onType.numberLiteral(memType.value);
        }
        // TODO
        // -------------------------------------------------------------------------
        // var callSigs = memType.getCallSignatures();
        // if (callSigs.length > 0) {
        //   var sig = callSigs[0];
        //   var params = sig.getParameters().map(function (p) { return optionalMember(p); });
        //   return { type: "function", params: params, return: getWithAliasProps(sig.getReturnType()) };
        // }
        // if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
        //   var objFlags = memType.objectFlags;
        //   if (objFlags & ts.ObjectFlags.Reference) {
        //     var tr = memType;
        //     return {
        //       type: "typeReference", name: getFullyQualifiedName(memType.symbol),
        //       typeParams: tr.typeArguments ? tr.typeArguments.map(getWithAliasProps) : [],
        //       flags: memType.flags,
        //       objFlags: objFlags
        //     };
        //   }
        //   else {
        //     if (objFlags & ts.ObjectFlags.Anonymous) {
        //       return {
        //         type: "object",
        //         members: convertProperties(memType)
        //       };
        //     }
        //     if (objFlags & ts.ObjectFlags.Interface) {
        //       return { type: "interfaceReference", name: getFullyQualifiedName(memType.symbol) };
        //     }
        //     return { type: "unknownObject", flags: objFlags };
        //   }
        // }
        // else if (memType.flags & ts.TypeFlags.TypeParameter) {
        //   return { type: "typeparam", name: checker.typeToString(memType) };
        // }
        else {
            console.log("UNKONWN");
            console.log(memType);
            return onType.unknown(checker.typeToString(memType));
            // return { unknown: checker.typeToString(memType), flags: memType.flags };
        }
    }
    /** True if this is visible outside this file, false otherwise */
    function isNodeExported(node) {
        var sym = checker.getSymbolAtLocation(node);
        return (
        // (ts.getCombinedModifierFlags(node.) & ts.ModifierFlags.Export) !== 0 ||
        (sym ? ((ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0) : false) ||
            (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile));
    }
}
exports._readDTS = _readDTS;
//# sourceMappingURL=ReadDTS.js.map