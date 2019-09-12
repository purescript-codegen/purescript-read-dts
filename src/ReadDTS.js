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
exports.eqIdentifierImpl = function (i1) {
    return function (i2) {
        return i1 === i2;
    };
};
exports.compilerOptions = {
    target: ts.ScriptTarget.ES5,
    module: ts.ModuleKind.CommonJS
};
function _readDTS(options, visit, fileName) {
    var program = ts.createProgram([fileName], options);
    var checker = program.getTypeChecker();
    var onDeclaration = visit.onDeclaration;
    var onType = visit.onType;
    var result = [];
    // Check only given declaration file
    for (var _i = 0, _a = program.getSourceFiles(); _i < _a.length; _i++) {
        var sourceFile = _a[_i];
        if (sourceFile.isDeclarationFile && sourceFile.fileName === fileName) {
            ts.forEachChild(sourceFile, function (declaration) {
                if (isNodeExported(declaration))
                    result.push(visitDeclaration(declaration));
            });
        }
    }
    return result;
    function visitDeclaration(node) {
        var processTypeParameters = function (typeParameters) {
            return (!typeParameters) ? [] : typeParameters.map(function (p) {
                var d = p.default ? getTSType(checker.getTypeAtLocation(p.default)) : null;
                return onType.typeParameter({ identifier: p.name.escapedText, default: d });
            });
        };
        if (ts.isInterfaceDeclaration(node)) {
            var nodeType_1 = checker.getTypeAtLocation(node);
            var members = nodeType_1.getProperties().map(function (sym) {
                var optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
                var memType = checker.getTypeOfSymbolAtLocation(sym, node);
                var t = getTSType(memType);
                return { name: sym.name, type: t, optional: optional };
            });
            var fullyQualifiedName_1 = checker.getFullyQualifiedName(nodeType_1.symbol);
            var i = {
                name: node.name.text,
                fullyQualifiedName: fullyQualifiedName_1,
                members: members,
                typeParameters: processTypeParameters(node.typeParameters)
            };
            return onDeclaration.interface(i);
        }
        else if (ts.isTypeAliasDeclaration(node)) {
            var nodeType_2 = checker.getTypeAtLocation(node);
            var x = {
                name: node.name.text,
                type: getTSType(nodeType_2),
                typeParameters: processTypeParameters(node.typeParameters)
            };
            return onDeclaration.typeAlias(x);
        }
        var nodeType = checker.getTypeAtLocation(node);
        var fullyQualifiedName = null;
        try {
            fullyQualifiedName = checker.getFullyQualifiedName(nodeType.symbol);
        }
        catch (e) {
        }
        return onDeclaration.unknown({ fullyQualifiedName: fullyQualifiedName, msg: "Unknown declaration node" });
    }
    function getTSType(memType) {
        if (memType.flags & (ts.TypeFlags.String
            | ts.TypeFlags.BooleanLike | ts.TypeFlags.Number
            | ts.TypeFlags.Null | ts.TypeFlags.VoidLike | ts.TypeFlags.Any)) {
            return onType.primitive(checker.typeToString(memType));
        }
        else if (memType.isUnion()) {
            var types = memType.types.map(getTSType);
            return onType.union(types);
        }
        else if (memType.isIntersection()) {
            var types = memType.types.map(getTSType);
            return onType.intersection(types);
        }
        else if (memType.isStringLiteral()) {
            return onType.stringLiteral(memType.value);
        }
        else if (memType.isNumberLiteral()) {
            return onType.numberLiteral(memType.value);
        }
        else if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
            var memObjectType = memType;
            var onInterfaceReference = function (target, typeArguments) {
                return onType.typeReference({
                    typeArguments: typeArguments,
                    fullyQualifiedName: checker.getFullyQualifiedName(target.symbol),
                    read: function () {
                        // XXX: This is for sure stupid strategy to access external interfaces.
                        var r = null;
                        if (target.symbol) {
                            if (target.symbol.valueDeclaration) {
                                r = visitDeclaration(target.symbol.valueDeclaration);
                            }
                            // XXX: I'm not sure why have to use declarations here...
                            //      For sure we should introduce proper error handling.
                            if (target.symbol.declarations.length === 1) {
                                r = visitDeclaration(target.symbol.declarations[0]);
                            }
                        }
                        return (r ? r : visit.onDeclaration.unknown({
                            fullyQualifiedName: this.fullyQualifiedName,
                            msg: "Unable to extract declaration"
                        }));
                    }
                });
            };
            if (memObjectType.objectFlags & ts.ObjectFlags.Reference) {
                var reference = memObjectType;
                if (reference.target.isClassOrInterface()) {
                    var typeArguments = reference.typeArguments ? reference.typeArguments.map(getTSType) : [];
                    return onInterfaceReference(reference.target, typeArguments);
                }
            }
            else if (memObjectType.isClassOrInterface()) {
                return onInterfaceReference(memObjectType, []);
            }
        }
        // I'm not sure why this check turns memType type into `never`
        else if (memType.isTypeParameter()) {
            var d = memType.getDefault();
            return onType.typeParameter({ identifier: memType.symbol.escapedName, default: d ? getTSType(d) : null });
        }
        return onType.unknown(checker.typeToString(memType));
    }
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