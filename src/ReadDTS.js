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
    if (mod != null) for (var k in mod) if (k !== "default" && Object.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports._readDTS = void 0;
var ts = __importStar(require("typescript"));
exports.eqIdentifierImpl = function (i1) {
    return function (i2) {
        return i1 === i2;
    };
};
var formatHost = {
    getCanonicalFileName: function (path) { return path; },
    getCurrentDirectory: ts.sys.getCurrentDirectory,
    getNewLine: function () { return ts.sys.newLine; }
};
function _readDTS(options, visit, file, either) {
    var sourceFile = undefined;
    var compilerOptions = {
        target: ts.ScriptTarget.ES5,
        module: ts.ModuleKind.CommonJS,
        strictNullChecks: options.strictNullChecks
    };
    var program = createProgram(file, compilerOptions);
    var checker = program.getTypeChecker();
    var onDeclaration = visit.onDeclaration;
    var onTypeNode = visit.onTypeNode;
    var declarations = [];
    var log = options.debug ? function (msg) { console.log(msg); } : function () { };
    if (options.compile) {
        var emitResult = program.emit();
        if (emitResult.emitSkipped) {
            var allDiagnostics = ts.getPreEmitDiagnostics(program).concat(emitResult.diagnostics);
            var errors_1 = [];
            allDiagnostics.forEach(function (d) {
                if (d.category === ts.DiagnosticCategory.Error) {
                    errors_1.push(ts.formatDiagnostic(d, formatHost));
                }
            });
            if (errors_1.length > 0) {
                return either.left(errors_1);
            }
        }
    }
    for (var _i = 0, _a = program.getSourceFiles(); _i < _a.length; _i++) {
        var sf = _a[_i];
        if (sf.isDeclarationFile && sf.fileName === file.path) {
            sourceFile = sf;
        }
    }
    if (sourceFile !== undefined) {
        if (!options.compile && sourceFile !== undefined) {
            var x = program.getSyntacticDiagnostics(sourceFile);
            var errors_2 = [];
            x.forEach(function (d) {
                if (d.category === ts.DiagnosticCategory.Error) {
                    errors_2.push(ts.formatDiagnostic(d, formatHost));
                }
            });
            if (errors_2.length > 0) {
                return either.left(errors_2);
            }
        }
        log("Starting iteration");
        ts.forEachChild(sourceFile, function (d) {
            log("Another declaration");
            if (isNodeExported(checker, d))
                declarations.push(visitDeclaration(d));
        });
        log("Ending iteration");
    }
    else {
        return either.left(["Source file not found"]);
    }
    return either.right({
        topLevel: declarations,
        readDeclaration: function (v) { return function () { return visitDeclaration(v); }; }
    });
    function property(sym, dec) {
        var optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
        var memType = dec ? checker.getTypeOfSymbolAtLocation(sym, dec) : checker.getDeclaredTypeOfSymbol(sym);
        log("PROPERTY" + sym.name);
        var t = getTSType(memType);
        return { name: sym.name, type: t, optional: optional };
    }
    function visitDeclaration(node) {
        var processTypeParameters = function (typeParameters) {
            return (!typeParameters) ? [] : typeParameters.map(function (p) {
                var d = p.default ? getTSType(checker.getTypeAtLocation(p.default)) : null;
                return { name: p.name.escapedText, default: d };
            });
        };
        var symbol = node.name ? checker.getSymbolAtLocation(node.name) : undefined;
        if (symbol) {
            var fullyQualifiedName_1 = checker.getFullyQualifiedName(symbol);
            var nodeType_1 = checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration);
            if (ts.isInterfaceDeclaration(node)) {
                var typeSignatures = checker.getSignaturesOfType(nodeType_1, ts.SignatureKind.Call);
                var s = typeSignatures[0];
                if (s) {
                    console.log(s.typeParameters);
                    checker.getReturnTypeOfSignature(s);
                }
                else {
                    console.log("EMPTY signature");
                }
                var properties = nodeType_1.getProperties().map(function (sym) { return property(sym, node); });
                var i = {
                    name: symbol.getName(),
                    fullyQualifiedName: fullyQualifiedName_1,
                    properties: properties,
                    typeParameters: processTypeParameters(node.typeParameters)
                };
                return onDeclaration.interface(i);
            }
            else if (ts.isClassDeclaration(node)) {
                // let properties = nodeType.getProperties().map((sym: ts.Symbol) => property(sym, node));
                var properties = checker.getPropertiesOfType(nodeType_1).map(function (sym) { return property(sym, node); });
                var i = {
                    // TODO: Extract class name
                    name: nodeType_1.symbol.getName(),
                    fullyQualifiedName: fullyQualifiedName_1,
                    properties: properties,
                    typeParameters: processTypeParameters(node.typeParameters)
                };
                return onDeclaration.class_(i);
            }
            else if (ts.isTypeAliasDeclaration(node)) {
                log("TYPE ALIAS");
                var nodeType_2 = checker.getTypeAtLocation(node);
                var x = {
                    name: node.name.text,
                    type: getTSType(nodeType_2),
                    typeParameters: processTypeParameters(node.typeParameters)
                };
                return onDeclaration.typeAlias(x);
                //} else if(ts.isMethodDeclaration(node)) {
                //  // let signature = checker.getSignatureFromDeclaration(node);
                //  log("METHOD Declaration")
                //  log(node.name.toString())
                // } else if(ts.isMethodSignature(node)) {
                //  // let signature = checker.getSignatureFromDeclaration(node);
                //  log("METHOD SIGNATURE")
                //  log(node.name.toString())
                // } else if (ts.isFunctionDeclaration(node)) {
                //   log("Function Declaration - commented out. I'm not sure if I should handle it as typeAlias?")
                //   // let functionType = checker.getTypeAtLocation(node)
                //   // let signature = checker.getSignatureFromDeclaration(node);
                //   // if(signature) {
                //     // return onDeclaration.function({
                //       // fullyQualifiedName: checker.getFullyQualifiedName(functionType.symbol),
                //       // ...functionSignature(signature)
                //     // })
                //   // }
            }
            else if (ts.isModuleDeclaration(node)) {
                log("Module declaration found:" + node.name);
                // let moduleType = checker.getTypeAtLocation(node.name)
                var declarations_1 = [];
                // let m = checker.getSymbolAtLocation(moduleType);
                console.log("Iterating module: " + node.name);
                ts.forEachChild(node, function (d) {
                    if (ts.isModuleBlock(d)) {
                        d.statements.forEach(function (s) {
                            // XXX: isNodeExported fails in case of ambient modules - why?
                            // if (isNodeExported(checker, d)) {
                            console.log("");
                            declarations_1.push(visitDeclaration(s));
                        });
                    }
                });
                return onDeclaration.module_({
                    fullyQualifiedName: fullyQualifiedName_1,
                    declarations: declarations_1
                });
            }
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
        // Because we are processing only typelevel
        // declarations we can be sure that
        // these literals are type level entities.
        if (memType.isStringLiteral()) {
            return onTypeNode.stringLiteral(memType.value);
        }
        else if (memType.isNumberLiteral()) {
            return onTypeNode.numberLiteral(memType.value);
        }
        // XXX: I haven't found any other way to access
        // BooleanLiteral value...
        else if ((memType.flags & ts.TypeFlags.BooleanLiteral) &&
            (memType.intrinsicName == "true" ||
                memType.intrinsicName == "false")) {
            if (memType.intrinsicName == "true") {
                return onTypeNode.booleanLiteral(true);
            }
            else {
                return onTypeNode.booleanLiteral(false);
            }
        }
        else if (memType.flags & (ts.TypeFlags.String
            | ts.TypeFlags.BooleanLike | ts.TypeFlags.Number
            | ts.TypeFlags.Null | ts.TypeFlags.VoidLike | ts.TypeFlags.Any)) {
            return onTypeNode.primitive(checker.typeToString(memType));
        }
        else if (memType.isUnion()) {
            var types = memType.types.map(getTSType);
            return onTypeNode.union(types);
        }
        else if (memType.isIntersection()) {
            var types = memType.types.map(getTSType);
            return onTypeNode.intersection(types);
        }
        else if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
            log("Possible object / non primitive type");
            var memObjectType = memType;
            var onInterfaceReference = function (target, typeArguments) {
                var ref = (target.symbol && target.symbol.valueDeclaration)
                    ? target.symbol.valueDeclaration
                    : (target.symbol && target.symbol.declarations.length === 1)
                        ? target.symbol.declarations[0]
                        : null;
                var fullyQualifiedName = checker.getFullyQualifiedName(target.symbol);
                return ref
                    ? onTypeNode.typeReference({ typeArguments: typeArguments, fullyQualifiedName: fullyQualifiedName, ref: ref })
                    : onTypeNode.unknown("Unable to get type declaration for:" + fullyQualifiedName + "<" + typeArguments + ">");
            };
            if (memObjectType.objectFlags & ts.ObjectFlags.Reference) {
                log("REFERENCE");
                var reference = memObjectType;
                if (checker.isArrayType(reference)) {
                    var elem = checker.getElementTypeOfArrayType(reference);
                    if (elem)
                        return onTypeNode.array(getTSType(elem));
                }
                if (checker.isTupleType(reference)) {
                    var e = void 0, elem = void 0, elems = [];
                    for (var i = 0;; i++) {
                        // Hack source:
                        // https://github.com/microsoft/TypeScript/blob/v3.6.3/src/compiler/checker.ts + getTupleElementType
                        e = "" + i;
                        elem = checker.getTypeOfPropertyOfType(reference, e);
                        if (elem) {
                            elems.push(getTSType(elem));
                        }
                        else {
                            break;
                        }
                    }
                    ;
                    return onTypeNode.tuple(elems);
                }
                if (reference.target.isClassOrInterface()) {
                    var typeArguments = reference.typeArguments ? reference.typeArguments.map(getTSType) : [];
                    return onInterfaceReference(reference.target, typeArguments);
                }
            }
            if (memObjectType.isClassOrInterface()) {
                return onInterfaceReference(memObjectType, []);
            }
            // This __seems__ to work in case of Pick<..> and Record<..>
            if ((memObjectType.objectFlags & ts.ObjectFlags.Mapped) &&
                (memObjectType.objectFlags & ts.ObjectFlags.Instantiated)) {
                var objDeclarations_1 = memObjectType.symbol.getDeclarations();
                var props = memObjectType.getProperties().map(function (sym) {
                    return property(sym, objDeclarations_1 ? objDeclarations_1[0] : sym.declarations ? sym.declarations[1] : sym.valueDeclaration);
                });
                var fullyQualifiedName = checker.getFullyQualifiedName(memObjectType.symbol);
                return onTypeNode.anonymousObject({ properties: props, fullyQualifiedName: fullyQualifiedName });
            }
            if (memObjectType.objectFlags & ts.ObjectFlags.Anonymous) {
                // TODO: Currently any object which is "callable" is interpreted
                // as a plain function
                var signature = memObjectType.getCallSignatures()[0];
                if (signature) {
                    log("Treating this as function: " + memObjectType.symbol.getName());
                    var functionType = {
                        parameters: signature.parameters.map(function (parameterSymbol) {
                            var _a;
                            return {
                                name: parameterSymbol.getName(),
                                type: getTSType(checker.getTypeOfSymbolAtLocation(parameterSymbol, (_a = parameterSymbol) === null || _a === void 0 ? void 0 : _a.valueDeclaration))
                            };
                        }),
                        returnType: getTSType(signature.getReturnType())
                    };
                    log("Returning funciton type:" + functionType);
                    return onTypeNode.function(functionType);
                }
                var props = memObjectType.getProperties().map(function (sym) { return property(sym, sym.valueDeclaration); });
                var fullyQualifiedName = checker.getFullyQualifiedName(memObjectType.symbol);
                return onTypeNode.anonymousObject({ fullyQualifiedName: fullyQualifiedName, properties: props });
            }
            return onTypeNode.unknown("Uknown object type node (flags = " + memObjectType.objectFlags + "):" + checker.typeToString(memObjectType));
        }
        else if (memType.isTypeParameter()) {
            var d = memType.getDefault();
            return onTypeNode.typeParameter({ name: memType.symbol.escapedName, default: d ? getTSType(d) : null });
        }
        return onTypeNode.unknown(checker.typeToString(memType));
    }
}
exports._readDTS = _readDTS;
// https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API#using-the-type-checker
function isNodeExported(checker, node) {
    var sym = checker.getSymbolAtLocation(node);
    return (sym ? ((ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0) : false ||
        (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile && node.kind !== ts.SyntaxKind.EndOfFileToken));
}
;
// https://stackoverflow.com/questions/53733138/how-do-i-type-check-a-snippet-of-typescript-code-in-memory
function createProgram(file, options) {
    var realHost = ts.createCompilerHost(options, true);
    var host = realHost;
    if (file.source) {
        var sourceFile_1 = ts.createSourceFile(file.path, file.source, ts.ScriptTarget.ES5, true);
        host = {
            fileExists: function (filePath) { return filePath === file.path || realHost.fileExists(filePath); },
            directoryExists: realHost.directoryExists && realHost.directoryExists.bind(realHost),
            getCurrentDirectory: realHost.getCurrentDirectory.bind(realHost),
            getDirectories: realHost.getDirectories ? realHost.getDirectories.bind(realHost) : undefined,
            getCanonicalFileName: function (fileName) { return realHost.getCanonicalFileName(fileName); },
            getNewLine: realHost.getNewLine.bind(realHost),
            getDefaultLibFileName: realHost.getDefaultLibFileName.bind(realHost),
            getSourceFile: function (fileName, languageVersion, onError, shouldCreateNewSourceFile) { return fileName === file.path
                ? sourceFile_1
                : realHost.getSourceFile(fileName, languageVersion, onError, shouldCreateNewSourceFile); },
            readFile: function (filePath) { return filePath === file.path
                ? file.source ? file.source : undefined
                : realHost.readFile(filePath); },
            useCaseSensitiveFileNames: function () { return realHost.useCaseSensitiveFileNames(); },
            writeFile: function (_, data) { data; },
        };
    }
    return ts.createProgram([file.path], options, host);
}
//# sourceMappingURL=ReadDTS.js.map
