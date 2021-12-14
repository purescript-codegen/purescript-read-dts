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
exports._readTypes = void 0;
var ts = __importStar(require("typescript"));
var formatHost = {
    getCanonicalFileName: function (path) { return path; },
    getCurrentDirectory: ts.sys.getCurrentDirectory,
    getNewLine: function () { return ts.sys.newLine; }
};
var log = function (msg) { console.log(msg); };
function _readTypes(_a) {
    var options = _a.options, visit = _a.visit, rootNames = _a.rootNames, inMemoryFiles = _a.inMemoryFiles, compilerHost = _a.compilerHost, either = _a.either;
    log = options.debug ? log : (function (msg) { msg; });
    var program = createProgram(rootNames, inMemoryFiles, compilerHost);
    var checker = program.getTypeChecker();
    var onDeclaration = visit.onDeclaration;
    var onTypeNode = visit.onTypeNode;
    var declarations = [];
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
    var _loop_1 = function (sourceFile) {
        if (!(rootNames.some(function (n) { return sourceFile.fileName === n; })))
            return "continue";
        if (!options.compile && sourceFile !== undefined) {
            var x = program.getSyntacticDiagnostics(sourceFile);
            var errors_2 = [];
            x.forEach(function (d) {
                if (d.category === ts.DiagnosticCategory.Error) {
                    errors_2.push(ts.formatDiagnostic(d, formatHost));
                }
            });
            if (errors_2.length > 0) {
                return { value: either.left(errors_2) };
            }
        }
        try {
            ts.forEachChild(sourceFile, function (d) {
                if (isNodeExported(checker, d))
                    declarations.push(visitDeclaration(d));
            });
        }
        catch (e) {
            return { value: either.left(e) };
        }
    };
    for (var _i = 0, _b = program.getSourceFiles(); _i < _b.length; _i++) {
        var sourceFile = _b[_i];
        var state_1 = _loop_1(sourceFile);
        if (typeof state_1 === "object")
            return state_1.value;
    }
    return either.right({
        topLevel: declarations,
        readDeclaration: function (v) { return function () { return visitDeclaration(v); }; }
    });
    function processTypeParameters(typeParameters) {
        return (!typeParameters) ? [] : typeParameters.map(function (p) {
            var d = p.default ? getTSType(checker.getTypeAtLocation(p.default)) : null;
            return { name: p.name.escapedText, default: d };
        });
    }
    ;
    function visitDeclaration(node) {
        var t, fqn;
        var typeNode = checker.getTypeAtLocation(node);
        if (ts.isInterfaceDeclaration(node)) {
            var props = typeNode.getProperties().map(function (sym) { return property(sym, node); });
            var body = onTypeNode.interface(props);
            var params = processTypeParameters(node.typeParameters);
            t = onTypeNode.parametric({ body: body, params: params });
            fqn = checker.getFullyQualifiedName(typeNode.symbol);
            // } else if(ts.isClassDeclaration(node) && node.name) {
            //   let props = typeNode.getProperties().map((sym: ts.Symbol) => property(sym, node));
            //   let body = onTypeNode.class(props);
            //   let params = processTypeParameters(node.typeParameters)
            //   t = onTypeNode.parametric({ body, params })
            //   fqn = checker.getFullyQualifiedName(typeNode.symbol);
        }
        else if (ts.isTypeAliasDeclaration(node)) {
            // let AllMeanings = ts.SymbolFlags.Value | ts.SymbolFlags.Type | ts.SymbolFlags.Namespace | ts.SymbolFlags.Alias;
            // let node2:number = checker.getTypeAtLocation(node);
            // TODO: would this work in the case of external modules?
            var symbol = checker.getSymbolAtLocation(node.name);
            log("Alias symbol:");
            log(symbol);
            // log("Alias symbol is undefined:");
            // log(symbol == undefined);
            // log("typeNode:");
            // log(typeNode);
            // log("fqn of symbol:")
            // log(symbol?checker.getFullyQualifiedName(symbol):"WTF");//node.name.text;
            // log("fqn of exported symbol:")
            // log(symbol?checker.getFullyQualifiedName(checker.getExportSymbolOfSymbol(symbol)):"WTF");//node.name.text;
            t = getTSType(typeNode);
            fqn = symbol ? checker.getFullyQualifiedName(checker.getExportSymbolOfSymbol(symbol)) : "WTF"; //node.name.text;
            // } else if(ts.isModuleDeclaration(node)) {
            //   log("Module declaration found:" + node.name);
            //   // let moduleType = checker.getTypeAtLocation(node.name)
            //   let declarations:d[] = [];
            //   // let m = checker.getSymbolAtLocation(moduleType);
            //   console.log("Iterating module: " + node.name)
            //   ts.forEachChild(node, function(d){
            //     if(ts.isModuleBlock(d)) {
            //       d.statements.forEach(function(s) {
            //         // XXX: isNodeExported fails in case of ambient modules - why?
            //         // if (isNodeExported(checker, d)) {
            //         declarations.push(visitDeclaration(s));
            //       });
            //     }
            //   })
            //  let symbol = node.name?checker.getSymbolAtLocation(node.name):undefined;
            //  let fqn = symbol?checker.getFullyQualifiedName(symbol):undefined;
            //  if(fqn) {
            //    return onDeclaration.module_({
            //        fqn,
            //        declarations
            //    });
            //  }
            // }
            //} else if(ts.isMethodDeclaration(node)) {
            //  let signature = checker.getSignatureFromDeclaration(node);
        }
        if (t && fqn) {
            return onDeclaration(fqn)(t);
        }
        if (typeNode) {
            try {
                fqn = checker.getFullyQualifiedName(typeNode.symbol);
            }
            catch (e) {
                throw (["Unable to resolve node: " + fqn]);
            }
        }
        throw (["Unable to resolve node: " + node]);
    }
    function property(sym, dec) {
        var optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
        var memType = dec ? checker.getTypeOfSymbolAtLocation(sym, dec) : checker.getDeclaredTypeOfSymbol(sym);
        var t = getTSType(memType, true);
        return { name: sym.name, type: t, optional: optional };
    }
    function getMemberTSType(memType) {
        return getTSType(memType, true);
    }
    function getTSType(memType, member) {
        var _a;
        if (member === void 0) { member = false; }
        if (memType.isLiteral()) {
            memType.isStringLiteral();
        }
        if (memType.aliasSymbol && member) {
            var s = memType.aliasSymbol;
            var fqn = checker.getFullyQualifiedName(s);
            var params = ((_a = memType.aliasTypeArguments) === null || _a === void 0 ? void 0 : _a.map(getMemberTSType)) || [];
            var ref = (s && s.valueDeclaration) ? s.valueDeclaration : (s && s.declarations && s.declarations.length === 1) ? s.declarations[0] : null;
            if (ref) {
                var t = onTypeNode.ref({ fqn: fqn, ref: ref });
                if (params) {
                    t = onTypeNode.application({ params: params, t: t });
                }
                return t;
            }
        }
        else if (memType.isStringLiteral()) {
            return onTypeNode.stringLiteral(memType.value);
        }
        else if (memType.isNumberLiteral()) {
            return onTypeNode.numberLiteral(memType.value);
        }
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
        else if (memType.isUnion()) {
            var types = memType.types.map(function (t) {
                return getMemberTSType(t);
            });
            return onTypeNode.union(types);
        }
        else if (memType.isIntersection()) {
            var types = memType.types.map(getMemberTSType);
            return onTypeNode.intersection(types);
        }
        else if (memType.flags & ts.TypeFlags.Any) {
            return onTypeNode.any;
            // } else if (memType.flags & (ts.TypeFlags.String
            //         | ts.TypeFlags.BooleanLike | ts.TypeFlags.Number
            //         | ts.TypeFlags.Null | ts.TypeFlags.VoidLike | ts.TypeFlags.Any)) {
            // // XXX: I haven't found any other way to access
            // // BooleanLiteral value...
            //   return onTypeNode.primitive(checker.typeToString(memType));
        }
        else if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
            var memObjectType = memType;
            // let onInterfaceReference = function(target: ts.InterfaceType, typeArguments: t[]) {
            //   let ref = (target.symbol && target.symbol.valueDeclaration)
            //       ?target.symbol.valueDeclaration
            //       :(target.symbol && target.symbol.declarations.length === 1)
            //         ?target.symbol.declarations[0]
            //         :null;
            //   let fqn = checker.getFullyQualifiedName(target.symbol);
            //   return ref
            //     ?onTypeNode.typeReference({typeArguments, fqn, ref})
            //     :onTypeNode.unknown("Unable to get type declaration for:" + fqn + "<" + typeArguments + ">")
            // }
            // if(memObjectType.objectFlags & ts.ObjectFlags.Reference) {
            //   let reference = <ts.TypeReference>memObjectType;
            //  if(checker.isArrayType(reference)) {
            //    log("Trying array")
            //    let elem = checker.getElementTypeOfArrayType(reference);
            //    if(elem)
            //      return onTypeNode.array(getMemberTSType(elem));
            //   }
            //   if(checker.isTupleType(reference)) {
            //     log("Tuple")
            //     let e: string, elem:ts.Type | undefined, elems:t[] = [];
            //     let MAX_TUPLE_LENGTH = 20;
            //     for(let i=0; i<MAX_TUPLE_LENGTH; i++) {
            //       // Hack source:
            //       // https://github.com/microsoft/TypeScript/blob/v3.6.3/src/compiler/checker.ts + getTupleElementType
            //       e = "" + i as string;
            //       elem = checker.getTypeOfPropertyOfType(reference, e);
            //       if(elem) {
            //         elems.push(getMemberTSType(elem));
            //       } else {
            //         break;
            //       }
            //     };
            //     return onTypeNode.tuple(elems);
            //   }
            //   if (reference.target.isClassOrInterface()) {
            //     log("Interface")
            //     let typeArguments = reference.typeArguments?reference.typeArguments.map(getMemberTSType):[];
            //     return onInterfaceReference(reference.target, typeArguments);
            //   }
            // } else if(memObjectType.isClassOrInterface()) {
            //   log("Interface 2?")
            //   return onInterfaceReference(memObjectType, []);
            // // This __seems__ to work in the case of Pick<..> and Record<..>
            // } else if((memObjectType.objectFlags & ts.ObjectFlags.Mapped) &&
            //    (memObjectType.objectFlags & ts.ObjectFlags.Instantiated)) {
            //   let objDeclarations = memObjectType.symbol.getDeclarations();
            //   let props = memObjectType.getProperties().map((sym: ts.Symbol) =>
            //     property(sym, objDeclarations?objDeclarations[0]:sym.declarations?sym.declarations[1]:sym.valueDeclaration)
            //   );
            //   return onTypeNode.object(props);
            // }
            // } else
            if (memObjectType.objectFlags & ts.ObjectFlags.Anonymous) {
                //     log("Anonymous")
                //     // TODO: Currently any object which is "callable" is interpreted
                //     // as a plain function
                //     let signature = memObjectType.getCallSignatures()[0];
                //     if(signature) {
                //       log("Treating this as function: " + memObjectType.symbol.getName());
                //       let functionType = {
                //         parameters: signature.parameters.map((parameterSymbol) => {
                //           return {
                //             name: parameterSymbol.getName(),
                //             type: getMemberTSType(
                //               checker.getTypeOfSymbolAtLocation(parameterSymbol, parameterSymbol?.valueDeclaration),
                //             )
                //           };
                //         }),
                //         returnType: getMemberTSType(signature.getReturnType())
                //       };
                //       log("Returning funciton type:" + functionType);
                //       return onTypeNode.function(functionType);
                //     }
                var props = memObjectType.getProperties().map(function (sym) { return property(sym, sym.valueDeclaration); });
                return onTypeNode.object(props);
            }
            //   return onTypeNode.unknown("Uknown object type node (flags = " + memObjectType.objectFlags + "):" + checker.typeToString(memObjectType));
            // }
            // else if (memType.isTypeParameter()) {
            //   log("Type parameter?")
            //   let d = memType.getDefault();
            //   return onTypeNode.typeParameter({ name: memType.symbol.escapedName, default: d?getTSType(d):null });
        }
        throw (["Unable to process type: " + checker.typeToString(memType)]);
    }
}
exports._readTypes = _readTypes;
// https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API#using-the-type-checker
function isNodeExported(checker, node) {
    var sym = checker.getSymbolAtLocation(node);
    return (sym ? ((ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0) : false ||
        (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile && node.kind !== ts.SyntaxKind.EndOfFileToken));
}
;
// https://stackoverflow.com/questions/53733138/how-do-i-type-check-a-snippet-of-typescript-code-in-memory
function createProgram(rootNames, inMemoryFiles, realHost) {
    var host = realHost;
    if (inMemoryFiles) {
        var paths_1 = inMemoryFiles.map(function (m) { return m.path; });
        var sourceFiles_1 = inMemoryFiles.map(function (m) { return { module: m, file: ts.createSourceFile(m.path, m.source, ts.ScriptTarget.ES5, true) }; });
        host = {
            fileExists: function (filePath) { return paths_1.some(function (p) { return filePath == p; }) || realHost.fileExists(filePath); },
            directoryExists: realHost.directoryExists && realHost.directoryExists.bind(realHost),
            getCurrentDirectory: realHost.getCurrentDirectory.bind(realHost),
            getDirectories: realHost.getDirectories ? realHost.getDirectories.bind(realHost) : undefined,
            getCanonicalFileName: function (fileName) { return realHost.getCanonicalFileName(fileName); },
            getNewLine: realHost.getNewLine.bind(realHost),
            getDefaultLibFileName: realHost.getDefaultLibFileName.bind(realHost),
            getSourceFile: function (fileName, languageVersion, onError, shouldCreateNewSourceFile) {
                var m = sourceFiles_1.find(function (f) { return f.module.path == fileName; });
                return m ? m.file : realHost.getSourceFile(fileName, languageVersion, onError, shouldCreateNewSourceFile);
            },
            readFile: function (fileName) {
                var f = sourceFiles_1.find(function (f) { return f.module.path == fileName; });
                return f ? f.module.source : realHost.readFile(fileName);
            },
            useCaseSensitiveFileNames: function () { return realHost.useCaseSensitiveFileNames(); },
            writeFile: function (_, data) { data; },
        };
    }
    var options = {};
    return ts.createProgram(rootNames, options, host);
}
//# sourceMappingURL=ReadDTS.js.map