import * as ts from "typescript";

const formatHost: ts.FormatDiagnosticsHost = {
  getCanonicalFileName: path => path,
  getCurrentDirectory: ts.sys.getCurrentDirectory,
  getNewLine: () => ts.sys.newLine
};

type Effect<a> = () => a;
type FQN = string;
type Nullable<a> = a | null;
type Result<d> = { topLevel: d[], readDeclaration: (v: ts.Declaration) => Effect<d> }

type Application<t> = { params: t[], t: t };
type Declaration = { fqn: FQN, ref: ts.Declaration };
type Function<t> = {
   params: { type: t, name: string }[],
  "return": t,
 }
type Param<t> = { name: ts.__String, default: Nullable<t> };
type Parametric<t> = { body: t, params: Param<t>[]};
type Prop<t> = { name: string, type: t, optional: boolean };
type Props<t> = Prop<t>[];

let log = function(msg: any) { console.log(msg); };

export function _readTypes<d, t, either>(
{ options, visit, rootNames, inMemoryFiles, compilerHost, either }: {
  options: {
    compile: boolean;
    debug: boolean;
  };
  visit: {
    onDeclaration:
     (fqn: FQN) => (t: t) => d;
    onTypeNode: {
      any: t;
      application: (a: Application<t>) => t;
      array: (e: t) => t;
      booleanLiteral: (v: boolean) => t;
      class: (ps: Props<t>) => t;
      function: (s: Function<t>) => t;
      interface: (ps: Props<t>) => t;
      intersection: (ts: t[]) => t;
      numberLiteral: (v: number) => t;
      object: (ps: Props<t>) => t;
      param: (tp: Param<t>) => t;
      parametric: (p: Parametric<t>) => t;
      primitive: (name: string) => t;
      ref: (d: Declaration) => t;
      stringLiteral: (value: string) => t;
      tuple: (types: t[]) => t;
      union: (members: t[]) => t;
      unknown: (err: string) => t;
    };
  };
  rootNames: string[];
  inMemoryFiles: { path: string; source: string; }[];
  compilerHost: ts.CompilerHost;
  either: {
    left: (err: String[]) => either;
    right: (result: Result<d>) => either;
  };
}): either {
  log = options.debug?log:(function(msg: any) { msg; });

  let program = createProgram(rootNames, inMemoryFiles, compilerHost);
  let checker = <MyChecker>program.getTypeChecker();
  let onDeclaration = visit.onDeclaration;
  let onTypeNode = visit.onTypeNode;
  let declarations:d[] = [];

  if(options.compile) {
    let emitResult = program.emit();
    if(emitResult.emitSkipped) {
      let allDiagnostics = ts.getPreEmitDiagnostics(program).concat(emitResult.diagnostics);
      let errors:any[] = [];
      allDiagnostics.forEach(function(d) {
        if(d.category === ts.DiagnosticCategory.Error) {
          errors.push(ts.formatDiagnostic(d, formatHost));
        }
      })
      if(errors.length > 0) {
        return either.left(errors);
      }
    }
  }
  for (const sourceFile of program.getSourceFiles()) {
    if (!(rootNames.some((n) => sourceFile.fileName === n)))
      continue;

    if(!options.compile && sourceFile !== undefined) {
      let x = program.getSyntacticDiagnostics(sourceFile);
      let errors:any[] = [];
      x.forEach(function(d) {
        if(d.category === ts.DiagnosticCategory.Error) {
          errors.push(ts.formatDiagnostic(d, formatHost));
        }
      })
      if(errors.length > 0) {
        return either.left(errors);
      }
    }
    try {
      ts.forEachChild(sourceFile, function(d) {
        // if (isNodeExported(check//App/appcler, d))
          declarations.push(visitDeclaration(d));
      });
    } catch(e) {
      return either.left(e);
    }
  }
  return either.right({
    topLevel: declarations,
    readDeclaration: (v:ts.Declaration) => () => visitDeclaration(v)
  })
  // It is probably better to use some internal checker machinery
  // than to use heuristics like `fqn == "Array"`
  interface MyChecker extends ts.TypeChecker {
    // Hack source:
    // https://github.com/microsoft/TypeScript/blob/v3.6.3/src/compiler/checker.ts
    // I've additionally restricted some signatures.
    getElementTypeOfArrayType: (type: ts.TypeReference) =>  ts.Type | undefined;
    isArrayType: (type: ts.TypeReference) => boolean;
    isTupleType: (type: ts.TypeReference) => boolean;
    isReadonlyArrayType: (type: ts.TypeReference) => boolean;
    getTypeOfPropertyOfType: (type: ts.TypeReference, propertyName: string) => ts.Type | undefined;
    getNullType: () => ts.Type,
    getUndefinedType: () => ts.Type,

    // Some other possible helpers
    // isTupleLikeType: (type: ts.Type) => boolean;
    // isArrayLikeType: (type: ts.Type) => boolean;
    // isEmptyArrayLiteralType: (type: ts.Type) => boolean;
    // isArrayOrTupleLikeType: (type: ts.Type) => boolean;
    // isNeitherUnitTypeNorNever: (type: ts.Type) => boolean;
    // isUnitType: (type: ts.Type) => boolean;
    // isLiteralType: (type: ts.Type) => boolean;
  }
  function processTypeParameters( typeParameters: ts.NodeArray<ts.TypeParameterDeclaration> | undefined): Param<t>[] {
      return (!typeParameters)?[]:typeParameters.map(function(p: ts.TypeParameterDeclaration) {
        let d = p.default?getTSType(checker.getTypeAtLocation(p.default)):null;
        return { name: p.name.escapedText, default: d };
      })
  };

  function visitDeclaration(node: ts.Node): d {
    let t, fqn;
    let typeNode = checker.getTypeAtLocation(node);

    if(ts.isInterfaceDeclaration(node)) {
      let props = typeNode.getProperties().map((sym: ts.Symbol) => property(sym, node));
      let body = onTypeNode.interface(props);

      let params = processTypeParameters(node.typeParameters)
      t = onTypeNode.parametric({ body, params })

      fqn = checker.getFullyQualifiedName(typeNode.symbol);
    // } else if(ts.isClassDeclaration(node) && node.name) {
    //   let props = typeNode.getProperties().map((sym: ts.Symbol) => property(sym, node));
    //   let body = onTypeNode.class(props);
    //   let params = processTypeParameters(node.typeParameters)
    //   t = onTypeNode.parametric({ body, params })
    //   fqn = checker.getFullyQualifiedName(typeNode.symbol);
    } else if (ts.isTypeAliasDeclaration(node)) {
      // let AllMeanings = ts.SymbolFlags.Value | ts.SymbolFlags.Type | ts.SymbolFlags.Namespace | ts.SymbolFlags.Alias;
      // let node2:number = checker.getTypeAtLocation(node);

      // TODO: would this work in the case of external modules?
      let symbol = checker.getSymbolAtLocation(node.name);

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
      fqn = symbol?checker.getFullyQualifiedName(checker.getExportSymbolOfSymbol(symbol)):"WTF";//node.name.text;

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
    if(t && fqn) {
      return onDeclaration(fqn)(t);
    }
    if(typeNode) {
      try {
        fqn = checker.getFullyQualifiedName(typeNode.symbol);
      } catch(e) {
        throw(["Unable to resolve node: " + fqn]);
      } 
    }
    throw(["Unable to resolve node: " + node]);
  }
  
  function property(sym: ts.Symbol, dec?: ts.Declaration): Prop<t> {
    let optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
    let memType = dec?checker.getTypeOfSymbolAtLocation(sym, dec):checker.getDeclaredTypeOfSymbol(sym);
    let t = getTSType(memType, true);
    return { name: sym.name, type: t, optional }
  }

  function getMemberTSType(memType: ts.Type): t {
    return getTSType(memType, true);
  }

  function getTSType(memType: ts.Type, member = false): t {
    if(memType.isLiteral()) {
      memType.isStringLiteral()
    }
    if(memType.aliasSymbol && member) {
      let s = memType.aliasSymbol;
      let fqn = checker.getFullyQualifiedName(s);
      let params = memType.aliasTypeArguments?.map(getMemberTSType) || [];
      let ref = (s && s.valueDeclaration)?s.valueDeclaration:(s && s.declarations && s.declarations.length === 1)?s.declarations[0]:null;
      if(ref) {
        let t = onTypeNode.ref({fqn, ref});
        if(params) {
          t = onTypeNode.application({params, t});
        }
        return t;
      }
    } else if(memType.isStringLiteral()) {
      return onTypeNode.stringLiteral(memType.value);
    } else if(memType.isNumberLiteral()) {
      return onTypeNode.numberLiteral(memType.value);
    } else if((memType.flags & ts.TypeFlags.BooleanLiteral) &&
            ((memType as any).intrinsicName == "true" ||
             (memType as any).intrinsicName == "false" )) {
      if((memType as any).intrinsicName == "true") {
          return onTypeNode.booleanLiteral(true);
      } else {
          return onTypeNode.booleanLiteral(false);
      }
    } else if (memType.isUnion()) {
      let types = memType.types.map((t) => {
        return getMemberTSType(t);
      });
      return onTypeNode.union(types);
    } else if (memType.isIntersection()) {
      let types = memType.types.map(getMemberTSType);
      return onTypeNode.intersection(types);
    } else if (memType.flags & ts.TypeFlags.Any) {
      return onTypeNode.any;
    // } else if (memType.flags & (ts.TypeFlags.String
    //         | ts.TypeFlags.BooleanLike | ts.TypeFlags.Number
    //         | ts.TypeFlags.Null | ts.TypeFlags.VoidLike | ts.TypeFlags.Any)) {
    // // XXX: I haven't found any other way to access
    // // BooleanLiteral value...
    //   return onTypeNode.primitive(checker.typeToString(memType));

    } else if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
      let memObjectType = <ts.ObjectType>memType;
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
     if(memObjectType.objectFlags & ts.ObjectFlags.Anonymous) {
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
        let props = memObjectType.getProperties().map((sym: ts.Symbol) => property(sym, sym.valueDeclaration));
        return onTypeNode.object(props);
      }
    //   return onTypeNode.unknown("Uknown object type node (flags = " + memObjectType.objectFlags + "):" + checker.typeToString(memObjectType));
    // }
    // else if (memType.isTypeParameter()) {
    //   log("Type parameter?")
    //   let d = memType.getDefault();
    //   return onTypeNode.typeParameter({ name: memType.symbol.escapedName, default: d?getTSType(d):null });
    }
    throw(["Unable to process type: " + checker.typeToString(memType)]);
  }
}

// https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API#using-the-type-checker
function isNodeExported(checker:ts.TypeChecker, node: ts.Node): boolean {
  let sym = checker.getSymbolAtLocation(node);
    return (
      sym? ((ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0):false ||
      (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile && node.kind !== ts.SyntaxKind.EndOfFileToken)
    )
};

// https://stackoverflow.com/questions/53733138/how-do-i-type-check-a-snippet-of-typescript-code-in-memory
function createProgram(rootNames: string[], inMemoryFiles: {path: string, source: string}[], realHost: ts.CompilerHost): ts.Program {
  let host = realHost;
  if(inMemoryFiles) {
    let paths = inMemoryFiles.map((m) => m.path);
    let sourceFiles = inMemoryFiles.map((m) => { return { module: m, file: ts.createSourceFile(m.path, m.source, ts.ScriptTarget.ES5, true)}});

    host = {
      fileExists: filePath => paths.some((p) => filePath == p) || realHost.fileExists(filePath),
      directoryExists: realHost.directoryExists && realHost.directoryExists.bind(realHost),
      getCurrentDirectory: realHost.getCurrentDirectory.bind(realHost),
      getDirectories: realHost.getDirectories?realHost.getDirectories.bind(realHost):undefined,
      getCanonicalFileName: fileName => realHost.getCanonicalFileName(fileName),
      getNewLine: realHost.getNewLine.bind(realHost),
      getDefaultLibFileName: realHost.getDefaultLibFileName.bind(realHost),
      getSourceFile: function(fileName, languageVersion, onError, shouldCreateNewSourceFile) {
        var m = sourceFiles.find((f) => f.module.path == fileName);
        return m?m.file:realHost.getSourceFile(fileName, languageVersion, onError, shouldCreateNewSourceFile);
      },
      readFile: function(fileName) {
        var f = sourceFiles.find((f) => f.module.path == fileName);
        return f?f.module.source:realHost.readFile(fileName);
      },
      useCaseSensitiveFileNames: () => realHost.useCaseSensitiveFileNames(),
      writeFile: (_, data) => { data },
    };
  }
  let options = {};
  return ts.createProgram(rootNames, options, host);
}
