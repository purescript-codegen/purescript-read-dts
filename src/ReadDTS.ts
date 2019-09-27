import * as ts from "typescript";

exports.eqIdentifierImpl = function(i1: ts.Identifier) {
  return function(i2: ts.Identifier) {
    return i1 === i2;
  }
}

const formatHost: ts.FormatDiagnosticsHost = {
  getCanonicalFileName: path => path,
  getCurrentDirectory: ts.sys.getCurrentDirectory,
  getNewLine: () => ts.sys.newLine
};

type Effect<a> = () => a;
type Nullable<a> = a | null;
type TypeParameter<t> = { name: ts.__String, default: Nullable<t> };
type Property<t> = { name: string, type: t, optional: boolean }
type Result<d> = { topLevel: d[], readDeclaration: (v: ts.Declaration) => Effect<d> }

export function _readDTS<d, t, either>(
  options: { strictNullChecks: boolean },
  visit: {
    onDeclaration: {
      interface: (x:
        {
          name: string,
          fullyQualifiedName: string,
          properties: Property<t>[]
          typeParameters: TypeParameter<t>[]
        }) => d
      typeAlias: (x: { name: string, type: t, typeParameters: TypeParameter<t>[] }) => d
      unknown: (u: { fullyQualifiedName: Nullable<string>, msg: string }) => d
    },
    onTypeNode: {
      anonymousObject: (properties: ({ fullyQualifiedName: string, properties: Property<t>[] })) => t,
      array: (type: t) => t,
      intersection: (types: t[]) => t,
      primitive: (name: string) => t,
      tuple: (types: t[]) => t,
      typeParameter: (tp: TypeParameter<t>) => t,
      typeReference: (i: { typeArguments: t[], fullyQualifiedName: string, ref: Nullable<ts.Declaration> }) => t,
      booleanLiteral: (value: boolean) => t,
      numberLiteral: (value: number) => t,
      stringLiteral: (value: string) => t,
      union: (members: t[]) => t,
      unknown: (err: string) => t
    }
  },
  file: { path: string, source: Nullable<string> },
  either: {
    left: (err : String[]) => either,
    right: (result: Result<d>) => either
  }
): either {
  let sourceFile:ts.SourceFile | undefined = undefined;
  let compilerOptions:ts.CompilerOptions =  {
    target: ts.ScriptTarget.ES5,
    module: ts.ModuleKind.CommonJS,
    strictNullChecks: options.strictNullChecks
  };
  let program = createProgram(file, compilerOptions);
  let checker = <MyChecker>program.getTypeChecker();
  let onDeclaration = visit.onDeclaration;
  let onTypeNode = visit.onTypeNode;
  let declarations:d[] = [];

  for (const sf of program.getSourceFiles()) {
    if (sf.isDeclarationFile && sf.fileName === file.path) {
      sourceFile = sf;
    }
  }
  if(sourceFile !== undefined) {
    if(sourceFile !== undefined) {
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
    ts.forEachChild(sourceFile, function(d) {
      if (isNodeExported(checker, d))
        declarations.push(visitDeclaration(d));
    });
  } else {
    return either.left(["Source file not found"])
  }
  return either.right({
    topLevel: declarations,
    readDeclaration: (v:ts.Declaration) => () => visitDeclaration(v)
  })
  // It is probably better to use some internal checker machinery
  // than to use heuristics like `fullyQualifiedName == "Array"`
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

  interface MyNode extends ts.Node {
    name?: ts.Identifier;
  }

  function property(sym: ts.Symbol, dec: ts.Declaration): Property<t> {
    let optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
    let memType = checker.getTypeOfSymbolAtLocation(sym, dec);
    let t = getTSType(memType);
    return { name: sym.name, type: t, optional }
  }

  function visitDeclaration(node: MyNode): d {
    let processTypeParameters = function ( typeParameters: ts.NodeArray<ts.TypeParameterDeclaration> | undefined): TypeParameter<t>[] {
      return (!typeParameters)?[]:typeParameters.map(function(p: ts.TypeParameterDeclaration) {
        let d = p.default?getTSType(checker.getTypeAtLocation(p.default)):null;
        return { name: p.name.escapedText, default: d };
      })
    }
    if(ts.isInterfaceDeclaration(node)) {
      let nodeType = checker.getTypeAtLocation(node);
      let properties = nodeType.getProperties().map((sym: ts.Symbol) => property(sym, node));
      let fullyQualifiedName = checker.getFullyQualifiedName(nodeType.symbol);
      let i = {
        name: node.name.text,
        fullyQualifiedName,
        properties,
        typeParameters: processTypeParameters(node.typeParameters)
      };
      return onDeclaration.interface(i);
    }
    else if (ts.isTypeAliasDeclaration(node)) {
      let nodeType = checker.getTypeAtLocation(node);
      let x = {
        name: node.name.text,
        type: getTSType(nodeType),
        typeParameters: processTypeParameters(node.typeParameters)
      };
      return onDeclaration.typeAlias(x);
    }
    let nodeType = checker.getTypeAtLocation(node);
    let fullyQualifiedName = null;
    try {
      fullyQualifiedName = checker.getFullyQualifiedName(nodeType.symbol)
    } catch(e) {
    }

    return onDeclaration.unknown({ fullyQualifiedName, msg: "Unknown declaration node"})
  }

  function getTSType(memType: ts.Type): t {
    // Because we are processing only typelevel
    // declarations we can be sure that
    // these literals are type level entities.
    if(memType.isStringLiteral()) {
      return onTypeNode.stringLiteral(memType.value);
    }
    else if(memType.isNumberLiteral()) {
      return onTypeNode.numberLiteral(memType.value);
    }
    // XXX: I haven't found any other way to access
    // BooleanLiteral value...
    else if((memType.flags & ts.TypeFlags.BooleanLiteral) &&
            ((memType as any).intrinsicName == "true" ||
             (memType as any).intrinsicName == "false" )) {
      if((memType as any).intrinsicName == "true") {
          return onTypeNode.booleanLiteral(true);
      } else {
          return onTypeNode.booleanLiteral(false);
      }
    }
    else if (memType.flags & (ts.TypeFlags.String
            | ts.TypeFlags.BooleanLike | ts.TypeFlags.Number
            | ts.TypeFlags.Null | ts.TypeFlags.VoidLike | ts.TypeFlags.Any)) {
      return onTypeNode.primitive(checker.typeToString(memType));
    }
    else if (memType.isUnion()) {
      let types = memType.types.map(getTSType);
      return onTypeNode.union(types);
    }
    else if (memType.isIntersection()) {
      let types = memType.types.map(getTSType);
      return onTypeNode.intersection(types);
    }
    else if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
      let memObjectType = <ts.ObjectType>memType;
      let onInterfaceReference = function(target: ts.InterfaceType, typeArguments: t[]) {
        let ref = (target.symbol && target.symbol.valueDeclaration)
            ?target.symbol.valueDeclaration
            :(target.symbol && target.symbol.declarations.length === 1)
              ?target.symbol.declarations[0]
              :null;
        let fullyQualifiedName = checker.getFullyQualifiedName(target.symbol);
        return ref
          ?onTypeNode.typeReference({typeArguments, fullyQualifiedName, ref})
          :onTypeNode.unknown("Unable to get type declaration for:" + fullyQualifiedName + "<" + typeArguments + ">")
      }
      if(memObjectType.objectFlags & ts.ObjectFlags.Reference) {
        let reference = <ts.TypeReference>memObjectType;
        if(checker.isArrayType(reference)) {
          let elem = checker.getElementTypeOfArrayType(reference);
          if(elem)
            return onTypeNode.array(getTSType(elem));
        }
        if(checker.isTupleType(reference)) {
          let e: string, elem:ts.Type | undefined, elems:t[] = [];
          for(let i=0;; i++) {
            // Hack source:
            // https://github.com/microsoft/TypeScript/blob/v3.6.3/src/compiler/checker.ts + getTupleElementType
            e = "" + i as string;
            elem = checker.getTypeOfPropertyOfType(reference, e);
            if(elem) {
              elems.push(getTSType(elem));
            } else {
              break;
            }
          };
          return onTypeNode.tuple(elems);
        }
        if (reference.target.isClassOrInterface()) {
          let typeArguments = reference.typeArguments?reference.typeArguments.map(getTSType):[];
          return onInterfaceReference(reference.target, typeArguments);
        }
      }
      if(memObjectType.isClassOrInterface()) {
        return onInterfaceReference(memObjectType, []);
      }
      // This __seems__ to work in case of Pick<..>
      if((memObjectType.objectFlags & ts.ObjectFlags.Mapped) &&
         (memObjectType.objectFlags & ts.ObjectFlags.Instantiated)) {
        let props = memObjectType.getProperties().map((sym: ts.Symbol) => 
          property(sym, sym.declarations?sym.declarations[0]:sym.valueDeclaration)
        );
        let fullyQualifiedName = checker.getFullyQualifiedName(memObjectType.symbol);
        return onTypeNode.anonymousObject({ properties: props, fullyQualifiedName });
      }
      if(memObjectType.objectFlags & ts.ObjectFlags.Anonymous) {
        let props = memObjectType.getProperties().map((sym: ts.Symbol) => property(sym, sym.valueDeclaration));

        let fullyQualifiedName = checker.getFullyQualifiedName(memObjectType.symbol);
        return onTypeNode.anonymousObject({ fullyQualifiedName,  properties: props });
      }
      return onTypeNode.unknown("Uknown object type node (flags = " + memObjectType.objectFlags + "):" + checker.typeToString(memObjectType));
    }
    else if (memType.isTypeParameter()) {
      let d = memType.getDefault();
      return onTypeNode.typeParameter({ name: memType.symbol.escapedName, default: d?getTSType(d):null });
    }
    return onTypeNode.unknown(checker.typeToString(memType));
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
function createProgram(file: {path: string, source: Nullable<string>}, options: ts.CompilerOptions): ts.Program {
  const realHost = ts.createCompilerHost(options, true);
  let host = realHost;
  if(file.source) {
    let sourceFile = ts.createSourceFile(file.path, file.source, ts.ScriptTarget.ES5, true);
    host = {
      fileExists: filePath => filePath === file.path || realHost.fileExists(filePath),
      directoryExists: realHost.directoryExists && realHost.directoryExists.bind(realHost),
      getCurrentDirectory: realHost.getCurrentDirectory.bind(realHost),
      getDirectories: realHost.getDirectories?realHost.getDirectories.bind(realHost):undefined,
      getCanonicalFileName: fileName => realHost.getCanonicalFileName(fileName),
      getNewLine: realHost.getNewLine.bind(realHost),
      getDefaultLibFileName: realHost.getDefaultLibFileName.bind(realHost),
      getSourceFile: (fileName, languageVersion, onError, shouldCreateNewSourceFile) => fileName === file.path
          ? sourceFile 
          : realHost.getSourceFile(fileName, languageVersion, onError, shouldCreateNewSourceFile),
      readFile: filePath => filePath === file.path 
          ? file.source?file.source:undefined
          : realHost.readFile(filePath),
      useCaseSensitiveFileNames: () => realHost.useCaseSensitiveFileNames(),
      writeFile: (_, data) => { data },
    };
  } 
  return ts.createProgram([file.path], options, host);
}