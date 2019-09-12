import * as ts from "typescript";

type Effect<a> = () => a;

exports.eqIdentifierImpl = function(i1: ts.Identifier) {
  return function(i2: ts.Identifier) {
    return i1 === i2;
  }
}

export const compilerOptions = {
  target: ts.ScriptTarget.ES5,
  module: ts.ModuleKind.CommonJS
};

type Nullable<a> = a | null;
type TypeParameter<t> = { identifier: ts.__String, default: Nullable<t> };

export function _readDTS<d, t>(
  options: ts.CompilerOptions,
  visit: {
    onDeclaration: {
      interface: (x:
        {
          name: string,
          fullyQualifiedName: string,
          members: { name: string, type: t, optional: boolean }[]
          typeParameters: t[]
        }) => d
      typeAlias: (x: { name: string, type: t, typeParameters: t[] }) => d
      unknown: (u: { fullyQualifiedName: Nullable<string>, msg: string }) => d
    },
    onType: {
      union: (types: t[]) => t,
      intersection: (types: t[]) => t,
      stringLiteral: (s: string) => t,
      numberLiteral: (n: number) => t,
      primitive: (name: string) => t,
      typeParameter: (tp: TypeParameter<t>) => t,
      typeReference: (i: { typeArguments: t[], fullyQualifiedName: string, read: Effect<d> }) => t,
      unknown: (err: string) => t
    }
  },
  fileName: string
): d[] {
  let program = ts.createProgram([fileName], options);
  let checker = program.getTypeChecker();
  let onDeclaration = visit.onDeclaration;
  let onType = visit.onType;
  let result:d[] = [];

  // Check only given declaration file
  for (const sourceFile of program.getSourceFiles()) {
    if (sourceFile.isDeclarationFile && sourceFile.fileName === fileName) {
      ts.forEachChild(sourceFile, function(declaration) {
        if (isNodeExported(declaration))
          result.push(visitDeclaration(declaration));
      });
    }
  }
  return result;

  interface MyNode extends ts.Node {
    name?: ts.Identifier;
  }

  function visitDeclaration(node: MyNode): d {
    let processTypeParameters = function ( typeParameters: ts.NodeArray<ts.TypeParameterDeclaration> | undefined): t[] {
      return (!typeParameters)?[]:typeParameters.map(function(p: ts.TypeParameterDeclaration) {
        let d = p.default?getTSType(checker.getTypeAtLocation(p.default)):null;
        return onType.typeParameter({ identifier: p.name.escapedText, default: d });
      })
    }
    if(ts.isInterfaceDeclaration(node)) {
      let nodeType = checker.getTypeAtLocation(node);
      let members = nodeType.getProperties().map((sym: ts.Symbol) => {
        let optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
        let memType = checker.getTypeOfSymbolAtLocation(sym, node);
        let t = getTSType(memType);
        return { name: sym.name, type: t, optional }
      });
      let fullyQualifiedName = checker.getFullyQualifiedName(nodeType.symbol);
      let i = {
        name: node.name.text,
        fullyQualifiedName,
        members,
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
    if (memType.flags & (ts.TypeFlags.String
      | ts.TypeFlags.BooleanLike | ts.TypeFlags.Number
      | ts.TypeFlags.Null | ts.TypeFlags.VoidLike | ts.TypeFlags.Any)) {
      return onType.primitive(checker.typeToString(memType));
    }
    else if (memType.isUnion()) {
      let types = memType.types.map(getTSType);
      return onType.union(types);
    }
    else if (memType.isIntersection()) {
      let types = memType.types.map(getTSType);
      return onType.intersection(types);
    }
    else if (memType.isStringLiteral()) {
      return onType.stringLiteral(memType.value);
    }
    else if (memType.isNumberLiteral()) {
      return onType.numberLiteral(memType.value);
    }
    else if (memType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
      let memObjectType = <ts.ObjectType>memType;
      let onInterfaceReference = function(target: ts.InterfaceType, typeArguments: t[]) {
        return onType.typeReference({
          typeArguments,
          fullyQualifiedName: checker.getFullyQualifiedName(target.symbol),
          read: function():d {
            // XXX: This is for sure stupid strategy to access external interfaces.
            let r = null;
            if(target.symbol) {
              if(target.symbol.valueDeclaration) {
                r = visitDeclaration(target.symbol.valueDeclaration);
              }
              // XXX: I'm not sure why have to use declarations here...
              //      For sure we should introduce proper error handling.
              if(target.symbol.declarations.length === 1) {
                r = visitDeclaration(target.symbol.declarations[0]);
              }
            }
            return (r?r:visit.onDeclaration.unknown({
              fullyQualifiedName: this.fullyQualifiedName,
              msg: "Unable to extract declaration"
            }));
          }
        });
      }
      if(memObjectType.objectFlags & ts.ObjectFlags.Reference) {
        let reference = <ts.TypeReference>memObjectType;
        if (reference.target.isClassOrInterface()) {
          let typeArguments = reference.typeArguments?reference.typeArguments.map(getTSType):[];
          return onInterfaceReference(reference.target, typeArguments);
        }
      } else if(memObjectType.isClassOrInterface()) {
        return onInterfaceReference(memObjectType, []);
      }
    }
    // I'm not sure why this check turns memType type into `never`
    else if (memType.isTypeParameter()) {
      let d = memType.getDefault();
      return onType.typeParameter({ identifier: memType.symbol.escapedName, default: d?getTSType(d):null });
    }
    return onType.unknown(checker.typeToString(memType));
  }
  function isNodeExported(node: ts.Node): boolean {
    let sym = checker.getSymbolAtLocation(node);

    return (
      // (ts.getCombinedModifierFlags(node.) & ts.ModifierFlags.Export) !== 0 ||
      (sym ? ((ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0) : false) ||
      (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile)
    );
  }
}
