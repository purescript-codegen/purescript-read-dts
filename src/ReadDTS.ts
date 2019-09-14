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

 type Property<t> = { name: string, type: t, optional: boolean }

export function _readDTS<d, t>(
  options: ts.CompilerOptions,
  visit: {
    onDeclaration: {
      interface: (x:
        {
          name: string,
          fullyQualifiedName: string,
          properties: Property<t>[]
          typeParameters: t[]
        }) => d
      typeAlias: (x: { name: string, type: t, typeParameters: t[] }) => d
      unknown: (u: { fullyQualifiedName: Nullable<string>, msg: string }) => d
    },
    onTypeNode: {
      anonymousObject: (properties: (Property<t>)[]) => t,
      intersection: (types: t[]) => t,
      primitive: (name: string) => t,
      typeParameter: (tp: TypeParameter<t>) => t,
      typeReference: (i: { typeArguments: t[], fullyQualifiedName: string, ref: Nullable<ts.Declaration> }) => t,
      union: (types: t[]) => t,
      unknown: (err: string) => t
    }
  },
  fileName: string
): { topLevel: d[], readDeclaration: (v: ts.Declaration) => Effect<d> }{
  let program = ts.createProgram([fileName], options);
  let checker = program.getTypeChecker();
  let onDeclaration = visit.onDeclaration;
  let onTypeNode = visit.onTypeNode;
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
  return {
    topLevel: result,
    readDeclaration: (v:ts.Declaration) => () => visitDeclaration(v)
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
    let processTypeParameters = function ( typeParameters: ts.NodeArray<ts.TypeParameterDeclaration> | undefined): t[] {
      return (!typeParameters)?[]:typeParameters.map(function(p: ts.TypeParameterDeclaration) {
        let d = p.default?getTSType(checker.getTypeAtLocation(p.default)):null;
        return onTypeNode.typeParameter({ identifier: p.name.escapedText, default: d });
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
    if (memType.flags & (ts.TypeFlags.String
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
        if (reference.target.isClassOrInterface()) {
          let typeArguments = reference.typeArguments?reference.typeArguments.map(getTSType):[];
          return onInterfaceReference(reference.target, typeArguments);
        }
      } else if(memObjectType.isClassOrInterface()) {
        return onInterfaceReference(memObjectType, []);
      } else if(memObjectType.objectFlags & ts.ObjectFlags.Anonymous) {
        let props = memObjectType.getProperties().map((sym: ts.Symbol) => property(sym, sym.valueDeclaration));
        return onTypeNode.anonymousObject(props);
      }
      return onTypeNode.unknown("Uknown object type node (flags = " + memObjectType.objectFlags + "):" + checker.typeToString(memObjectType));
    }
    // I'm not sure why this check turns memType type into `never`
    else if (memType.isTypeParameter()) {
      let d = memType.getDefault();
      return onTypeNode.typeParameter({ identifier: memType.symbol.escapedName, default: d?getTSType(d):null });
    }
    return onTypeNode.unknown(checker.typeToString(memType));
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
