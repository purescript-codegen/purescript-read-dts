import * as ts from "typescript";

type Unit = void;
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

export function _readDTS<t>(
  options: ts.CompilerOptions,
  visit: {
    onDeclaration: {
      interface: (x:
        {
          name: string,
          fullyQualifiedName: string,
          members: { name: string, type: t, optional: boolean }[]
          typeParameters: t[]
        }) => Effect<Unit>
      typeAlias: (x: { name: string, type: t }) => Effect<Unit>
    },
    onType: {
      union: (types: t[]) => t,
      intersection: (types: t[]) => t,
      interfaceReference: (i: { typeArguments: t[], fullyQualifiedName: string, read: Effect<Unit> }) => t,
      stringLiteral: (s: string) => t,
      numberLiteral: (n: number) => t,
      primitive: (name: string) => t,
      typeParameter: (name: ts.__String) => t,
      unknown: (name: string) => t
    }
  },
  fileName: string
): Unit {
  let program = ts.createProgram([fileName], options);
  let checker = program.getTypeChecker();
  let onDeclaration = visit.onDeclaration;
  let onType = visit.onType;

  // Check only given declaration file
  for (const sourceFile of program.getSourceFiles()) {
    if (sourceFile.isDeclarationFile && sourceFile.fileName === fileName) {
      ts.forEachChild(sourceFile, visitDeclaration);
    }
  }

  interface MyNode extends ts.Node {
    name?: ts.Identifier;
  }

  function visitDeclaration(node: MyNode) {
    // Only consider exported nodes
    if (!isNodeExported(node)) { return; }

    if(ts.isInterfaceDeclaration(node)) {
      let nodeType = checker.getTypeAtLocation(node);
      let members = nodeType.getProperties().map((sym: ts.Symbol) => {
        let optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
        let memType = checker.getTypeOfSymbolAtLocation(sym, node);
        let t = getTSType(memType);
        return { name: sym.name, type: t, optional }
      });
      let fullyQualifiedName = checker.getFullyQualifiedName(nodeType.symbol);
      let typeParameters = (!node.typeParameters)?[]:node.typeParameters.map(function(p: ts.TypeParameterDeclaration) {
        return onType.typeParameter(p.name.escapedText);
      })
      let i = { name: node.name.text, fullyQualifiedName, members, typeParameters };
      onDeclaration.interface(i)();
    }
    else if (ts.isTypeAliasDeclaration(node)) {
      let nodeType = checker.getTypeAtLocation(node);
      let x = { name: node.name.text, type: getTSType(nodeType) };
      onDeclaration.typeAlias(x)();
    }
    else if (ts.isModuleDeclaration(node)) {
      ts.forEachChild(node, visitDeclaration);
    }
    else {
      let nodeType = checker.getTypeAtLocation(node);
      if(nodeType.symbol)
        console.log(nodeType.symbol.getName());
    }
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
        return onType.interfaceReference({
          typeArguments,
          fullyQualifiedName: checker.getFullyQualifiedName(target.symbol),
          read: function() {
            // XXX: This is for sure stupid strategy to access external interfaces.
            if(target.symbol) {
              if(target.symbol.valueDeclaration) {
                return visitDeclaration(target.symbol.valueDeclaration);
              }
              // XXX: I'm not sure why have to use declarations here...
              //      For sure we should introduce proper error handling.
              else if(target.symbol.declarations.length === 1) {
                return visitDeclaration(target.symbol.declarations[0]);
              }
            }
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
      return onType.typeParameter(memType.symbol.escapedName);
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
