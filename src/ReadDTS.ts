import * as ts from "typescript";

export const compilerOptions = {
  target: ts.ScriptTarget.ES5,
  module: ts.ModuleKind.CommonJS
};

export function _readDTS<t, a>(
  fileName: string,
  options: ts.CompilerOptions,
  onVisit: {
    interface: (x:
      {
        name: string,
        members: { name: string, type: t, optional: boolean }[]
      }) => a,
    typeAlias: (x: { name: string, type: t }) => a
  },
  onType: {
    unionOrIntersection: (types: t[]) => t,
    stringLiteral: (s: string) => t,
    numberLiteral: (n: number) => t,
    primitive: (name: string) => t,
    unknown: (name: string) => t
  }
): a[] {
  // Build a program using the set of root file names in fileNames
  let program = ts.createProgram([fileName], options);

  let checker = program.getTypeChecker();

  let output: a[] = [];

  // Visit every sourceFile in the program
  for (const sourceFile of program.getSourceFiles()) {
    if (sourceFile.fileName === fileName) {
      ts.forEachChild(sourceFile, visit);
    }
  }

  return output;

  interface MyNode extends ts.Node {
    name?: ts.Identifier;
  }

  function visit(node: MyNode) {
    // Only consider exported nodes
    if (!isNodeExported(node)) { return; }

    if (
      (node.kind === ts.SyntaxKind.InterfaceDeclaration
        || node.kind === ts.SyntaxKind.TypeAliasDeclaration)
      && node.name) {
      // let symbol = checker.getSymbolAtLocation(node.name);
      // if (symbol) {
      let nodeType = checker.getTypeAtLocation(node);
      if (nodeType.isClassOrInterface()) {
        let members = nodeType.getProperties().map((sym: ts.Symbol) => {
          let optional = (sym.flags & ts.SymbolFlags.Optional) == ts.SymbolFlags.Optional;
          let memType = checker.getTypeOfSymbolAtLocation(sym, node);
          return { name: sym.name, type: getTSType(memType), optional }
        });

        let x = { name: node.name.text, members };

        // console.log(x);
        output.push(onVisit.interface(x));
      } else {
        let x = { name: node.name.text, type: getTSType(nodeType) };
        // console.log(x);
        output.push(onVisit.typeAlias(x));
      }

    } else if (node.kind === ts.SyntaxKind.ModuleDeclaration) {
      // This is a namespace, visit its children
      ts.forEachChild(node, visit);
    }
  }

  function getTSType(memType: ts.Type): t {
    if (memType.isUnionOrIntersection()) {
      let types = memType.types.map(getTSType);
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
      return onType.unknown(checker.typeToString(memType));
      // return { unknown: checker.typeToString(memType), flags: memType.flags };
    }
  }

  /** True if this is visible outside this file, false otherwise */
  function isNodeExported(node: ts.Node): boolean {
    let sym = checker.getSymbolAtLocation(node);

    return (
      // (ts.getCombinedModifierFlags(node.) & ts.ModifierFlags.Export) !== 0 ||
      (sym ? ((ts.getCombinedModifierFlags(sym.valueDeclaration) & ts.ModifierFlags.Export) !== 0) : false) ||
      (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile)
    );
  }
}
