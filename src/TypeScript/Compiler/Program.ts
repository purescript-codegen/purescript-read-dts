import * as ts from "typescript";

export function createCompilerHostImpl(options: ts.CompilerOptions) : ts.CompilerHost {
  return ts.createCompilerHost(options, true);
}

export function createProgramImpl(rootNames: ReadonlyArray<string>, options: ts.CompilerOptions, host?: ts.CompilerHost) {
  return ts.createProgram(rootNames, options, host);
}

export function getTypeChecker(program: ts.Program) {
  return program.getTypeChecker();
}

export function getRootFileNames(program: ts.Program) {
  return program.getRootFileNames();
}

export function getSourceFiles(program: ts.Program) {
  return program.getSourceFiles();
}

export function getFileName(sourceFile: ts.SourceFile) {
  return sourceFile.fileName;
}

