import ts from "typescript";

export function createCompilerHostImpl(options: ts.CompilerOptions) : ts.CompilerHost {
  return ts.createCompilerHost(options, true);
}

export function createProgramImpl(rootNames: ReadonlyArray<string>, options: ts.CompilerOptions, host?: ts.CompilerHost) : ts.Program {
  return ts.createProgram(rootNames, options, host);
}

export function getTypeChecker(program: ts.Program) : ts.TypeChecker {
  return program.getTypeChecker();
}

export function getRootFileNames(program: ts.Program) : ReadonlyArray<string> {
  return program.getRootFileNames();
}

export function emit(program: ts.Program) : ts.EmitResult {
  return program.emit();
}

export function getSourceFiles(program: ts.Program) : ReadonlyArray<ts.SourceFile> {
  return program.getSourceFiles();
}

export function getFileName(sourceFile: ts.SourceFile) : string {
  return sourceFile.fileName;
}

export function getPreEmitDiagnostics(program: ts.Program) : ReadonlyArray<ts.Diagnostic> {
  return ts.getPreEmitDiagnostics(program);
}

export function emitResultDiagnostics(emitResult: ts.EmitResult) : ReadonlyArray<ts.Diagnostic> {
  return emitResult.diagnostics;
}
