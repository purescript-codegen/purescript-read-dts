import ts from "typescript";
export function createCompilerHostImpl(options) {
    return ts.createCompilerHost(options, true);
}
export function createProgramImpl(rootNames, options, host) {
    return ts.createProgram(rootNames, options, host);
}
export function getTypeChecker(program) {
    return program.getTypeChecker();
}
export function getRootFileNames(program) {
    return program.getRootFileNames();
}
export function emit(program) {
    return program.emit();
}
export function getSourceFiles(program) {
    return program.getSourceFiles();
}
export function getFileName(sourceFile) {
    return sourceFile.fileName;
}
export function getPreEmitDiagnostics(program) {
    return ts.getPreEmitDiagnostics(program);
}
export function emitResultDiagnostics(emitResult) {
    return emitResult.diagnostics;
}
//# sourceMappingURL=Program.js.map