import * as ts from "typescript";

export const fromCompilerHost = (host: ts.CompilerHost) => {
  return {
    fileExists: host.fileExists.bind(host),
    directoryExists: host.directoryExists && host.directoryExists.bind(host),
    getCurrentDirectory: host.getCurrentDirectory && host.getCurrentDirectory.bind(host),
    getDirectories: host.getDirectories && host.getDirectories.bind(host),
    getCanonicalFileName: host.getCanonicalFileName.bind(host),
    getNewLine: host.getNewLine.bind(host),
    getDefaultLibFileName: host.getDefaultLibFileName.bind(host),
    getSourceFile: host.getSourceFile.bind(host),
    readFile: host.readFile.bind(host),
    useCaseSensitiveFileNames: host.useCaseSensitiveFileNames.bind(host),
    writeFile: host.writeFile.bind(host),
  };
}

