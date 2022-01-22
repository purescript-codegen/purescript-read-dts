"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getFileName = exports.getSourceFiles = exports.getRootFileNames = exports.getTypeChecker = exports.createProgramImpl = exports.createCompilerHostImpl = void 0;
var ts = __importStar(require("typescript"));
function createCompilerHostImpl(options) {
    return ts.createCompilerHost(options, true);
}
exports.createCompilerHostImpl = createCompilerHostImpl;
function createProgramImpl(rootNames, options, host) {
    return ts.createProgram(rootNames, options, host);
}
exports.createProgramImpl = createProgramImpl;
function getTypeChecker(program) {
    return program.getTypeChecker();
}
exports.getTypeChecker = getTypeChecker;
function getRootFileNames(program) {
    return program.getRootFileNames();
}
exports.getRootFileNames = getRootFileNames;
function getSourceFiles(program) {
    return program.getSourceFiles();
}
exports.getSourceFiles = getSourceFiles;
function getFileName(sourceFile) {
    return sourceFile.fileName;
}
exports.getFileName = getFileName;
//# sourceMappingURL=Program.js.map