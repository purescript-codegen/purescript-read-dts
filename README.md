# purescript-read-dts

## Status

_Major rewrite_. I want to put here some stubs for the notes:

* Explain `TypeScript` package structure:

  - it tries to follow original `typescript/typescript` project structure.

  - `Internal` modules are modules which expose typescript implementation internals which seems to be really useful.

  - `Contrib` subpackage contains additions - for clarity

  - Bindings to `is*` are usually renamed to `as*` functions which return `Maybe t` where `t` is narrowed type

  - `Node r` - provide info about interfaces access model.

  - Add info about immutability of the Program from the checker and the decision behind "pure" bindings.

    [A Program is an immutable collection of 'SourceFile's and a 'CompilerOptions']("https://github.com/microsoft/TypeScript/blob/v4.5.2/src/compiler/program.ts#L842")


## Old docs for reference during rewrite:

---

Provide a way to work with TypeScript [types declarations](https://stackoverflow.com/questions/21247278/about-d-ts-in-typescript).

### Status

Currently we are in a somewhat inconsistent state. On the one hand this codebase is very rough in a many places and it is developed thanks to the live experimentation and direct source browsing of the typescript compiler. On the other hand we successfully test and use it against quite a large codegen projects.

As a main battlefield we have choosen `purescript-react-basic-mui` so please check this project for an extended example. 

API of this library consists of three layers.

  * We have semivisitor API build upon TypeScript compiler API and provided by `ReadDTS.ts`and `ReadDTS.purs`.

  * We have an opinionated minimal AST which represents TypeScript types declarations. You can find it in `ReadDTS/AST.purs`. Initially this AST contains unresolved refreneces to other types which are filled during unfolding done by effectfull `AST.coalgebra`.

  * Finally we have `ReadDTS/Instantiation.purs` module (it [should be renamed to](/purescript-codegen/purescript-read-dts/issues/7) `Specialization.purs`) which provides an AST for an instantiated type from a given type declaration.

This is directed and acyclical dependency graph of modules so you can use any previous layer without bothering about the next step ASTs design.

Please, consider [Test.ReadDTS.Instantiation](
https://github.com/purescript-codegen/purescript-read-dts/blob/master/test/ReadDTS/Instantiation.purs) as a form of a short guide for the top layer of this lib.

### Testing

Currently we are runnig only `test/ReadDTS/Instantiation.purs` as a part of main suite.

```
$ spago test
```

### Credits

* This library has started as an attempt to fix some bugs in `purescript-readts`. It has diverged into a separate lib and till its full maturation we want to keep it separate and not propose any merges.

* Initial developement was funded by lambdaterms.com

### Useful links
* [Typescript compiler API - using the type checker](https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API#using-the-type-checker)
* [purescript-tsd-gen](https://github.com/minoki/purescript-tsd-gen) - Generate DTS from purescript, the reverse of this repository.
* Generate json-schema from your Typescript sources: [typescript-json-schema](https://github.com/YousefED/typescript-json-schema). This is useful to see how the typescript compiler works and which information can be extracted from it.
