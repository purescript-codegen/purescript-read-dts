# purescript-read-dts

Provide a way to work with TypeScript types declarations.

## History

This library has started as an attempt to fix some bugs in `purescript-readts`. It has diverged into a separate lib and till its full maturation we want to keep it separate and not propose any merges.


## Status

Currently we are in a somewhat inconsistent state. On one hand this codebase is rough in many places and it is developed thanks to live experimentation by direct source browsing of the typescript compiler. On the other hand we with are successfully using it and testing it against quite a large codegen.

As a main battlefield we have choosen `purescript-react-basic-mui` and it has already prooven to be quite successful approach but on quite limited subset of Typescript constructs.
There are for sure multiple pieces there which should be incorporated back here. You can look there for some inspiration how this lib can be used:

https://github.com/dwhitney/purescript-react-basic-mui/tree/codegen-read-dts/codegen/src

## Design

API of this library consists of three layers.

  * We have semivisitor API build upon TypeScript compiler API and provided by `ReadDTS.ts`and `ReadDTS.purs`.

  * We have an opinionated minimal AST which represents TypeScript types declarations. You can find it in `ReadDTS/AST.purs`. Initially this AST contains unresolved refreneces to other types which are filled during unfolding done by effectfull `AST.coalgebra`.

  * Finally we have `ReadDTS/Instantiation.purs` module which provides an AST for an instantiated type from a given type declaration.

This is directed and acyclical dependency graph of modules so you can use any previous layer without bothering about the next step ASTs design.


## Testing

Currently we are runnig only `test/ReadDTS/Instantiation.purs` as a part of main suite.

```
$ spago test
```
