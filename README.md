# purescript-read-dts

Provide a way to work with TypeScript types declarations.



## Status

Currently we are in a somewhat inconsistent state. On the one hand this codebase is very rough in a many places and it is developed thanks to the live experimentation and direct source browsing of the typescript compiler. On the other hand we successfully test and use it against quite a large codegen projects.

As a main battlefield we have choosen `purescript-react-basic-mui` so please check this project for an extended example. Please consider also [Test.ReadDTS.Instantiation](
https://github.com/purescript-codegen/purescript-read-dts/blob/master/test/ReadDTS/Instantiation.purs) as a form of current documentation.

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


## Credits

* This library has started as an attempt to fix some bugs in `purescript-readts`. It has diverged into a separate lib and till its full maturation we want to keep it separate and not propose any merges.

* Initial developement was funded by lambdaterms.com
