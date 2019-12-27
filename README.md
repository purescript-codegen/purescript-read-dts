# purescript-read-dts


## History

This library has started as an attempt to fix some bugs in `purescript-readts`. It has diverged into separate lib and till its full maturation we want to keep it separate and not propse any merges.
As a main battlefield we have choosen `purescript-react-basic-mui` and it has already prooven to be quite successful approach but on quite limited subset of Typescript constructs.
There are for sure multiple pieces there which should be incorporated back here. You can look there for some inspiration how this lib can be used:

https://github.com/dwhitney/purescript-react-basic-mui/tree/codegen-read-dts/codegen/src

## Goals

Provide a way to work with `typescript` type declarations.

## Architecture

This libarary wraps `ts` complier API and provides a way to build recursive AST from it. It seems that it is really hard to cover the whole `ts` type system related to type declartion with public compiler API but we are trying our best.

...and of course real docs comming soon :-P


