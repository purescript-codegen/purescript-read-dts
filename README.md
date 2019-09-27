# purescript-read-dts


## History

This library has started as an fork and attempt to fix some bugs in `purescript-readts`. Beacause @doolse (the readts author) is not responsive at the moment and we wanted to have something working quickly we had decided to rewrite the whole thing.

## Goals

Provide a way to work with `typescript` type declarations.

## Architecture

This libarary wraps `ts` complier API and provides a way to build recursive AST from it. It seems that it is really hard to cover the whole `ts` type system related to type declartion with public compiler API but we are trying our best.

...and of course real docs comming soon :-P


