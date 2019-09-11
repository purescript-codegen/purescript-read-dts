export type AliasOfPrimitiveTypes = number | string;

export type AliasWithRecursion<t> = { head : t, tail: AliasWithRecursion<t> } | null;

export interface Base1<f, s> {
  f: f,
  s: s
}

export interface Base2 {
  second: string
}

// export interface BaseProps<t> {
//   classes: t
// }
// 
// export type X = { x: string };
// 
// export interface OverlappingName {
//   prop1: 0 | 1 | 2
//   prop2: '0' | '1' | '2'
// }
// 
// export namespace z {
//   interface zInner {
//     prop: 0 | 1
//   }
// }
