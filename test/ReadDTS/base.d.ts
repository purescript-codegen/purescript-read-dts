export interface Base1<t1, t2, t3, n=number> {
  t1: t1,
  t2: t2[],
  t3: [t3],
  union: '1' | 1 | 'value'
  tp: [string, number, any],
  n: n
}

export interface Base2<t=string> {
  t: t
}

export interface Base3 {
  s: string
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
