// import { Base1, Base2 } from './base.d';
// 
// export type AliasForUnionOfPrimitiveTypes = number | string;
// 
// export type AliasWithRecursion<t> = { head : t, tail: AliasWithRecursion<t> } | null;

export interface InterfaceWithAliasProps<t=string> {
  // base1: Base1<t, string>
  // base2: Base2
  obj: {
    objNumber: number
    objT: t
  }
}

// export interface Final {
//   i: InterfaceWithAliasProps<string>
// }
// 
// type F = Final;
