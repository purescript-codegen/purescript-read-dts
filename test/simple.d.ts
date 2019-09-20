import { Base1, Base2, Base3 } from './base.d';

export interface A<t=string> {
  base1: Base1<t, string, number>
  base2: Base2
  base3: Base3
  obj: {
    objNumber: number
    objT: t
  }
}

// export type AliasForUnionOfPrimitiveTypes = number | string;
// 
// export type AliasWithRecursion<t> = { head : t, tail: AliasWithRecursion<t> } | null;


// export interface Final {
//   i: InterfaceWithAliasProps<string>
// }
// 
// type F = Final;
