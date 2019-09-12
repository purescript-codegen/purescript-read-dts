import { Base1, Base2 } from './base.d';

export type AliasForUnionOfPrimitiveTypes = number | string;

export type AliasWithRecursion<t> = { head : t, tail: AliasWithRecursion<t> } | null;

export interface InterfaceWithAliasProps<t> {
  base1: Base1<t, string>
  base2: Base2
}

export interface Final {
  i: InterfaceWithAliasProps<string>
}

type F = Final;
