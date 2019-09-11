import { Base1, Base2 } from './base.d';

export interface InterfaceWithAliasProps<t> {
  base1: Base1<t, string>
  base2: Base2
}

export interface Final {
  i: InterfaceWithAliasProps<string>
}

