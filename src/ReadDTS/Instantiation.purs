module ReadDTS.Instantiation where

import Data.Map (Map)

-- | Not our current problems:
-- |
-- | * How to represent recursive cycles so they can be useful?
-- | * Should we provide some lazy evaluating value in place of recursive type?
-- | * Should we provide `fullyQualifiedNames` somewhere too?

-- | Current problems:
-- | * We want to parametrize this by itself so we can use recursion schemes on this type.
-- | * We want to move `AST.apply*` here and rename it probably `Instatiation.typeNode`
-- | `Instantiation.application`.
-- | * We want to start from the basic types up :-)

data Type
  = Array Type
  | Boolean
  | Number
  | Object (Map String { t ∷ Type, optional ∷ Boolean })
  | String
  | Tuple (Array Type)
  | Union (Array Type)
  | Uknown String


