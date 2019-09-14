module ReadDTS.Instantiation where

import Data.Map (Map)

data Type
  = Boolean
  | Intersection (Array Type)
  | Number
  | Object (Map String { t ∷ Type, optional ∷ Boolean })
  | String
  | Union (Array Type)
  | Uknown String


