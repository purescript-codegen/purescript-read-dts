module TS.Codegen.PS where

import Prelude
import PureScript.CST.Types
import Tidy.Codegen
import Tidy.Codegen.Monad

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

-- tsClass :: Module Void
-- tsClass = unsafePartial $ codegenModule "Data.Maybe" do
--   importOpen "Prelude"
--   tell
--     [ declData "Maybe" [ typeVar "a" ]
--         [ dataCtor "Nothing" []
--         , dataCtor "Just" [ typeVar "a" ]
--         ]
-- 
--     , declDerive Nothing []
--         (typeApp "Functor" [ typeCtor "Maybe" ])
-- 
--     , declSignature "maybe" do
--         typeForall [ typeVar "a", typeVar "b" ] do
--           typeArrow
--             [ typeVar "b"
--             , typeArrow [ typeVar "a" ] (typeVar "b")
--             , typeApp (typeCtor "Maybe") [ typeVar "a" ]
--             ]
--             (typeVar "b")
--     , declValue "maybe" [ binderVar "nothing", binderVar "just" ] do
--         exprCase [ exprSection ]
--           [ caseBranch [ binderCtor "Just" [ bindarVar "a" ] ] do
--               exprApp (exprIdent "just") [ exprIdent "a" ]
--           , caseBranch [ binderCtor "Nothing" [] ] do
--               exprIdent "nothing"
--           ]
--     ]
