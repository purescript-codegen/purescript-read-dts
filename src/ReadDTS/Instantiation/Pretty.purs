module ReadDTS.Instantiation.Pretty where

import Prelude

import Data.Array (uncons) as Array
import Data.Foldable (foldMap, foldl, intercalate, length)
import Data.List (List(..)) as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith) as String
import Data.Tuple (Tuple(..)) as Tuple
import Matryoshka (Algebra, cata)
import ReadDTS.Instantiation (Type, TypeF(..))
import Text.Pretty (Columns(..), Doc, Stack(..), beside, empty, hcat, render, text, width)

joinWithDoc ∷ Doc → Array Doc → Maybe Int → Doc
joinWithDoc sep elems (Just w) = case Array.uncons elems of
  Nothing → empty 0 0
  Just { head, tail } →  doc
    where
      step { row, rows } curr = if width row + width sep + width curr > w
        then { row: curr, rows: List.Cons row rows }
        else { row: row `beside` sep `beside` curr, rows }
      { row, rows } = foldl step { row: head, rows: List.Nil } tail
      doc = unwrap $ foldl (\rows curr → rows <> Stack (sep `beside` curr)) (Stack row) $ rows

joinWithDoc sep elems Nothing =
  unwrap $ intercalate (Columns sep) (map Columns elems)

pprint ∷ Type → String
pprint = render <<< cata alg
  where
  alg ∷ Algebra TypeF Doc
  alg Any = text "any"
  alg (Array t) = hcat [ text "[", t, text "]" ]
  alg Boolean = text "boolean"
  alg (Function { fullyQualifiedName, parameters, returnType }) = hcat
    [ text "function "
    , text fullyQualifiedName
    , joinWithDoc (text ", ") (map (text <<< _.name) parameters) (Just 80)
    , text ") : "
    , returnType
    ]
  alg (Intersection t1 t2) = hcat [ t1, text " & ", t2 ]
  alg Null = text "null"
  alg Number = text "number"
  alg (Object _ props) | length props == 0 = text "{}"
  alg (Object fqn props) = objDoc
    where
      sep = text ":  "
      sepOpt = text "?: "
      step (Tuple.Tuple n { type: t, optional }) =
        Stack $ hcat [ text n, (if optional then sepOpt else sep), t ]
      nameCol = Columns $ text $ "<" <> fqn <> ">: "
      propsCol
        = Columns
        <<< unwrap
        <<< foldMap step
        $ (Map.toUnfoldable props ∷ Array _)
      objDoc = unwrap $ nameCol <> propsCol
  alg String = text "string"
  alg (Tuple ts) =
    hcat [ text "(", joinWithDoc (text ", ") ts (Just 80), text ")" ]
  alg (BooleanLiteral b) = text $ "@" <> show b
  alg (StringLiteral s) = text $ "@" <> show s
  alg (NumberLiteral n) = text $ "@" <> show n
  alg Undefined = text "undefined"
  alg (Union ts) = joinWithDoc (text " | ") ts (Just 80)
  alg (Unknown s) = text $ "unknown: " <> s <> ""
  alg Void = text "void"

pprintTypeName ∷ TypeF String → String
pprintTypeName Any = "any"
pprintTypeName (Array t) = "[ " <> t <> "]"
pprintTypeName (Boolean) = "boolean"
pprintTypeName (Function r) = "function:" <> r.fullyQualifiedName
pprintTypeName (Intersection t1 t2) = "intersection: " <> t1 <> " & " <> t2
pprintTypeName Null = "null"
pprintTypeName Number = "number"
pprintTypeName (NumberLiteral n) = "@" <> show n
pprintTypeName (Object _ ms) | length ms == 0 = "{}"
pprintTypeName (Object fqn _) = "<" <> fqn <> ">"
pprintTypeName String = "string"
pprintTypeName (Union ts) = String.joinWith " | " ts
pprintTypeName (Tuple ts) = "(" <> String.joinWith ", " ts <> ")"
pprintTypeName (BooleanLiteral b) = "@" <> show b
pprintTypeName (StringLiteral s) = "@" <> s
pprintTypeName Undefined = "undefined"
pprintTypeName (Unknown s) = "unkown:" <> s
pprintTypeName Void = "void"

