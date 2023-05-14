module ReadDTS.AST.Printer where

-- Quick and really dirty (sorry about that) printer for the AST.

import Prelude

import Data.Array (fromFoldable, uncons) as Array
import Data.Foldable (foldMap, foldl, intercalate)
import Data.List (List(..)) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Matryoshka (Algebra, cata)
import ReadDTS (fqnToString)
import ReadDTS.AST (TsType(..))
import Text.Pretty (Columns(..), Doc, Stack(..), beside, empty, hcat, render, text, width)
import TypeScript.Compiler.Types (FullyQualifiedName)

joinWithDoc :: Doc -> Array Doc -> Maybe Int -> Doc
joinWithDoc sep elems (Just w) = case Array.uncons elems of
  Nothing -> empty 0 0
  Just { head, tail } -> doc
    where
    step { row, rows } curr =
      if width row + width sep + width curr > w then { row: curr, rows: List.Cons row rows }
      else { row: row `beside` sep `beside` curr, rows }
    { row, rows } = foldl step { row: head, rows: List.Nil } tail
    doc = unwrap $ foldl (\rows curr -> rows <> Stack (sep `beside` curr)) (Stack row) $ rows

joinWithDoc sep elems Nothing =
  unwrap $ intercalate (Columns sep) (map Columns elems)

pprintProps name props = objDoc
  where
  sep = text ":  "
  sepOpt = text "?: "
  step ({ name: n, type: t, optional }) =
    Stack $ hcat [ text n, (if optional then sepOpt else sep), t ]
  nameCol = Columns $ name -- $ "<" <> fqn <> ">: "
  propsCol =
    Columns
      <<< unwrap
      <<< foldMap step
      $ props
  objDoc = unwrap (nameCol <> Columns (text " ") <> propsCol)

pprint = render <<< cata alg
  where
  alg :: Algebra (TsType FullyQualifiedName) Doc
  alg TsAny = text "any"
  alg (TsApplication fqn arr) = hcat $ [ text $ fqnToString fqn, text "<" ] <> Array.fromFoldable arr <> [ text ">" ]
  alg (TsArray t) = hcat [ text "[", t, text "]" ]
  alg TsBoolean = text "boolean"

  -- | FIXME: Print constructors
  alg (TsClass { bases, constructors, props }) = case bases of
    [] -> pprintProps (text "class") props
    _ -> do
      let
        header = hcat $ [ text "class(" ] <> bases <> [ text ")" ]
      pprintProps header props

  alg (TsInterface { bases, props }) = case bases of
    [] -> pprintProps (text "interface") props
    _ -> do
      let
        header = hcat $ [ text "interface(" ] <> bases <> [ text ")" ]
      pprintProps header props
  alg (TsFunction parameters returnType) = do
    let
      arg { name, type: t } = joinWithDoc (text ":") [ text name, t ] (Just 80)
    hcat
      [ text "function "
      , text "("
      , joinWithDoc (text ", ") (map arg parameters) (Just 30)
      , text "): "
      , returnType
      ]
  alg (TsIntersection arr) = joinWithDoc (text " & ") arr (Just 80)
  alg TsNull = text "null"
  alg TsNumber = text "number"
  -- FIXME
  alg (TsObject props) = pprintProps (text "object") props
  -- | length props == 0 = text "{}"
  alg TsString = text "string"
  alg (TsTuple ts) =
    hcat [ text "(", joinWithDoc (text ", ") ts (Just 80), text ")" ]
  alg (TsBooleanLiteral b) = text $ "@" <> show b
  alg (TsStringLiteral s) = text $ "@" <> show s
  alg (TsNumberLiteral n) = text $ "@" <> show n
  alg TsUndefined = text "undefined"
  alg (TsUnion ts) = joinWithDoc (text " | ") ts (Just 80)
  alg (TsMerge ts) = joinWithDoc (text " `merge` ") ts (Just 80)
  alg (TsUnknown _) = text "unknown "
  alg (TsParametric body params) = text "pprint.FIXME: parametric"
  alg (TsParameter _) = text "FIXME: param"
  alg (TsTypeRef fqn) = text (fqnToString fqn)

-- pprintTypeName :: TsType String -> String
-- pprintTypeName TsAny = "any"
-- pprintTypeName (TsArray t) = "[ " <> t <> "]"
-- pprintTypeName (TsBoolean) = "boolean"
-- pprintTypeName (TsFunction r) = "function"
-- pprintTypeName (TsIntersection t1 t2) = "intersection: " <> t1 <> " & " <> t2
-- pprintTypeName TsNull = "null"
-- pprintTypeName Number = "number"
-- pprintTypeName (NumberLiteral n) = "@" <> show n
-- pprintTypeName (Object _ ms) | length ms == 0 = "{}"
-- pprintTypeName (Object fqn _) = "<" <> fqn <> ">"
-- pprintTypeName String = "string"
-- pprintTypeName (Union ts) = String.joinWith " | " ts
-- pprintTypeName (Tuple ts) = "(" <> String.joinWith ", " ts <> ")"
-- pprintTypeName (BooleanLiteral b) = "@" <> show b
-- pprintTypeName (StringLiteral s) = "@" <> s
-- pprintTypeName Undefined = "undefined"
-- pprintTypeName (Unknown s) = "unkown:" <> s
-- pprintTypeName Void = "void"
