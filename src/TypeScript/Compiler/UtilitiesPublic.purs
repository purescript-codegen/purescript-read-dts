module TypeScript.Compiler.UtilitiesPublic where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Prim.Row (class Cons, class Union) as Row
import TypeScript.Compiler.Types (Node)

idText ::
  forall i tag tagRow tagRow_.
  Row.Cons tag Void () tagRow =>
  Row.Union tagRow tagRow_ ("Identifier" :: Void, "PrivateIdentifier" :: Void) =>
  Node tag i ->
  String
idText = runFn1 unsafeIdTextImpl


foreign import unsafeIdTextImpl :: forall tag i. Fn1 (Node tag i) String

