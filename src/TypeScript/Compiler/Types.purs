module TypeScript.Compiler.Types where

import Data.Undefined.NoProblem (Opt)
import Prelude (class Eq, class Ord, class Show)

foreign import data Program :: Type

-- | You can find partial translation (with `this` binding) into just
-- | record of this opaque type in the `ReadDTS.TypeScript.Testing` module.
-- | Should we just move that API here?
foreign import data CompilerHost :: Type

foreign import data TypeChecker :: Type

foreign import data ScriptTarget :: Type

foreign import scriptTarget ::
  { "ES3" :: ScriptTarget
  , "ES5" :: ScriptTarget
  , "ES2015" :: ScriptTarget
  , "ES2016" :: ScriptTarget
  , "ES2017" :: ScriptTarget
  , "ES2018" :: ScriptTarget
  , "ES2019" :: ScriptTarget
  , "ES2020" :: ScriptTarget
  , "ESNext" :: ScriptTarget
  , "JSON" :: ScriptTarget
  , "Latest" :: ScriptTarget
  }

foreign import data ModuleKind :: Type

foreign import moduleKind ::
  { "None" :: ModuleKind
  , "CommonJS" :: ModuleKind
  , "AMD" :: ModuleKind
  , "UMD" :: ModuleKind
  , "System" :: ModuleKind
  , "ES2015" :: ModuleKind
  , "ESNext" :: ModuleKind
  }

type CompilerOptions =
  { "module" :: Opt ModuleKind
  , strictNullChecks :: Boolean
  , target :: Opt ScriptTarget
  }

-- | Opaque type for string like identifiers nodes.
foreign import data Identifier :: Type

foreign import data PropertyName :: Type

foreign import data TypeReference :: Type

-- | `Node` carries information about possible accessors.
-- |
-- | Few notes:
-- |
-- | * We treat `Node` values in this binding as immutable.
-- | * We need this opaque type around possible records because
-- | we don't want to expose its internal `kind` field which
-- | is used for casting etc.
-- | * We need this opaque type because we should disallow
-- | creation of nodes which are partial (i.e. missing `kind` field)
-- | or inconsistent (when `kind` field is incosistent with the
-- | actual shape of the node record).
-- | * Nodes don't have methods. If they have we will not
-- | expose them as methods shoudl be bounded to the js object.
foreign import data Node :: Row Type -> Type

-- | FIXME: Currently we use this wrapper to tag some specific
-- | nodes which we feed back into the FFI so we won't mess
-- | the types of the nodes by only structurally typing
-- | them with rows.
-- | This is going to be private constructor
newtype Node' (tag :: Symbol) i = Node' (Node i)

unNode' :: forall tag i. Node' tag i -> Node i
unNode' (Node' n) = n

-- | The interface in the `compiler/types.ts` is called
-- | `Type`. I've found that this makes some PS errors
-- | hard to read. `Type` collide with PS builtin type name.
foreign import data Typ :: Row Type -> Type
foreign import data TypeFlags :: Type
foreign import data Symbol_ :: Type

newtype FullyQualifiedName = FullyQualifiedName String

derive instance Eq FullyQualifiedName
derive instance Ord FullyQualifiedName
derive newtype instance Show FullyQualifiedName

