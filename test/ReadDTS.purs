module Test.ReadDTS where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (foldMap, for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.String (joinWith) as String
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Global.Unsafe (unsafeStringify)
import ReadDTS (Declarations, File, FullyQualifiedName(..), OnDeclaration, OnType, TsDeclaration, Visit, fqnToString)
import ReadDTS (FullyQualifiedName(..), defaults, readDTS) as ReadDTS
import ReadDTS.AST (ApplicationRef(..), TypeConstructor(..), TypeNode(..)) as AST
import ReadDTS.AST (visit) as ReadDTS.AST
import Test.Unit (failure, suite, test) as Test
import Test.Unit.Assert (equal) as Assert
import Unsafe.Coerce (unsafeCoerce)

dts source =
  { path: "inMemory.d.ts"
  , source: Just source
  }

dts' = dts <<< String.joinWith "\n"

readTopLevel ∷ File → Aff (Array (AST.TypeConstructor Unit))
readTopLevel file = do
  liftEffect (ReadDTS.readDTS ReadDTS.defaults ReadDTS.AST.visit file) >>= case _ of
    Right { topLevel } → pure $ (map $ const unit) <$> topLevel
    Left errs → do
      Test.failure ("Compilation failed: " <> String.joinWith "\n" errs)
      pure []

suite = do
  Test.suite "Exposed symbols" do
    Test.test "Single declaration" do
      let
        file = dts "export interface SomeInterface {}"
        expected = AST.Interface
          { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"inMemory\".SomeInterface"
          , name: "SomeInterface"
          , properties: []
          , typeParameters: []
          }
      readTopLevel file >>= Assert.equal [ expected ]
    Test.suite "Declaration merge" $ do
      Test.test "two monomorhic interfaces" do
        let
          file = dts'
            [ "export interface SomeInterface { y: string }"
            , "export interface SomeInterface { x: number }"
            ]
          expected = AST.Interface
            { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"inMemory\".SomeInterface"
            , name: "SomeInterface"
            , properties:
                [{ name: "x", optional: false, type: AST.String }
                ,{ name: "y", optional: false, type: AST.Number }
                ]
            , typeParameters: []
            }
        readTopLevel file >>= Assert.equal [ expected ]

      Test.test "two polymorphic interfaces" do
        let
          file = dts'
            [ "export interface SomeInterface<x, z> { y: x }"
            , "export interface SomeInterface<x, z> { x: x }"
            ]
          expected = AST.Interface
            { fullyQualifiedName: ReadDTS.FullyQualifiedName "\"inMemory\".SomeInterface"
            , name: "SomeInterface"
            , properties:
                [ { name: "y"
                  , optional: false
                  , type: (AST.TypeParameter { default: Nothing, name: "x" })
                  }
                , { name: "x"
                  , optional: false
                  , type: (AST.TypeParameter { default: Nothing, name: "x" })
                  }
                ]
            , typeParameters:
                [ { default: Nothing
                  , name: "x"
                  }
                , { default: Nothing
                  , name: "z"
                  }
                ]
            }
        readTopLevel file >>= Assert.equal [ expected ]


-- type TsDeclarationRef =
--   { fullyQualifiedName ∷ FullyQualifiedName
--   , tsDeclaration ∷ TsDeclaration
--   }
-- 
-- type TypeRepr =
--   { repr ∷ String
--   , tsDeclarations ∷ Array TsDeclarationRef
--   }
-- 
-- newtype DeclarationRepr = DeclarationRepr
--   { fullyQualifiedName ∷ Maybe FullyQualifiedName
--   , repr ∷ String
--   , tsDeclarations ∷ Array TsDeclarationRef
--   }
-- derive instance newtypeDeclarationRepr ∷ Newtype DeclarationRepr _
-- 
-- stringOnDeclaration ∷ OnDeclaration DeclarationRepr TypeRepr
-- stringOnDeclaration =
--   -- | FIX: I'm handling `function` as unknown for now
--   -- | Provide proper handling.
--   { class_: \i → DeclarationRepr
--       { fullyQualifiedName: Just i.fullyQualifiedName
--       , repr: serClass i
--       , tsDeclarations:
--           (foldMap (foldMap _.tsDeclarations <<< _.default)) i.typeParameters
--           <> foldMap _.type.tsDeclarations i.properties
--       }
--   , interface: \i → DeclarationRepr
--       { fullyQualifiedName: Just i.fullyQualifiedName
--       , repr: serInterface i
--       , tsDeclarations:
--           (foldMap (foldMap _.tsDeclarations <<< _.default)) i.typeParameters
--           <> foldMap _.type.tsDeclarations i.properties
--       }
--   -- | TODO: Fix printing of module here
--   , module_: \i → DeclarationRepr
--       { fullyQualifiedName: Nothing -- Just i.fullyQualifiedName
--       , repr: serModule i
--       , tsDeclarations: []
--       }
--   -- | It seems that type aliases don't introduce names so they don't
--   -- | have fullyQualifiedName... but of course I can be wrong.
--   , typeAlias: \r@{ type: t, typeParameters } → DeclarationRepr
--       { fullyQualifiedName: Nothing
--       , repr: serTypeAlias r
--       , tsDeclarations: t.tsDeclarations <>
--           (foldMap (foldMap _.tsDeclarations <<< _.default)) typeParameters
--       }
--   , unknown: \u → DeclarationRepr
--       { repr: serUnknown u
--       , fullyQualifiedName: u.fullyQualifiedName
--       , tsDeclarations: []
--       }
--   }
-- 
-- serUnknown ∷ { fullyQualifiedName ∷ Maybe FullyQualifiedName, msg ∷ String } → String
-- serUnknown r = "unkownDeclaration " <> show r.fullyQualifiedName <> ": " <> r.msg
-- 
-- serTypeAlias r
--   = "typeAlias "
--   <> r.name
--   <> " <" <> joinWith ", " (map (unsafeTsStringToString <<< _.name) r.typeParameters) <> "> : "
--   <> r.type.repr
-- 
-- serModule { fullyQualifiedName }
--   = "module "
--   <> show fullyQualifiedName
-- 
-- serClass { name, fullyQualifiedName, properties, typeParameters }
--   = "class "
--   <> show fullyQualifiedName
--   <> " <" <> joinWith ", " (map (\{ name, default } → unsafeTsStringToString name <> " = " <> foldMap _.repr default) typeParameters) <> "> : \n\t"
--   <> joinWith ";\n\t" (map onMember properties)
--   where
--     onMember r = joinWith " " [r.name, if r.optional then "?:" else ":", r.type.repr ]
-- 
-- serInterface { name, fullyQualifiedName, properties, typeParameters }
--   = "interface "
--   <> show fullyQualifiedName
--   <> " <" <> joinWith ", " (map (\{ name, default } → unsafeTsStringToString name <> " = " <> foldMap _.repr default) typeParameters) <> "> : \n\t"
--   <> joinWith ";\n\t" (map onMember properties)
--   where
--     onMember r = joinWith " " [r.name, if r.optional then "?:" else ":", r.type.repr ]
-- 
-- noDeclarations ∷ String → TypeRepr
-- noDeclarations repr = { repr, tsDeclarations: [] }
-- 
-- stringOnType ∷ OnType DeclarationRepr TypeRepr
-- stringOnType =
--   { anonymousObject: \{ properties: props, fullyQualifiedName: fqn } →
--       let
--         onMember r = joinWith " " [r.name, if r.optional then "?:" else ":", r.type.repr ]
--       in
--         { repr: "<" <> fqnToString fqn <> ">" <> "{" <> (joinWith " , " (map onMember props)) <> "}"
--         , tsDeclarations: foldMap _.type.tsDeclarations props
--         }
--   , array: \t → { repr: "Array: " <> t.repr, tsDeclarations: t.tsDeclarations }
--   , function: \u →
--       { repr: "function TO BE FIXED"
--       , tsDeclarations: []
--       }
--   , intersection: \ts →
--       { repr: append "intersection: " <<< joinWith " & " <<< map _.repr $ ts
--       , tsDeclarations: foldMap _.tsDeclarations ts
--       }
--   , primitive: noDeclarations <<< show
--   , tuple: \ts →
--       { repr: "(" <> (joinWith ", " $ map _.repr ts) <> ")"
--       , tsDeclarations: foldMap _.tsDeclarations ts
--       }
--   , typeParameter: case _ of
--       { default: Nothing, name } → noDeclarations $ unsafeStringify name
--       { default: Just d, name } →
--           { repr: unsafeStringify name <> " = " <> d.repr
--           , tsDeclarations: d.tsDeclarations
--           }
--   , typeReference: \{ fullyQualifiedName, ref, typeArguments } →
--       { repr: "typeReference: " <> show fullyQualifiedName <> "<" <> joinWith ", " (map _.repr typeArguments) <> ">"
--       , tsDeclarations: { fullyQualifiedName, tsDeclaration: ref } :foldMap _.tsDeclarations typeArguments
--       }
--   , booleanLiteral: \b →
--       { repr: "booleanLiteral: " <> show b
--       , tsDeclarations: []
--       }
--   , numberLiteral: \n →
--       { repr: "numberLiteral: " <> show n
--       , tsDeclarations: []
--       }
--   , stringLiteral: \s →
--       { repr: "stringLiteral: " <> show s
--       , tsDeclarations: []
--       }
--   , union: \ts →
--       { repr: append "union: " <<< joinWith " | " <<< map _.repr $ ts
--       , tsDeclarations: foldMap _.tsDeclarations ts
--       }
--   , unknown: noDeclarations <<< append "unknown: " <<< show
--   }
-- 
-- -- file ∷ ReadDTS.File
-- -- fileName = "test/simple.d.ts"
-- -- file =
-- --   { path: "node_modules/@material-ui/core/Fab/Fab.d.ts"
-- --   , source: Nothing
-- --   }
-- -- source = """
-- -- // import { FabProps } from "@material-ui/core/Fab/Fab";
-- -- 
-- -- // export type FabInstance = FabProps;
-- -- 
-- -- export interface New<x = number> {
-- --   z?: 8
-- --   // t: {} | string | 8 | undefined | null
-- --   // s: null
-- --   // u: undefined
-- --   // x: x | boolean
-- --   // y: true | 8
-- --   // z: boolean | null
-- -- }
-- -- 
-- -- export type X = 8 | null | string | boolean;
-- -- 
-- -- export type Y = {};
-- -- 
-- -- // export type NewInstance = New;
-- -- """
-- 
-- source = """
--   // declare module 'system' {
--   interface MemoryPressureMonitor
--     extends EventTarget<{
--       memorypressurechange: Event;
--     }> {
--     onmemorypressurechange: (event: Event) ⇒ void;
--     readonly pressure: 'normal' | 'high' | 'critical';
--   }
--   interface MemoryUsage {
--     readonly peak: number;
--     readonly total: number;
--     readonly used: number;
--   }
--   interface Memory {
--     readonly js: MemoryUsage;
--     readonly monitor: MemoryPressureMonitor;
--     readonly native: MemoryUsage;
--   }
--   const memory: Memory;
--   function launchApp(uuid: string, launchArguments?: any): void;
--   // }
-- """
-- 
-- file =
--   { path: "test/test.module.d.ts"
--   , source: Just source
--   }
-- 
-- 
-- -- | XXX:
-- -- | * These test are currently not run as a part of test suite.
-- -- | * Turn this into a proper suite and call this from test/Main.purs
-- main ∷ Effect Unit
-- main = do
--   let
--     compilerOptions = ReadDTS.defaults { strictNullChecks = true }
--   --   constructors = { onDeclaration: stringOnDeclaration, onTypeNode: stringOnType } 
-- 
--   -- readDTS compilerOptions constructors file >>= case _ of
--   --   Right { topLevel, readDeclaration } → do
--   --     pure unit
--   --     -- for_ topLevel \(DeclarationRepr r) → do
--   --     --    log r.repr
--   --     --    log "\n"
-- 
--   --     -- -- | Single pass of loading... We should test exhaustive loading too.
--   --     -- let
--   --     --   initCache = Map.fromFoldable <<< catMaybes <<< map case _ of
--   --     --     d@(DeclarationRepr { fullyQualifiedName: Just fullyQualifiedName }) → Just (Tuple fullyQualifiedName d)
--   --     --     otherwise → Nothing
--   --     --   cache = initCache topLevel
-- 
--   --     --   step c { fullyQualifiedName, tsDeclaration } = case fullyQualifiedName `Map.lookup` c of
--   --     --     Nothing → readDeclaration tsDeclaration >>= flip (Map.insert fullyQualifiedName) c >>> pure
--   --     --     Just _ → pure c
-- 
--   --     -- log "Single pass of loading declarations...\n\n"
-- 
--   --     -- cache' ← foldM step cache (foldMap (unwrap >>> _.tsDeclarations) topLevel)
-- 
--   --     -- log "Collected declarations:\n\n"
-- 
--   --     -- for_ cache' \(DeclarationRepr r) → do
--   --     --    log r.repr
--   --     --    log "\n"
--   --   Left err → do
--   --     log "Ts compiler reported errors related to given source file:"
--   --     for_ err log
-- 
--   AST.build compilerOptions file >>= case _ of
--     Right (result ∷ Array (TypeConstructor Application')) → do
--       for_ result $ flip instantiate [] >>> runExcept >>> case _ of
--         Right t → do
--           log $ Instantiation.Pretty.pprint t
--           log $ show $ isObjectLiteral t
--         Left e → log $ "Instantiation error:" <> e
--     Left err → do
--       log "Ts compiler reported errors related to given source file:"
--       for_ err log
-- 
--   Test.runTest Test.ReadDTS.Instantiation.suite
