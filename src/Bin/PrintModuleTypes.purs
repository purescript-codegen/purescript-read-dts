module Bin.PrintModuleTypes where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Undefined.NoProblem (opt)
import Data.Undefined.NoProblem as NoProblem
import Debug (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import JS.Object.Immutable (newtypedProps, onNewtypedProps)
import JS.Object.Immutable as Immutable
import JS.Unsafe.Stringify (unsafeStringify)
import JS.Variant.Untagged as Untagged
import Node.Path (FilePath)
import Options.Applicative ((<**>))
import Options.Applicative as O
import ReadDTS.AST (TypeRepr(..), printTsTypeConstructorName)
import ReadDTS.AST (types) as ReadDTS.AST
import ReadDTS.AST.Printer (pprint)
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)
import TypeScript.Compiler.Parser (SourceCode(..))
import TypeScript.Compiler.Program (createCompilerHost, createProgram, getPreEmitDiagnostics)
import TypeScript.Compiler.Types (FullyQualifiedName(..), Program, moduleKind, moduleResolutionKind, scriptTarget)
import TypeScript.Compiler.Types.Nodes as Nodes
import TypeScript.Testing.BoundedCompilerHost as BoundedCompilerHost
import TypeScript.Testing.InMemory as InMemory
import TypeScript.Testing.Types (InMemoryFile(..))
import Unsafe.Coerce (unsafeCoerce)

data RootModule
  = RootFilePath FilePath
  | RootFileContent FilePath String
derive instance Generic RootModule _

instance Show RootModule where
  show = genericShow

compile
  :: RootModule
  -> Effect Program
compile rootModule = do
  let
    compilerOpts =
      { module: opt moduleKind."ES2015"
      , moduleResolution: opt moduleResolutionKind."NodeNext"
      , noEmitOnError: opt true
      , noImplicitAny: opt true
      , rootDirs: []
      , target: opt scriptTarget."ES5"
      , strictNullChecks: false
      , allowSyntheticDefaultImports: opt true
      }
  compilerHost <- createCompilerHost compilerOpts <#> BoundedCompilerHost.fromCompilerHost
  conf <- case rootModule of
    RootFilePath filePath -> pure
      { host: compilerHost
      , root: filePath
      }
    RootFileContent filePath fileContent -> do
      let
        inMemoryRoot = InMemoryFile
          { path: filePath
          , source: SourceCode fileContent
          }
      compilerHost' <- InMemory.boundedCompilerHost
        { files: [inMemoryRoot]
        , defaultLib: Nothing
        , subhost: Just $ compilerHost
        }
      pure { host: compilerHost', root: filePath }

  createProgram [conf.root] compilerOpts (Just $ BoundedCompilerHost.toCompilerHost conf.host)

data TypeTransformation
  = ApplyDefaultParams
  | Expand
  | ExpandOneLayer
  | ExpandTwoLayers
  | ExpandRecursively


type Options =
  { typeTransformation :: Maybe TypeTransformation
  , modulePath :: FilePath
  -- , variables :: Maybe (NonEmptyArray String)
  , "type" :: String
  }

options :: O.Parser Options
options = ado
  possibleTypeTransformation <- do
    let
      applyOpt = O.flag' ApplyDefaultParams do
        O.long "apply-default-params"
          <> O.short 'a'
          <> O.help "Compute the final type by applying it to an empty type params (i.e. `type Foo = Bar<>`)"

      expandOpt = O.flag' Expand do
        O.long "expand-type"
          <> O.short 'e'
          <> O.help "Try to expand the type by using the trick described here:  https://stackoverflow.com/a/57683652"

      expandOneLayerOpt = O.flag' ExpandOneLayer do
        O.long "expand-type-one-layer"
          <> O.short 'l'
          <> O.help "Try to expand the type by using the trick described here:  https://stackoverflow.com/a/57683652"

      expandTwoLayersOpt = O.flag' ExpandTwoLayers do
        O.long "expand-type-two-layers"
          <> O.short 't'
          <> O.help "Try to expand the type by using the trick described here:  https://stackoverflow.com/a/57683652"

      expandRecursivelyOpt = O.flag' ExpandRecursively do
        O.long "expand-type-recursively"
          <> O.short 'r'
          <> O.help "Try to expand the type by using the trick described here:  https://stackoverflow.com/a/57683652"
    Just <$> applyOpt <|> Just <$> expandOpt <|> Just <$> expandRecursivelyOpt <|> Just <$> expandOneLayerOpt <|> Just <$> expandTwoLayersOpt <|> pure Nothing

  modulePath <- O.option O.str do
    O.long "module-path"
      <> O.metavar "module"
  typ <- O.option O.str do
    O.long "type"
      <> O.short 't'
      <> O.metavar "type"
      <> O.help "A type name which we want to exapnd"
  in
    { typeTransformation: possibleTypeTransformation
    , modulePath
    , "type": typ
    }

main :: Effect Unit
main = do
  opts <- liftEffect $ O.execParser $ O.info (options <**> O.helper) $ fold
    [ O.fullDesc
    , O.progDesc "Print a typescript type (possibly expanded)"
    -- , O.header "read-dts - a tool for reading TypeScript definitions"
    ]

  let
    moduleName = fromMaybe opts.modulePath $ do
      let
        noDTs = String.stripSuffix (String.Pattern ".d.ts") opts.modulePath
        noTs = String.stripSuffix (String.Pattern ".ts") opts.modulePath
      noDTs <|> noTs

    { rootModule, fqn } = case opts.typeTransformation of
      Just expand -> do
        let
          rootModule = RootFileContent "root.ts" $ String.joinWith "\n"
            [ "import { " <> opts."type" <> " } from \"./" <> moduleName <> "\""
            , "type ApplyDefaultParams<T> = T<>;"
            , "type Expand<T> = extends infer O ? { [K in keyof O]: O[K] } : never;"
            , "type ExpandOneLayer<T> = T extends object"
            , "  ? T extends infer O ? { [K in keyof O]: Expand<O[K]> } : never"
            , "  : T;"
            , "type ExpandTwoLayers<T> = T extends object"
            , "  ? T extends infer O ? { [K in keyof O]: ExpandOneLayer<O[K]> } : never"
            , "  : T;"
            , "type ExpandRecursively<T> = T extends object"
            , "  ? T extends infer O ? { [K in keyof O]: ExpandRecursively<O[K]> } : never"
            , "  : T;"
            , case expand of
                ApplyDefaultParams ->
                  "export type Expanded = ApplyDefaultParams<" <> opts."type" <> ">"
                Expand ->
                  "export type Expanded = Expand<" <> opts."type" <> ">"
                ExpandOneLayer ->
                  "export type Expanded = ExpandOneLayer<" <> opts."type" <> ">"
                ExpandTwoLayers ->
                  "export type Expanded = ExpandTwoLayers<" <> opts."type" <> ">"
                ExpandRecursively ->
                  "export type Expanded = ExpandRecursively<" <> opts."type" <> ">"
            ]
        { rootModule, fqn: FullyQualifiedName "\"root\".Expanded" }
      Nothing -> do
        { rootModule: RootFilePath opts.modulePath
        , fqn: FullyQualifiedName $ "\"" <> moduleName <> "\"." <> opts."type"
        }

  program <- liftEffect $ compile rootModule

  --  newtype DiagnosticMessageChain = DiagnosticMessageChain
  --    ( Immutable
  --      ( messageText :: String
  --      , category :: DiagnosticCategory
  --      , code :: Int
  --      , next :: Opt (Array DiagnosticMessageChain)
  --      )
  --    )
  --  
  --  type MessageText = Untagged (StringRow + (diagnosticMessageChain :: DiagnosticMessageChain))
  case getPreEmitDiagnostics program of
    [] -> pure unit
    diagnostics -> do
      log $ "Compiler outputed some debug diagnostics"
      for_ diagnostics \diagnostic -> do
        let
          { messageText, file, start } = newtypedProps diagnostic
        let
          file' = NoProblem.toMaybe file
          start' = NoProblem.toMaybe start
          toMessage = Untagged.case_ unsafeStringify
            # Untagged.on' (Proxy :: Proxy "diagnosticMessageChain") (Just <<< unsafeCoerce) do
                \diagnosticMessageChain -> diagnosticMessageChain `onNewtypedProps` _.messageText
            # Untagged.onPrimitive (Proxy :: Proxy "string") identity
          message = toMessage messageText
        log $ "  " <> message
        for_ file' \fileNode -> do
          log $ "  File: " <> fileNode `Nodes.onProps` _.fileName
        for_ start' \startingPosition -> do
          log $ "  Start: " <> show startingPosition



  case ReadDTS.AST.types program of
    Right types -> do
      case Map.lookup fqn types of
        Just (TypeRepr t) -> do
          log $ pprint t
          -- log $ formatTypeFlags' t
          -- log $ typeToString checker t
          traceM $ printTsTypeConstructorName $ unwrap t
        Nothing -> do
          traceM $ "Type not found: " <> unwrap fqn
    Left err -> do
      traceM $ "FAILURE: " <> show err
      -- failure $ "FAILURE: " <> show err
