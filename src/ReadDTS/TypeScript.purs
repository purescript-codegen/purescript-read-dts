module ReadDTS.TypeScript where

import Prelude

import Control.Alt ((<|>))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe, fromJust)
import Data.Undefined.NoProblem (toMaybe) as NoProblem
import Debug (traceM)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons, class Union) as Row
import TypeScript.Compiler.Checker (getFullyQualifiedName, getSymbolAtLocation)
import TypeScript.Compiler.Factory.NodeTests (asClassDeclaration, asFunctionDeclaration, asInterfaceDeclaration, asTypeAliasDeclaration)
import TypeScript.Compiler.Factory.NodeTests as Nodes
import TypeScript.Compiler.Types (FullyQualifiedName, Node, TypeChecker)
import TypeScript.Compiler.Types.Nodes (DeclarationStatement, interface) as Nodes
import TypeScript.Compiler.Types.Symbol (getName) as Symbol
import TypeScript.Debug (formatTypeFlags')

isNodeExported :: forall l k. TypeChecker -> Node l k -> Boolean
isNodeExported = runFn2 isNodeExportedImpl

foreign import isNodeExportedImpl :: forall l k. Fn2 TypeChecker (Node l k) Boolean

toDeclarationStatement
  :: forall i tag tagRow tagRow_
   . Row.Cons tag Void () tagRow
  => Row.Union
       tagRow
       tagRow_
       ( "ClassDeclaration" :: Void
       , "ClassLikeDeclaration" :: Void
       , "FunctionDeclaration" :: Void
       , "InterfaceDeclaration" :: Void
       , "TypeAliasDeclaration" :: Void
       )
  => Node tag i
  -> Nodes.DeclarationStatement
toDeclarationStatement = toDeclarationStatementImpl

-- | FIXME: don't export
foreign import toDeclarationStatementImpl :: forall l r. Node l r -> Nodes.DeclarationStatement


-- These are all declaration statements but `VariableStatement`/`VariableDeclaration` is something else...
-- export interface ExportAssignment extends DeclarationStatement, JSDocContainer {
-- export interface FunctionDeclaration extends FunctionLikeDeclarationBase, DeclarationStatement, LocalsContainer {
-- export interface MissingDeclaration extends DeclarationStatement {
-- export interface ClassDeclaration extends ClassLikeDeclarationBase, DeclarationStatement {
-- export interface InterfaceDeclaration extends DeclarationStatement, JSDocContainer {
-- export interface TypeAliasDeclaration extends DeclarationStatement, JSDocContainer, LocalsContainer {
-- export interface EnumDeclaration extends DeclarationStatement, JSDocContainer {
-- export interface ModuleDeclaration extends DeclarationStatement, JSDocContainer, LocalsContainer {
-- export interface ImportEqualsDeclaration extends DeclarationStatement, JSDocContainer {
-- export interface NamespaceExportDeclaration extends DeclarationStatement, JSDocContainer {
-- export interface ExportDeclaration extends DeclarationStatement, JSDocContainer {
-- export interface ExportAssignment extends DeclarationStatement, JSDocContainer {

asDeclarationStatement :: forall l r. Node l r -> Maybe Nodes.DeclarationStatement
asDeclarationStatement node =
  (toDeclarationStatement <$> asTypeAliasDeclaration node)
    <|> (toDeclarationStatement <$> asInterfaceDeclaration node)
    <|> (toDeclarationStatement <$> asClassDeclaration node)
    <|> (toDeclarationStatement <$> asFunctionDeclaration node)

getDeclarationStatementFqn :: TypeChecker -> Nodes.DeclarationStatement -> Maybe FullyQualifiedName
getDeclarationStatementFqn checker node = do
  symbol <-
    (getSymbolAtLocation checker <<< _.name <<< Nodes.interface =<< asTypeAliasDeclaration node)
      <|> (getSymbolAtLocation checker =<< asInterfaceDeclaration node)
      <|> (getSymbolAtLocation checker <<< _.name <<< Nodes.interface =<< asInterfaceDeclaration node)
      <|> (getSymbolAtLocation checker =<< asClassDeclaration node)
      <|> (getSymbolAtLocation checker =<< NoProblem.toMaybe <<< _.name <<< Nodes.interface =<< asClassDeclaration node)
      <|> (getSymbolAtLocation checker =<< asFunctionDeclaration node)
      -- FIXME: debugging
      <|> (getSymbolAtLocation checker node)
  pure $ getFullyQualifiedName checker symbol

