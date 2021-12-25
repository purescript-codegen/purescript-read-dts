import * as ts from "typescript";

export const getChildren = (node: ts.Node): ts.Node[] => node.getChildren();

