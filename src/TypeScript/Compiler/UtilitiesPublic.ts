import * as ts from "typescript";

export const unsafeIdTextImpl = (identifierOrPrivateName: ts.Identifier | ts.PrivateIdentifier): string => ts.idText(identifierOrPrivateName);

