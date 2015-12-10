{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module TypeErrors where

import JavaUtils
import PrettyUtils
import Src
import SrcLoc

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen

data TypeError
  = General Doc
  | DuplicateParam Name
  | ExpectJClass
  | IndexTooLarge
  | TypeMismatch Type Type
  | KindMismatch Kind Kind Type
  | MissingTyAscription Name
  | NotInScope Name
  | ProjectionOfNonProduct
  | NotWellKinded Type
  | NotMember Name Type
  | NotAFunction Type

  -- Java-specific type errors
  | NoSuchClass       ClassName
  | NoSuchConstructor ClassName [ClassName]
  | NoSuchMethod      (JReceiver ClassName) MethodName [ClassName]
  | NoSuchField       (JReceiver ClassName) FieldName
  | ImportFail        ModuleName
  deriving (Show)

type LTypeErrorExpr = Located (TypeError, Maybe ReadExpr)

-- instance Error LTypeErrorExpr where

prettyTypeError :: FilePath -> LTypeErrorExpr -> Doc
prettyTypeError filePath (L loc (err, expr)) =
    case expr of
      Nothing -> pretty loc <> pretty err
      Just expr -> text filePath <> colon <> pretty loc <> pretty err <$> text "In the expression" <> colon <+> pretty expr

instance Pretty TypeError where
  pretty (General doc)      = doc
  pretty (NotInScope x)  = code (text x) <+> text "is not in scope"
  pretty (DuplicateParam ident) = text "duplicate parameter" <+> code (text ident)
  pretty (NotWellKinded t)  = code (pretty t) <+> text "is not well-kinded"
  pretty (KindMismatch expected found t) =
    text "kind mismatch" <> colon <$>
    indent 2 (text "expected" <+> code (pretty expected) <> comma <$>
              text "   found" <+> code (pretty found)) <$>
    text "in the type" <> colon <+> pretty t

  pretty (TypeMismatch expected found) =
    text "type mismatch" <> colon <$>
    indent 2 (text "expected" <+> code (pretty expected) <> comma <$>
              text "   found" <+> code (pretty found))
  pretty (NoSuchClass c)  = text "no such class:" <+> code (text c)
  pretty (NotMember x t)  = code (text x) <+> text "is not a member of the type" <+> code (pretty t)
  pretty (NotAFunction t) = code (pretty t) <+> text "is not a function; it cannot be applied"

  -- Java-specific type errors
  pretty (NoSuchMethod (NonStatic c) m cs) =
    text "no such method" <+> code (text m) <+>
    text "on" <+> code (pretty (JClass c)) <+>
    text "with parameters of type" <+> commas (map (code . pretty . JClass) cs)
  pretty (NoSuchMethod (Static c) m cs) =
    text "no such static method" <+> code (text m) <+>
    text "on" <+> code (pretty (JClass c)) <+>
    text "with parameters of type" <+> commas (map (code . pretty . JClass) cs)

  pretty (NoSuchField (NonStatic c) f) =
    text "no such field" <+> code (text f) <+>
    text "on" <+> code (pretty (JClass c))
  pretty (NoSuchField (Static c) f) =
    text "no such static field" <+> code (text f) <+>
    text "on" <+> code (pretty (JClass c))

  pretty (MissingTyAscription ident) =
    text "recursive definition" <+> code (text ident) <+>
    text "needs type ascription for the right-hand side"

  pretty e = text (show e)

-- instance Error TypeError where
  -- strMsg
