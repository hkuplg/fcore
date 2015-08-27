{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module TypeErrors where

import Src
import SrcLoc
import JavaUtils
import PrettyUtils

import Text.PrettyPrint.ANSI.Leijen
import Control.Monad.Error

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
  deriving (Show)

type LTypeErrorExpr = Located (TypeError, Maybe ReadExpr)

instance Error LTypeErrorExpr where

instance Pretty LTypeErrorExpr where
    pretty (L loc (err, expr)) =
        case expr of
          Nothing -> pretty loc <> pretty err
          Just expr -> pretty loc <> pretty err <$> text "In the expression" <> colon <+> pretty expr

instance Pretty TypeError where
  pretty (General doc)      = prettyError <+> doc
  pretty (NotInScope x)  = prettyError <+> code (text x) <+> text "is not in scope"
  pretty (DuplicateParam ident) = prettyError <+> text "duplicate parameter" <+> code (text ident)
  pretty (NotWellKinded t)  = prettyError <+> code (pretty t) <+> text "is not well-kinded"
  pretty (KindMismatch expected found t) =
    prettyError <+> text "kind mismatch" <> colon <$>
    indent 2 (text "expected" <+> code (pretty expected) <> comma <$>
              text "   found" <+> code (pretty found)) <$>
    text "in the type" <> colon <+> pretty t

  pretty (TypeMismatch expected found) =
    prettyError <+> text "type mismatch" <> colon <$>
    indent 2 (text "expected" <+> code (pretty expected) <> comma <$>
              text "   found" <+> code (pretty found))
  pretty (NoSuchClass c)  = prettyError <+> text "no such class:" <+> code (text c)
  pretty (NotMember x t)  = prettyError <+> code (text x) <+> text "is not a member of the type" <+> code (pretty t)
  pretty (NotAFunction t) = prettyError <+> code (pretty t) <+> text "is not a function; it cannot be applied"

  -- Java-specific type errors
  pretty (NoSuchMethod (NonStatic c) m cs) =
    prettyError <+> text "no such method" <+> code (text m) <+>
    text "on" <+> code (pretty (JType (JClass c))) <+>
    text "with parameters of type" <+> commas (map (code . pretty . JType . JClass) cs)
  pretty (NoSuchMethod (Static c) m cs) =
    prettyError <+> text "no such static method" <+> code (text m) <+>
    text "on" <+> code (pretty (JType (JClass c))) <+>
    text "with parameters of type" <+> commas (map (code . pretty . JType . JClass) cs)

  pretty (NoSuchField (NonStatic c) f) =
    prettyError <+> text "no such field" <+> code (text f) <+>
    text "on" <+> code (pretty (JType (JClass c)))
  pretty (NoSuchField (Static c) f) =
    prettyError <+> text "no such static field" <+> code (text f) <+>
    text "on" <+> code (pretty (JType (JClass c)))

  pretty (MissingTyAscription ident) =
    prettyError <+> text "recursive definition" <+> code (text ident) <+>
    text "needs type ascription for the right-hand side"

  pretty e = prettyError <+> text (show e)

instance Error TypeError where
  -- strMsg
