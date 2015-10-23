module PrettyPrint (showExpr) where

import qualified Language.Java.Pretty (prettyPrint)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>), (<>), text, dot, colon)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Unbound.Generics.LocallyNameless


import qualified Src as S
import           Syntax


class Pretty p where
  ppr :: (Applicative m, LFresh m) => p -> m Doc

instance Pretty Expr where
  ppr (Var x) = return . text . show $ x
  ppr (App e es) = PP.parens <$> ((<+>) <$> ppr e <*> (ppr es))
  ppr (Lam bnd) = lunbind bnd $ \(delta, b) -> do
    delta' <- ppr delta
    b' <- ppr b
    return (PP.parens $ text "λ" <> delta' <+> dot <+> b')
  ppr (Star) = return $ PP.char '★'
  ppr (Pi bnd) = lunbind bnd $ \(delta, b) -> do
    let Cons bb = delta
    let ((x, Embed t), bb') = unrebind bb
    b' <- ppr b
    if (show x == "_" && isEmpty bb')
      then do
        t' <- ppr t
        return (PP.parens $ t' <+> text "→" <+> b')
      else do
        delta' <- ppr delta
        return (PP.parens $ text "Π" <> delta' <+> dot <+> b')
  ppr (Mu b) = lunbind b $ \((x, Embed t), e) -> do
    t' <- ppr t
    e' <- ppr e
    return (PP.parens $ text "μ" <+> (text . show $ x) <+> colon <+> t' <+> dot <+> e')
  ppr (F t e) = do
    e' <- ppr e
    t' <- ppr t
    return (text "cast↑" <> PP.brackets t' <+> e')
  ppr (U e) = (text "cast↓" <+>) <$> ppr e
  ppr (Let bnd) = lunbind bnd $ \((x, Embed e1), e2) -> do
    e1' <- ppr e1
    e2' <- ppr e2
    return (text "let" <+> (text . show $ x) <+> PP.equals <+> e1' <+> text "in" <+> e2')
  ppr (If g e1 e2) = do
    g' <- ppr g
    e1' <- ppr e1
    e2' <- ppr e2
    return
      (text "if" <+>
       PP.parens g' <+>
       text "then" <+>
       PP.parens e1' <+>
       text "else" <+>
       PP.parens e2')
  ppr Nat = return $ text "nat"
  ppr (Lit (S.Int n)) = return $ PP.integer n
  ppr (Lit (S.String s)) = return $ PP.dquotes (PP.string s)
  ppr (Lit (S.Bool b)) = return $ PP.bool b
  ppr (Lit (S.Char c)) = return $ PP.char c
  ppr (Lit (S.UnitLit)) = return $ text "()"
  ppr (PrimOp op e1 e2) = do
    e1' <- ppr e1
    e2' <- ppr e2
    return $ PP.parens (e1' <+> op' <+> e2')

    where
      op' = text (Language.Java.Pretty.prettyPrint java_op)
      java_op =
        case op of
          S.Arith op'   -> op'
          S.Compare op' -> op'
          S.Logic op'   -> op'

instance Pretty Operation where
  ppr Add = return . text $ "+"
  ppr Mult = return . text $ "*"
  ppr Sub = return . text $ "-"

instance Pretty Tele where
  ppr Empty = return PP.empty
  ppr (Cons bnd) = do
    t' <- ppr t
    bnd' <- ppr b'
    return ((PP.parens $ (text . show $ x) <+> colon <+> t') <> bnd')

    where
      ((x, Embed t), b') = unrebind bnd

showExpr :: Expr -> String
showExpr = show . runLFreshM . ppr

isEmpty :: Tele -> Bool
isEmpty Empty = True
isEmpty _ = False
