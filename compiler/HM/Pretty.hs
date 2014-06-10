module HM.Pretty where

class Pretty a where 
    pretty :: a -> String

instance Pretty CType where
    pretty CTLit = "lit"
    pretty (CTVar x) = x
    pretty (CTArr t1 t2) = 
        case t1 of 
            CTArr _ _ -> paren (pretty t1) ++ arrow ++ pretty t2
            _         -> pretty t1 ++ arrow ++ pretty t2
            where arrow = " -> "
                  paren s = "(" ++ s ++ ")"

instance Pretty Exp where
    pretty (EVar x) = x
    pretty (ELit i) = show i
    pretty (EApp e0 e1) = "(" ++ pretty e0 ++ " " ++ pretty e1 ++ ")"
    pretty (ELam x e) = "(\\" ++ x ++ " -> " ++ pretty e ++ ")"
    pretty (ELet x e0 e1) = "(let " ++ x ++ " = " ++ pretty e0 ++ " in " ++ pretty e1 ++ ")"
    pretty (ELetRec bindings body) = "(let rec " ++ prettyBindings ++ " in " ++ pretty body ++ ")"
        where prettyBindings = intercalate " and " $ map (\(x, e) -> x ++ " = " ++ pretty e) bindings
    pretty (EUn  op e) = "(" ++ pretty op  ++ pretty e ++ ")"
    pretty (EBin op e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty op ++ " " ++ pretty e2 ++ ")"
    pretty (EIf0 e0 e1 e2) = "(if0 " ++ pretty e0 ++ " then " ++ pretty e1 ++ " else " ++ pretty e2 ++ ")"

instance Pretty UnOp where
    pretty UMinus = "-"
    pretty Not    = "!"

instance Pretty BinOp where
    pretty Add = "+" 
    pretty Sub = "-" 
    pretty Mul = "*" 
    pretty Div = "/" 
    pretty Mod = "%"

    pretty Eq = "=="
    pretty Ne = "!="
    pretty Lt = "<"
    pretty Gt = ">"
    pretty Le = "<="
    pretty Ge = ">="

    pretty And = "&&"
    pretty Or = "||"

instance Pretty Type where
    pretty (TMono t) = pretty t
    pretty (TPoly s) = pretty s

instance Pretty Mono where
    pretty (MVar a) = a
    pretty (MPrim Int) = "int"
    pretty (MPrim Bool) = "bool"
    pretty (MApp t0 t1) = "(" ++ pretty t0 ++ " -> " ++ pretty t1 ++ ")"

instance Pretty Poly where
    pretty (PMono t) = pretty t
    pretty (PForall a s) = "(forall " ++ a ++ " . " ++ pretty s ++ ")"
