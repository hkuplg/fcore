-- | EDSL for Language-Java

module JavaEDSL where

import Language.Java.Syntax

-- TODO: module

arrayTy :: Type -> Type
arrayTy ty  = RefType (ArrayType ty)

classTy :: String -> Type
classTy t = RefType $ ClassRefType (ClassType [(Ident t,[])])

-- javaClassType :: String -> Type
-- javaClassType t = RefType $ classTy t

names :: [String] -> Name
names xs = Name $ map Ident xs

var :: String -> Exp
var x = ExpName $ names [x]

block :: [BlockStmt] -> Block
block = Block

bStmt :: Stmt -> BlockStmt
bStmt = BlockStmt

localVars :: [Modifier] -> Type -> [VarDecl] -> BlockStmt
localVars = LocalVars

methodCall :: String -> [Argument] -> Stmt
methodCall ident argu = ExpStmt (MethodInv (MethodCall (Name [Ident ident]) argu))

classMethodCall :: Exp -> String -> [Argument] -> Stmt
classMethodCall e s argus = ExpStmt (MethodInv (PrimaryMethodCall e [] (Ident s) argus))

paramDecl :: Type -> String -> FormalParam
paramDecl t n = FormalParam [] t False (VarId (Ident n))

varDecl :: String -> Maybe VarInit -> VarDecl
varDecl name e = VarDecl (VarId $ Ident name) e

methodDecl :: [Modifier] -> Maybe Type -> String -> [FormalParam] -> Maybe Block -> MemberDecl
methodDecl modi ty name params body = MethodDecl modi [] ty (Ident name) params [] (MethodBody body)

fieldDecl :: [Modifier] -> Type -> [VarDecl] -> MemberDecl
fieldDecl = FieldDecl

memberDecl :: MemberDecl -> Decl
memberDecl = MemberDecl

classDecl :: [Modifier] -> String -> ClassBody -> ClassDecl
classDecl modi ident body =
  ClassDecl modi (Ident ident) [] Nothing [] body

classBody :: [Decl] -> ClassBody
classBody = ClassBody

cast :: Type -> Exp -> Exp
cast = Cast

instCreat :: ClassType -> [Argument] -> Exp
instCreat cls args = InstanceCreation [] cls args Nothing

assign :: Name -> AssignOp -> Exp -> Exp
assign name op expr = Assign (NameLhs name)
                             op
                             expr
