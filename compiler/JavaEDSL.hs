-- | EDSL for Language-Java

module JavaEDSL where

import Language.Java.Syntax
import StringPrefixes

-- TODO: module


arrayTy :: Type -> Type
arrayTy ty  = RefType (ArrayType ty)

classTy :: String -> Type
classTy t = RefType $ ClassRefType $ classTyp t

classTyp :: String -> ClassType
classTyp t = ClassType [(Ident t, [])]


closureClass :: String
closureClass = "hk.hku.cs.f2j.Closure"

closureType :: Type
closureType = classTy closureClass

objClassTy :: Type
objClassTy = classTy "Object"

-- javaClassType :: String -> Type
-- javaClassType t = RefType $ classTy t

name :: [String] -> Name
name xs = Name $ map Ident xs

var :: String -> Exp
var x = ExpName $ name [x]

block :: [BlockStmt] -> Block
block = Block

bStmt :: Stmt -> BlockStmt
bStmt = BlockStmt

localVar :: Type -> VarDecl -> BlockStmt
localVar typ vard = LocalVars [] typ [vard]

localFinalVar :: Type -> VarDecl -> BlockStmt
localFinalVar typ vard = LocalVars [Final] typ [vard]

methodCall :: String -> [Argument] -> Stmt
methodCall ident argu = ExpStmt (MethodInv (MethodCall (Name [Ident ident]) argu))

applyMethodCall :: Ident -> BlockStmt
applyMethodCall f = BlockStmt (classMethodCall (ExpName $ Name [f]) "apply" [])

classMethodCall :: Exp -> String -> [Argument] -> Stmt
classMethodCall e s argus = ExpStmt (MethodInv (PrimaryMethodCall e [] (Ident s) argus))

paramDecl :: Type -> String -> FormalParam
paramDecl t n = FormalParam [] t False (VarId (Ident n))

varDecl :: String -> Exp -> VarDecl
varDecl nam e = VarDecl (VarId $ Ident nam) (Just $ InitExp e)

varDeclNoInit :: String -> VarDecl
varDeclNoInit nam = VarDecl (VarId $ Ident nam) Nothing

methodDecl :: [Modifier] -> Maybe Type -> String -> [FormalParam] -> Maybe Block -> MemberDecl
methodDecl modi ty nam params body = MethodDecl modi [] ty (Ident nam) params [] (MethodBody body)

fieldDecl :: Type -> VarDecl -> MemberDecl
fieldDecl typ vdecl = FieldDecl [] typ [vdecl]

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

assign :: Name -> Exp -> BlockStmt
assign lhs rhs = BlockStmt $ ExpStmt $ Assign (NameLhs lhs) EqualA rhs

assignField :: FieldAccess -> Exp -> BlockStmt
assignField access rhs = BlockStmt $ ExpStmt $ Assign (FieldLhs access) EqualA rhs

fieldAccess :: Exp -> String -> Exp
fieldAccess expr str = FieldAccess $ PrimaryFieldAccess expr (Ident str)

fieldAccExp :: Exp -> String -> FieldAccess
fieldAccExp expr str = PrimaryFieldAccess expr (Ident str)

localClassDecl :: String -> String -> ClassBody -> BlockStmt
localClassDecl nam super body = LocalClass (ClassDecl [] (Ident nam) [] (Just $ ClassRefType $ ClassType [(Ident super, [])]) [] body)

localClass :: String -> ClassBody -> BlockStmt
localClass nam body = LocalClass (ClassDecl [] (Ident nam) [] Nothing [] body)

funInstCreate :: Int -> Exp
funInstCreate i = instCreat fun []
  where fun = (ClassType [(Ident ("Fun" ++ show i),[])])

closureBodyGen :: [Decl] -> [BlockStmt] -> Int -> Bool -> ClassBody
closureBodyGen initDecls body idCF generateClone =
  classBody $ initDecls ++ [applyMethod] ++ if generateClone
                                               then [cloneMethod]
                                               else []
  where applyMethod = MemberDecl $ methodDecl [Public] Nothing "apply" [] (Just (Block body))
        cloneMethod = MemberDecl $ methodDecl [Public] (Just closureType) "clone" [] (Just cloneBody)
        cloneBody = (block [localVar closureType (varDecl "c" (funInstCreate idCF))
                    ,assign (name ["c", closureInput]) (ExpName $ name ["this", closureInput])
                    ,bStmt (classMethodCall (var "c")
                                            "apply"
                                            [])
                    ,bStmt (Return (Just (cast closureType (var "c"))))])

mainArgType :: [FormalParam]
mainArgType = [paramDecl (arrayTy $ classTy "String") "args"]

mainBody :: Maybe Block
mainBody = Just (block [bStmt $ classMethodCall (var "System.out")
                                                "println"
                                                [var "apply()"]])

wraperClass :: String -> [BlockStmt] -> Maybe Type -> Maybe Block -> TypeDecl
wraperClass className stmts returnType mainbodyDef =
  ClassTypeDecl
    (classDecl [Public]
               className
               (classBody [memberDecl $
                           methodDecl [Static]
                                      returnType
                                      "apply"
                                      []
                                      body
                          ,memberDecl $
                           methodDecl [Public,Static]
                                      Nothing
                                      "main"
                                      mainArgType
                                      mainbodyDef]))
  where body = Just (block stmts)
