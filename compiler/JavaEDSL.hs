-- | EDSL for Language-Java

module JavaEDSL where

import Language.Java.Syntax
import StringPrefixes
import Data.Either

arrayTy :: Type -> Type
arrayTy ty  = RefType (ArrayType ty)

classTy :: String -> Type
classTy t = RefType $ ClassRefType $ classTyp t

classTyp :: String -> ClassType
classTyp t = ClassType [(Ident t, [])]

closureType :: Type
closureType = classTy (namespace ++ "Closure")

objClassTy :: Type
objClassTy = classTy "Object"

name :: [String] -> Name
name xs = Name $ map Ident xs

var :: String -> Either Name Exp
var x = Left $ name [x]

block :: [BlockStmt] -> Block
block = Block

bStmt :: Stmt -> BlockStmt
bStmt = BlockStmt

localVar :: Type -> VarDecl -> BlockStmt
localVar typ vard = LocalVars [] typ [vard]

localFinalVar :: Type -> VarDecl -> BlockStmt
localFinalVar typ vard = LocalVars [Final] typ [vard]

methodCall :: [String] -> [Argument] -> Stmt
methodCall idents argu = ExpStmt (MethodInv (MethodCall (name idents) argu))

applyMethodCall :: Exp -> Stmt
applyMethodCall f = (classMethodCall f "apply" [])

applyCall :: BlockStmt
applyCall = bStmt $ methodCall ["apply"] []

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

assignE :: Name -> Exp -> Stmt
assignE lhs rhs = ExpStmt $ Assign (NameLhs lhs) EqualA rhs

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

closureBodyGen :: [Decl] -> [BlockStmt] -> Int -> Bool -> Type -> ClassBody
closureBodyGen initDecls body idCF generateClone className =
  classBody $ initDecls ++ [applyMethod] ++ if generateClone
                                               then [cloneMethod]
                                               else []
  where applyMethod = MemberDecl $ methodDecl [Public] Nothing "apply" [] (Just (Block body))
        cloneMethod = MemberDecl $ methodDecl [Public] (Just className) "clone" [] (Just cloneBody)
        cloneBody = (block [localVar className (varDecl "c" (funInstCreate idCF))
                    ,assign (name ["c", closureInput]) (ExpName $ name ["this", closureInput])
                    ,bStmt $ (applyMethodCall (left . var $ "c"))
                    ,bStmt (Return (Just (cast className (left $ var "c"))))])

mainArgType :: [FormalParam]
mainArgType = [paramDecl (arrayTy $ classTy "String") "args"]

left :: Either Name Exp -> Exp
left (Left x) = ExpName x
left (Right _) = error "this should be left (variable name)"

right :: Either Name Exp -> Exp
right (Right x) = x
right (Left _) = error "this should be right (literal or method inv)"

unwrap :: Either Name Exp -> Exp
unwrap x = if isLeft x then left x else right x

mainBody :: Maybe Block
mainBody = Just (block [bStmt $ classMethodCall (left $ var "System.out")
                                                "println"
                                                [left $ var "apply()"]])

wrapperClass :: String -> [BlockStmt] -> Maybe Type -> Maybe Block -> [FormalParam] -> Maybe Block -> Bool -> TypeDecl
wrapperClass className stmts returnType mainbodyDef testArgType testBodyDef genTest =
  ClassTypeDecl
    (classDecl [Public]
               className
               (classBody (applyMethod : mainMethod : if genTest then [testMethod] else [])))
  where body = Just (block stmts)
        applyMethod = memberDecl $ methodDecl [Static] returnType "apply" [] body
        testMethod = memberDecl $ methodDecl [Public,Static] returnType "test" testArgType testBodyDef
        mainMethod  = memberDecl $ methodDecl [Public,Static] Nothing "main" mainArgType mainbodyDef

annotation :: String -> Modifier
annotation ann = Annotation (MarkerAnnotation {annName = Name [Ident ann]})

returnNull :: Maybe Block
returnNull = (Just (Block [BlockStmt (Return (Just (Lit Null)))]))
