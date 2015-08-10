-- | EDSL for Language-Java

module JavaEDSL where

import Language.Java.Syntax
import StringPrefixes

-- Type

arrayTy :: Type -> Type
arrayTy ty  = RefType (ArrayType ty)

classTy :: String -> Type
classTy t = RefType $ ClassRefType $ classTyp t

classTyp :: String -> ClassType
classTyp t = ClassType [(Ident t, [])]

intTy :: Type
intTy = PrimType IntT

closureType :: Type
closureType = classTy (namespace ++ "Closure")

objClassTy :: Type
objClassTy = classTy "Object"

-- var

name :: [String] -> Name
name xs = Name $ map Ident xs

var :: String -> Either Name Literal
var x = Left $ name [x]

varExp :: String -> Exp
varExp x = ExpName $ name [x]

varDecl :: String -> Exp -> VarDecl
varDecl nam e = VarDecl (VarId $ Ident nam) (Just $ InitExp e)

varDeclNoInit :: String -> VarDecl
varDeclNoInit nam = VarDecl (VarId $ Ident nam) Nothing

localVar :: Type -> VarDecl -> BlockStmt
localVar typ vard = LocalVars [] typ [vard]

localFinalVar :: Type -> VarDecl -> BlockStmt
localFinalVar typ vard = LocalVars [Final] typ [vard]

extractVar :: Either Name Literal -> String
extractVar x = case x of
                Left (Name xs) -> case head xs of
                                   Ident xs' -> xs'
                Right _ -> error "this should be a left (variable name)"

-- transform

block :: [BlockStmt] -> Block
block = Block

bStmt :: Stmt -> BlockStmt
bStmt = BlockStmt

expToBlockStmt :: Exp -> BlockStmt
expToBlockStmt = BlockStmt . ExpStmt

left :: Either Name Literal -> Exp
left (Left x) = ExpName x
left (Right _) = error "this should be left (variable name)"

right :: Either Name Literal -> Exp
right (Right x) = Lit x
right (Left _) = error "this should be right (literal or method inv)"

unwrap :: Either Name Literal -> Exp
unwrap x = case x of
            Left (Name xs) -> ExpName . Name $ xs
            Right e -> Lit e

localToMemberClass :: BlockStmt -> MemberDecl
localToMemberClass (LocalClass t) = MemberClassDecl t
localToMemberClass (LocalVars modi typ decls) = FieldDecl modi typ decls
localToMemberClass _ = error "parameter should be a local class"

-- method

paramDecl :: Type -> String -> FormalParam
paramDecl t n = FormalParam [] t False (VarId (Ident n))

methodCallExp :: [String] -> [Argument] -> Exp
methodCallExp idents argu = MethodInv (MethodCall (name idents) argu)

methodCall :: [String] -> [Argument] -> Stmt
methodCall idents argu = ExpStmt (methodCallExp idents argu)

applyMethodCall :: Exp -> Stmt
applyMethodCall f = classMethodCall f "apply" []

applyCall :: BlockStmt
applyCall = bStmt $ methodCall ["apply"] []

classMethodCall :: Exp -> String -> [Argument] -> Stmt
classMethodCall e s argus = ExpStmt (MethodInv (PrimaryMethodCall e [] (Ident s) argus))

-- class construction

-- -- field member
fieldDecl :: Type -> VarDecl -> MemberDecl
fieldDecl typ vdecl = FieldDecl [] typ [vdecl]

finalFieldDecl :: Type -> VarDecl -> MemberDecl
finalFieldDecl typ vdecl = FieldDecl [Final] typ [vdecl]

-- -- class member
memberClassDecl :: String -> String -> ClassBody -> MemberDecl
memberClassDecl nam super body = MemberClassDecl (ClassDecl [] (Ident nam) [] (Just $ ClassRefType $ ClassType [(Ident super, [])]) [] body)

-- -- method member
methodDecl :: [Modifier] -> Maybe Type -> String -> [FormalParam] -> Maybe Block -> MemberDecl
methodDecl modi ty nam params body = MethodDecl modi [] ty (Ident nam) params [] (MethodBody body)

-- -- constructor
constructorDecl :: String -> [FormalParam] -> Maybe ExplConstrInv -> [BlockStmt] -> MemberDecl
constructorDecl nam params constrinv stmts = ConstructorDecl [] [] (Ident nam) params [] (ConstructorBody constrinv stmts)

-- -- -- for constructor: this.name = name
initField :: String -> BlockStmt
initField fieldname = let fieldaccess' = fieldAccExp This fieldname
                      in   assignField fieldaccess' (ExpName (name [fieldname]))

-- -- classbody
memberDecl :: MemberDecl -> Decl
memberDecl = MemberDecl

classBody :: [Decl] -> ClassBody
classBody = ClassBody

closureBodyGen :: [Decl] -> [BlockStmt] -> Int -> Bool -> Type -> ClassBody
closureBodyGen initDecls body idCF generateClone className = classBody $ initDecls ++ [applyMethod] ++ [cloneMethod | generateClone]
  where
    applyMethod = MemberDecl $ methodDecl [Public] Nothing "apply" [] (Just (Block body))
    cloneMethod = MemberDecl $ methodDecl [Public] (Just className) "clone" [] (Just cloneBody)
    cloneBody = block
                  [ localVar className (varDecl "c" (funInstCreate idCF))
                  ,
                  -- ,assign (name ["c", closureInput]) (ExpName $ name ["this", closureInput])
                  -- ,bStmt $ (applyMethodCall (left . var $ "c"))
                  bStmt (Return (Just (cast className (left $ var "c"))))
                  ]

-- -- class decl
localClassDecl :: String -> String -> ClassBody -> BlockStmt
localClassDecl nam super body = LocalClass (ClassDecl [] (Ident nam) [] (Just $ ClassRefType $ ClassType [(Ident super, [])]) [] body)

localClass :: String -> ClassBody -> BlockStmt
localClass nam body = LocalClass (ClassDecl [] (Ident nam) [] Nothing [] body)

classDecl :: [Modifier] -> String -> ClassBody -> ClassDecl
classDecl modi ident =
  ClassDecl modi (Ident ident) [] Nothing []

-- expression

-- -- lit
integerExp :: Integer -> Exp
integerExp = Lit . Int

stringExp :: String -> Exp
stringExp = Lit . String

nullExp :: Exp
nullExp = Lit Null

-- -- op
eq :: Exp -> Exp -> Exp
eq e1 = BinOp e1 Equal

-- -- type cast
cast :: Type -> Exp -> Exp
cast = Cast

-- -- class field access
fieldAccess :: Exp -> String -> Exp
fieldAccess expr str = FieldAccess $ PrimaryFieldAccess expr (Ident str)

fieldAccExp :: Exp -> String -> FieldAccess
fieldAccExp expr str = PrimaryFieldAccess expr (Ident str)

-- -- class instantiation
instCreat :: ClassType -> [Argument] -> Exp
instCreat cls args = InstanceCreation [] cls args Nothing

funInstCreate :: Int -> Exp
funInstCreate i = instCreat fun []
  where fun = ClassType [(Ident (closureTransName ++ show i),[])]

-- -- assignment
assign :: Name -> Exp -> BlockStmt
assign lhs rhs = BlockStmt $ ExpStmt $ Assign (NameLhs lhs) EqualA rhs

assignE :: Name -> Exp -> Stmt
assignE lhs rhs = ExpStmt $ Assign (NameLhs lhs) EqualA rhs

assignField :: FieldAccess -> Exp -> BlockStmt
assignField access rhs = BlockStmt $ ExpStmt $ Assign (FieldLhs access) EqualA rhs

-- -- if
ifthen :: Exp -> Stmt -> Stmt
ifthen = IfThen

-- -- switch
switchStmt:: Exp -> [SwitchBlock] ->Stmt
switchStmt = Switch

switchBlock :: Maybe Exp -> [BlockStmt] -> SwitchBlock
switchBlock (Just e) bs = SwitchBlock (SwitchCase e) $ bs ++ [bStmt (Break Nothing)]
switchBlock Nothing  bs = SwitchBlock Default bs

-- -- return
returnNull :: Maybe Block
returnNull = Just (Block [BlockStmt (Return (Just (Lit Null)))])

returnExp :: Exp -> Maybe Block
returnExp e= Just (Block [BlockStmt (Return (Just e))])

returnExpS :: Exp -> BlockStmt
returnExpS e= BlockStmt (Return (Just e))

-- -- exception
throwRuntimeException :: String -> Stmt
throwRuntimeException s = Throw $ instCreat (classTyp "RuntimeException") [Lit. String $ s]

-- Main

mainArgType :: [FormalParam]
mainArgType = [paramDecl (arrayTy $ classTy "String") "args"]

mainBody :: Maybe Block
mainBody = Just (block [bStmt $ classMethodCall (left $ var "System.out")
                                                "println"
                                                [left $ var "apply()"]])

wrapperClass :: String -> [BlockStmt] -> Maybe Type -> Maybe Block -> TypeDecl
wrapperClass className stmts returnType mainbodyDef  =
  ClassTypeDecl
    (classDecl [Public]
               className
               (classBody (applyMethod : [mainMethod])))
  where body = Just (block stmts)
        applyMethod = memberDecl $ methodDecl [Static] returnType "apply" [] body
        -- testMethod = memberDecl $ methodDecl [Public,Static] returnType "test" testArgType testBodyDef
        mainMethod  = memberDecl $ methodDecl [Public,Static] Nothing "main" mainArgType mainbodyDef

annotation :: String -> Modifier
annotation ann = Annotation MarkerAnnotation {annName = Name [Ident ann]}
