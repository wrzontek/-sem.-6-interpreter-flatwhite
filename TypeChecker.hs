{-# LANGUAGE FlexibleContexts #-}
module TypeChecker where
import Control.Monad.Except
import Data.Map as Map
import AbsFlatwhite
import Control.Monad.State ( modify, StateT (runStateT), MonadState (get) )
import TExpressions (getExprType)
import Types
import System.IO (stderr, hPutStrLn)
import Control.Monad

-- tPrintString :: [Expr] -> BNFC'Position -> TypeChecker TType
-- tPrintString [expr] p = do
--     s <- evalString expr p
--     liftIO $ putStr $ s ++ "\n"
--     return VVoid
-- tPrintString [] p = throwError $ "no argument in print function at: " ++ showPos p
-- tPrintString exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p

-- tPrintInt :: [Expr] -> BNFC'Position -> TypeChecker TType
-- tPrintInt [expr] p = do
--     n <- evalInteger expr p
--     liftIO $ putStr $ show n ++ "\n"
--     return VVoid
-- tPrintInt [] p = throwError $ "no argument in print function at: " ++ showPos p
-- tPrintInt exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p

-- printBool :: [Expr] -> BNFC'Position -> TypeChecker TType
-- printBool [expr] p = do
--     b <- evalBool expr p
--     liftIO $ putStr $ show b ++ "\n"
--     return VVoid
-- printBool [] p = throwError $ "no argument in print function at: " ++ showPos p
-- printBool exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p


safeHead :: [BlockPos] -> BlockPos
safeHead [] = BNFC'Position (-1) (-1) -- will not be equal to any actual position in program
safeHead (x:xs) = x

typeToTTypeForDecl :: Type-> BNFC'Position -> TypeChecker TType
typeToTTypeForDecl (Int _) _ = return TInt
typeToTTypeForDecl (Str _) _ = return TString
typeToTTypeForDecl (Bool _) _ = return TBool
typeToTTypeForDecl _ p = throwError $ "Cannot declare void/function variable at: " ++ showPos p

typeToTTypeForReturn :: Type -> BNFC'Position -> TypeChecker TType
typeToTTypeForReturn (Int _) _ = return TInt
typeToTTypeForReturn (Str _) _ = return TString
typeToTTypeForReturn (Bool _) _ = return TBool
typeToTTypeForReturn (Void _) _ = return TVoid
typeToTTypeForReturn _ p = throwError $ "Cannot return function at: " ++ showPos p


-- combineStateOps :: (Map Ident [TypeInfo] -> Map Ident [TypeInfo]) -> ([BNFC'Position] -> [BNFC'Position]) -> (Map Ident [TypeInfo], [BNFC'Position]) -> (Map Ident [TypeInfo], [BNFC'Position])
-- combineStateOps mapOp blocksOp (map, blocks) = (mapOp map, blocksOp blocks)

execDecl :: BlockPos -> BNFC'Position -> Type -> [Item] -> Bool -> TypeChecker ()
execDecl _ p t [] _ = throwError $ "Empty declaration at: " ++ showPos p
execDecl b p t [NoInit p' x] ro = do
    t' <- typeToTTypeForDecl t p
    env <- get
    case Map.lookup x env of
        Just ds@(d:_) -> case d of
            TypeInfo _ _ b' -> if b == b'
                    then throwError $  "Redeclaration at: " ++ showPos p
                    else modify (Map.insert x (TypeInfo t' ro b : ds))
        _ -> modify (Map.insert x [TypeInfo t' ro b])

execDecl b p t [Init p' x expr] ro = do
    t1 <- typeToTTypeForDecl t p
    t2 <- getExprType expr
    if t1 /= t2
        then throwError $  "Declaration of incorrect type at: " ++ showPos p
        else do
            env <- get
            case Map.lookup x env of
                Just ds@(d:_) -> case d of
                    TypeInfo _ _ b' -> if b == b'
                            then throwError $  "Redeclaration at: " ++ showPos p
                            else modify (Map.insert x (TypeInfo t1 ro b : ds))
                _ -> modify (Map.insert x [TypeInfo t1 ro b])

execDecl b p t (x:xs) ro = do
    execDecl b p t [x] ro
    execDecl b p t xs ro

checkStmts :: BlockPos -> [Stmt] -> TypeChecker ()
checkStmts _ [] = return ()
checkStmts b (s:sx) = do
    checkStmt b s
    env <- get
    case Map.lookup returnValue env of
        Nothing -> checkStmts b sx
        Just _ -> return ()

checkStmt :: BlockPos -> Stmt -> TypeChecker ()
checkStmt b (Decl p t x) = execDecl b p t x False
checkStmt b (ConstDecl p t x) = execDecl b p t x True

checkStmt _ (Empty p) = return ()
checkStmt _ (BStmt p (Block p' [])) = return ()
checkStmt b (BStmt p (Block newBlock stmts)) = do
    modify (Map.delete returnValue)
    checkStmts newBlock stmts
    modify (Map.fromList . Prelude.map (removeInnerDeclarations newBlock) . toList)
    where
        removeInnerDeclarations :: BlockPos -> (Ident, [TypeInfo]) -> (Ident, [TypeInfo])
        removeInnerDeclarations b (ident, ts) = (ident, cutOutBlockDeclarations ts b)

        cutOutBlockDeclarations :: [TypeInfo] -> BlockPos -> [TypeInfo]
        cutOutBlockDeclarations [] _ = []
        cutOutBlockDeclarations tis@((TypeInfo TFunction {} _ x) : xs) y = tis
        cutOutBlockDeclarations (ti@(TypeInfo _ _ x) : xs) y
            | x == y = cutOutBlockDeclarations xs y
            | otherwise = ti : cutOutBlockDeclarations xs y

checkStmt b (Ass p ident expr) = do
    vars <- get
    case Map.lookup ident vars of
        Just (x:_) -> case x of
            (TypeInfo t False _) -> do
                t' <- getExprType expr
                when (t /= t') $ throwError $  "Assignment of incorrect type at: " ++ showPos p
            (TypeInfo _ True _) -> throwError $ "Assignment to readonly variable: " ++ show ident ++ " at: " ++ showPos p
        _ -> throwError $ "Assignment to undeclared variable: " ++ show ident ++ " at: " ++ showPos p

checkStmt b (Cond p expr stmt) = do
    cond <- getExprType expr
    case cond of
      TBool -> checkStmt b stmt
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

checkStmt b (While p expr stmt) = do
    cond <- getExprType expr
    case cond of
      TBool -> checkStmt b stmt
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

checkStmt b (CondElse p expr stmt1 stmt2) = do
    cond <- getExprType expr
    case cond of
      TBool -> do
          initialEnv <- get
          checkStmt b stmt1
          modify (const initialEnv)
          checkStmt b stmt2
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

checkStmt b (For p loopVar startExpr endExpr stmt) = do
    env <- get
    startVal <- getExprType startExpr
    endVal <- getExprType endExpr
    case Map.lookup loopVar env of
        Just ds@(d:_) -> case d of
            TypeInfo _ _ b' -> if b == b'
                then throwError $  "Redeclaration at: " ++ showPos p
                else do
                    modify (Map.insert loopVar (TypeInfo TInt True b : ds))
                    checkStmt b stmt
        _ -> do
            modify (Map.insert loopVar [TypeInfo TInt True b])
            checkStmt b stmt

checkStmt b (SExp p expr) = do 
    getExprType expr
    return ()

checkStmt b (Ret p expr) = do
    var <- getExprType expr
    modify (Map.insert returnValue [TypeInfo var True returnBlockPos])
    return ()

checkStmt b (VRet p) = do
    modify (Map.insert returnValue [TypeInfo TVoid True returnBlockPos])
    return ()

argToTTypeWithIdent :: Arg -> TypeChecker (TType, Ident)
argToTTypeWithIdent (Arg _ (Str _) ident) = return (TString, ident)
argToTTypeWithIdent (Arg _ (Int _) ident) = return (TInt, ident)
argToTTypeWithIdent (Arg _ (Bool _) ident) = return (TBool, ident)
argToTTypeWithIdent (Arg p _ ident) = throwError $ "Function/void passed as function argument: " ++ show ident ++ " at: " ++ showPos p

execDef :: TopDef -> TypeChecker ()
execDef (FnDef fPos _ f _ (Block _ [])) = throwError $ "Empty function body at: " ++ showPos fPos
execDef (FnDef fPos retT f args block) = do
    funcEnv <- get
    case Map.lookup f funcEnv of
        Just ds@(d:_) -> throwError $  "Function redefinition at: " ++ showPos fPos
        _ -> do
            argTypesWithIdents <- mapM argToTTypeWithIdent args
            retT <- typeToTTypeForReturn retT fPos
            modify (Map.insert (funcIdent f) [TypeInfo (TFunction argTypesWithIdents retT block) True fPos])

execDefs :: [TopDef] -> TypeChecker ()
execDefs [] = return ()
execDefs (d:ds) = do
    execDef d
    execDefs ds

declareCorrectArgs :: BNFC'Position -> [(TType, Ident)] -> TypeChecker ()
declareCorrectArgs _ [] = return ()
declareCorrectArgs b ((t,ident):args) = do
    modify (Map.insert ident [TypeInfo t False b])
    declareCorrectArgs b args

clearNonFunctions :: Map Ident [TypeInfo] -> Map Ident [TypeInfo]
clearNonFunctions typeMap = Map.fromList $ go (Map.toList typeMap) where
    go [] = []
    go ((f, t):xs) = (f, Prelude.filter isFunction t) : go xs

    isFunction (TypeInfo TFunction {} _ _) = True
    isFunction _ = False

checkAllFunctions :: BNFC'Position -> [(Ident, [TypeInfo])] -> TypeChecker ()
checkAllFunctions b [] = return ()
checkAllFunctions b ((fIdent, [TypeInfo (TFunction args retT block) _ p]) : fs) = do
    modify clearNonFunctions
    declareCorrectArgs b args
    checkStmt p (BStmt p block)
    postStmtsEnv <- get
    case Map.lookup returnValue postStmtsEnv of
        (Just ((TypeInfo retT' _ _):_)) ->  if retT == retT'
            then checkAllFunctions b fs
            else throwError $ "Incorrect return type from function " ++ show fIdent
        _ -> throwError $ "Function: " ++ show fIdent ++ " doesn't return"

checkAllFunctions _ _ = error "defined non-function as function" -- should never happen, just for pattern-matching purpuses


execTypeCheck :: BNFC'Position -> [TopDef] -> TypeChecker ()
execTypeCheck p defs = do
    execDefs defs
    env <- get
    case Map.lookup (funcIdent $ Ident "main") env of
        Just _ -> checkAllFunctions p $ Map.toList env
        _ -> throwError "No 'main' function defined"
