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

returnBlockPos :: BlockPos
returnBlockPos = BNFC'Position (-2) (-2)  -- will not be equal to any other position in program

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

execStmts :: BlockPos -> [Stmt] -> TypeChecker ()
execStmts _ [] = return ()
execStmts b (s:sx) = do
        execStmt b s
        execStmts b sx

execStmt :: BlockPos -> Stmt -> TypeChecker ()
execStmt b (Decl p t x) = execDecl b p t x False
execStmt b (ConstDecl p t x) = execDecl b p t x True

execStmt _ (Empty p) = return ()
execStmt _ (BStmt p (Block p' [])) = return ()
execStmt b (BStmt p (Block newBlock stmts)) = do
    execStmts newBlock stmts
    modify (Map.fromList . Prelude.map (removeInnerDeclarations newBlock) . toList)
    where
        removeInnerDeclarations :: BlockPos -> (Ident, [TypeInfo]) -> (Ident, [TypeInfo])
        removeInnerDeclarations b (ident, ts) = (ident, cutFrom ts b)

        cutFrom :: [TypeInfo] -> BlockPos -> [TypeInfo]
        cutFrom [] _ = []
        cutFrom tis@((TypeInfo (TFunction _) _ x) : xs) y = tis
        cutFrom (ti@(TypeInfo _ _ x) : xs) y
            | x == returnBlockPos = ti : cutFrom xs y
            | x == y = xs
            | otherwise = cutFrom xs y
        -- cutTo :: [TypeInfo] -> BlockPos -> [TypeInfo]
        -- cutTo [] _ = []
        -- cutTo (ti@(TypeInfo _ _ x) : xs) y
        --     | x == y = [ti]
        --     | otherwise = ti : cutTo xs y

execStmt b (Ass p ident expr) = do
    vars <- get
    case Map.lookup ident vars of
        Just (x:_) -> case x of
            (TypeInfo t False _) -> do
                t' <- getExprType expr
                when (t /= t') $ throwError $  "Assignment of incorrect type at: " ++ showPos p
            (TypeInfo _ True _) -> throwError $ "Assignment to readonly variable: " ++ show ident ++ " at: " ++ showPos p
        _ -> throwError $ "Assignment to undeclared variable: " ++ show ident ++ " at: " ++ showPos p

execStmt b (Cond p expr stmt) = do
    cond <- getExprType expr
    case cond of
      TBool -> execStmt b stmt
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

execStmt b (While p expr stmt) = do
    cond <- getExprType expr
    case cond of
      TBool -> execStmt b stmt
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

execStmt b (CondElse p expr stmt1 stmt2) = do
    cond <- getExprType expr
    case cond of
      TBool -> do
          initialEnv <- get
          execStmt b stmt1
          modify (const initialEnv)
          execStmt b stmt2
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

execStmt b (For p loopVar startExpr endExpr stmt) = do
    env <- get
    case Map.lookup loopVar env of
        Nothing -> do
            startVal <- getExprType startExpr
            endVal <- getExprType endExpr
            case (startVal, endVal) of
                (TInt, TInt) -> do
                    modify (Map.insert loopVar [TypeInfo TInt True b])
                    execStmt b stmt
                _ -> throwError $ "Non-integer for range at: " ++ showPos p
        _ -> throwError $ "Redeclaration at: " ++ showPos p

execStmt b (SExp p expr) = return ()

execStmt b (Ret p expr) = do
    var <- getExprType expr
    modify (Map.insert returnValue [TypeInfo var True returnBlockPos])
    return ()

execStmt b (VRet p) = do
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
            -- case d of
            -- TypeInfo _ _ b' -> if fPos == b'
            --     then throwError $  "Function Redefiniction at: " ++ showPos fPos
            --     else do
            --         modify (Map.insert (funcIdent f) (TypeInfo (TFunction function) True fPos : ds))
        _ -> modify (Map.insert (funcIdent f) [TypeInfo (TFunction function) True fPos])

    where
        checkFunctionArgs :: BNFC'Position -> [(TType, Ident)] -> [Expr] -> TypeChecker ()
        checkFunctionArgs p [] [] = return ()
        checkFunctionArgs p [] e = throwError $ "Incorrect argument count at: " ++ showPos p
        checkFunctionArgs p a [] = throwError $ "Incorrect argument count at: " ++ showPos p
        checkFunctionArgs p ((argType, argIdent):as) (e:es) = do
            vars <- get
            t <- getExprType e
            if argType == t
                then case Map.lookup argIdent vars of
                    Just ds@(d:_) -> case d of
                        TypeInfo _ _ b' -> if p == b'
                                then throwError $  "Redeclaration at: " ++ showPos p
                                else do
                                    modify (Map.insert argIdent (TypeInfo t False p : ds))
                                    checkFunctionArgs p as es
                    _ -> do
                        modify (Map.insert argIdent [TypeInfo t False p])
                        checkFunctionArgs p as es
                else
                    throwError $ "Incorrect argument type for argument: " ++ show argIdent ++ " at: " ++ showPos p

        function :: [Expr] -> BNFC'Position -> TypeChecker TType
        function xs p' = do
            initialEnv <- get
            modify (Map.delete returnValue)
            argTypes <- mapM argToTTypeWithIdent args
            checkFunctionArgs p' argTypes xs
            execStmt p' (BStmt p' block)
            postStmtsEnv <- get
            modify (const initialEnv) -- returning to original, pre-function-call environment
            case Map.lookup returnValue postStmtsEnv of
                (Just ((TypeInfo retT' _ _):_)) ->  do
                    retT <- typeToTTypeForReturn retT p'
                    if retT == retT' then return retT' else throwError $ "Incorrect return type from function call at " ++ showPos p'
                _ -> throwError $ "Function: " ++ show f ++ " doesn't return"

execDefs :: [TopDef] -> TypeChecker ()
execDefs [] = return ()
execDefs (d:ds) = do
    execDef d
    execDefs ds    

execTypeCheck :: BNFC'Position -> [TopDef] -> TypeChecker ()
execTypeCheck p defs = do
    execDefs defs
    env <- get
    case Map.lookup (funcIdent $ Ident "main") env of
        (Just ((TypeInfo (TFunction f) _ _):_)) -> do
            f [] p
            return ()
        _ -> throwError "No 'main' function defined"    