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

typeToTTypeForDecl :: Type' BNFC'Position -> BNFC'Position -> TypeChecker TType
typeToTTypeForDecl (Int _) _ = return TInt
typeToTTypeForDecl (Str _) _ = return TString
typeToTTypeForDecl (Bool _) _ = return TBool
typeToTTypeForDecl _ p = throwError $ "Cannot declare void/function variable at: " ++ showPos p

typeToTTypeForReturn :: Type' BNFC'Position -> BNFC'Position -> TypeChecker TType
typeToTTypeForReturn (Int _) _ = return TInt
typeToTTypeForReturn (Str _) _ = return TString
typeToTTypeForReturn (Bool _) _ = return TBool
typeToTTypeForReturn (Void _) _ = return TVoid
typeToTTypeForReturn _ p = throwError $ "Cannot return function at: " ++ showPos p


-- typeToTType (Fun _ ret args) = TFunction (Prelude.map typeToTType args) (typeToTType ret)
-- typeToTType (Void _) = TVoid

execStmt :: Stmt -> TypeChecker ()
execStmt (Decl p t []) = throwError $ "Empty declaration at: " ++ showPos p
execStmt (Decl p t [NoInit p' x]) = do
    env <- get
    case Map.lookup x env of
        Nothing -> do
            t' <- typeToTTypeForDecl t p
            modify (Map.insert x (TypeInfo t' False))
        _ -> throwError $  "Redeclaration at: " ++ showPos p

execStmt (Decl p t [Init p' x expr]) = do
    env <- get
    case Map.lookup x env of
        Nothing -> do
            t1 <- typeToTTypeForDecl t p
            t2 <- getExprType expr
            if t1 == t2
                then modify (Map.insert x (TypeInfo t1 False))
                else throwError $  "Declaration of incorrect type at: " ++ showPos p
        _ -> throwError $  "Redeclaration at: " ++ showPos p

execStmt (Decl p t (x:xs)) = do
    execStmt (Decl p t [x])
    execStmt (Decl p t xs)

execStmt (ConstDecl p t []) = throwError $  "Empty declaration at: " ++ showPos p
execStmt (ConstDecl p t [NoInit p' x]) = do
    env <- get
    case Map.lookup x env of
        Nothing -> do
            t' <- typeToTTypeForDecl t p
            modify (Map.insert x (TypeInfo t' True))
        _ -> throwError $  "Redeclaration at: " ++ showPos p

execStmt (ConstDecl p t [Init p' x expr]) = do
    env <- get
    case Map.lookup x env of
        Nothing -> do
            t1 <- typeToTTypeForDecl t p
            t2 <- getExprType expr
            if t1 == t2
                then modify (Map.insert x (TypeInfo t1 True))
                else throwError $  "Declaration of incorrect type at: " ++ showPos p
        _ -> throwError $  "Redeclaration at: " ++ showPos p

execStmt (ConstDecl p t (x:xs)) = do
    execStmt (ConstDecl p t [x])
    execStmt (ConstDecl p t xs)

execStmt (Empty p) = return ()
execStmt (BStmt p (Block p' [])) = return ()
execStmt (BStmt p (Block p' (s:sx))) = do
    execStmt s
    execStmt (BStmt p (Block p' sx))

execStmt (Ass p ident expr) = do
    vars <- get
    case Map.lookup ident vars of
        (Just (TypeInfo t False)) -> do
            t' <- getExprType expr
            when (t /= t') $ throwError $  "Assignment of incorrect type at: " ++ showPos p
        (Just (TypeInfo _ True)) -> throwError $ "Assignment to readonly variable: " ++ show ident ++ " at: " ++ showPos p
        Nothing -> throwError $ "Assignment to undeclared variable: " ++ show ident ++ " at: " ++ showPos p

execStmt (Cond p expr stmt) = do
    cond <- getExprType expr
    case cond of 
      TBool -> execStmt stmt
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

execStmt (While p expr stmt) = do
    cond <- getExprType expr
    case cond of 
      TBool -> execStmt stmt
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

execStmt (CondElse p expr stmt1 stmt2) = do
    cond <- getExprType expr
    case cond of
      TBool -> do 
          initialEnv <- get
          execStmt stmt1
          modify (\_ -> initialEnv) 
          execStmt stmt2
      _ -> throwError $ "Non-boolean condition at: " ++ showPos p

execStmt (For p loopVar startExpr endExpr stmt) = do
    env <- get
    case Map.lookup loopVar env of 
        Nothing -> do
            startVal <- getExprType startExpr
            endVal <- getExprType endExpr
            case (startVal, endVal) of
                (TInt, TInt) -> do
                    modify (Map.insert loopVar (TypeInfo TInt True))
                    execStmt stmt
                _ -> throwError $ "Non-integer for range at: " ++ showPos p
        _ -> throwError $ "Redeclaration at: " ++ showPos p

execStmt (SExp p expr) = return ()

execStmt (Ret p expr) = do
    var <- getExprType expr
    modify (Map.insert returnValue (TypeInfo var True))
    return ()

execStmt (VRet p) = do
    modify (Map.insert returnValue (TypeInfo TVoid True))
    return ()

argToTTypeWithIdent :: Arg' BNFC'Position -> TypeChecker (TType, Ident)
argToTTypeWithIdent (Arg _ (Str _) ident) = return (TString, ident)
argToTTypeWithIdent (Arg _ (Int _) ident) = return (TInt, ident)
argToTTypeWithIdent (Arg _ (Bool _) ident) = return (TBool, ident)
argToTTypeWithIdent (Arg p _ ident) = throwError $ "Function/void passed as function argument: " ++ show ident ++ " at: " ++ showPos p

execDef :: TopDef -> TypeChecker ()
execDef (FnDef p _ f _ (Block _ [])) = throwError $ "Empty function body at: " ++ showPos p
execDef (FnDef p retT f args block) = do
    modify (Map.insert (funcIdent f) (TypeInfo (TFunction function) True))
    where
        checkFunctionArgs :: BNFC'Position -> [(TType, Ident)] -> [Expr] -> TypeChecker ()
        checkFunctionArgs p [] [] = return ()
        checkFunctionArgs p [] e = throwError $ "Incorrect argument count at: " ++ showPos p
        checkFunctionArgs p a [] = throwError $ "Incorrect argument count at: " ++ showPos p
        checkFunctionArgs p ((argType, argIdent):as) (e:es) = do
            t <- getExprType e
            if argType == t
                then do
                    modify (Map.insert argIdent (TypeInfo t False))
                    checkFunctionArgs p as es
                else
                    throwError $ "Incorrect argument type for argument: " ++ show argIdent ++ " at: " ++ showPos p

        function :: [Expr] -> BNFC'Position -> TypeChecker TType
        function xs p' = do
            initialEnv <- get
            modify (Map.delete returnValue)
            argTypes <- mapM argToTTypeWithIdent args
            checkFunctionArgs p' argTypes xs
            execStmt (BStmt p' block)
            postStmtsEnv <- get
            modify (\_ -> initialEnv) -- returning to original, pre-function-call environment
            case Map.lookup returnValue postStmtsEnv of
                Nothing -> throwError $ "Function: " ++ show f ++ " doesn't return"
                (Just (TypeInfo retT' _)) ->  do
                    retT <- typeToTTypeForReturn retT p'
                    if retT == retT' then return retT' else throwError $ "Cannot return function at " ++ showPos p

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
        (Just (TypeInfo (TFunction f) _)) -> do
            f [] p
            return ()
        _ -> throwError "No 'main' function defined"    