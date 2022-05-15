{-# LANGUAGE FlexibleContexts #-}
module Interpreter where
import Control.Monad.Except
import Data.Map as Map
import AbsFlatwhite
import Control.Monad.State ( modify, StateT (runStateT), MonadState (get) )
import IExpressions (evalExpr, evalBool, evalInteger, evalString)
import Types
import System.IO (stderr, hPutStrLn)
import Control.Monad
import TypeChecker (execTypeCheck)

printString :: [Expr] -> BNFC'Position -> Interpreter Var
printString [expr] p = do
    s <- evalString expr p
    liftIO $ putStr $ s ++ "\n"
    return VVoid
printString [] p = throwError $ "no argument in print function at: " ++ showPos p
printString exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p

printInt :: [Expr] -> BNFC'Position -> Interpreter Var
printInt [expr] p = do
    n <- evalInteger expr p
    liftIO $ putStr $ show n ++ "\n"
    return VVoid
printInt [] p = throwError $ "no argument in print function at: " ++ showPos p
printInt exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p

printBool :: [Expr] -> BNFC'Position -> Interpreter Var
printBool [expr] p = do
    b <- evalBool expr p
    liftIO $ putStr $ show b ++ "\n"
    return VVoid
printBool [] p = throwError $ "no argument in print function at: " ++ showPos p
printBool exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p

noInitVar :: Type -> Var
noInitVar (Int _) = VInt 0
noInitVar (Str _) = VString ""
noInitVar (Bool _) = VBool False
noInitVar (Void _) = VVoid
noInitVar Fun {} = error "cannot declare function as variable"

execDecl :: BNFC'Position -> Type -> [Item] -> Interpreter ()
execDecl p t [] = throwError $  "Empty declaration at " ++ showPos p
execDecl p t [NoInit p' x] = modify (Map.insert x (noInitVar t))
execDecl p t [Init p' x expr] = do
    var <- evalExpr expr
    modify (Map.insert x var)
execDecl p t (x:xs) = do
    execDecl p t [x]
    execDecl p t xs

execStmt :: Stmt -> Interpreter ()
execStmt (Decl p t x) = execDecl p t x
execStmt (ConstDecl p t x) =  execDecl p t x

execStmt (Empty p) = return ()
execStmt (BStmt p (Block p' [])) = return ()
execStmt (BStmt p (Block p' (s:sx))) = do
    execStmt s
    execStmt (BStmt p (Block p' sx))

execStmt (Ass p ident expr) = do
    vars <- get
    case Map.lookup ident vars of
        (Just v) -> do
            var <- evalExpr expr
            modify (Map.insert ident var)
        Nothing -> throwError $ "Assignment to undeclared variable: " ++ show ident ++ " at " ++ showPos p

execStmt (Cond p expr stmt) = do
    cond <- evalBool expr p
    when cond $ execStmt stmt

execStmt (CondElse p expr stmt1 stmt2) = do
    cond <- evalBool expr p
    if cond then execStmt stmt1 else execStmt stmt2

execStmt w@(While p expr stmt) = do
    cond <- evalBool expr p
    when cond $ do
        execStmt stmt
        execStmt w

execStmt (For p loopVar startExpr endExpr stmt) = do
    startVal <- evalInteger startExpr p
    endVal <- evalInteger endExpr p
    forLoop stmt startVal endVal
    where
    forLoop :: Stmt -> Integer -> Integer -> Interpreter ()
    forLoop s iter end =
        when (iter <= end) $ do
            modify (Map.insert loopVar (VInt iter))
            execStmt s
            forLoop s (iter + 1) end

execStmt (SExp p expr) = do
    evalExpr expr
    return ()

execStmt (Ret p expr) = do
    var <- evalExpr expr
    modify (Map.insert returnValue var)
    return ()
execStmt (VRet p) = do
    modify (Map.insert returnValue VVoid)
    return ()


execDef :: TopDef -> Interpreter ()
execDef (FnDef p retT f args block) = do
    modify (Map.insert (funcIdent f) (VFunction function))
    where
        getFunctionArgs :: BNFC'Position -> [Arg] -> [Expr] -> Interpreter ()
        getFunctionArgs p [] [] = return ()
        getFunctionArgs p [] e = throwError $ "Incorrect argument count at: " ++ showPos p
        getFunctionArgs p a [] = throwError $ "Incorrect argument count at: " ++ showPos p
        getFunctionArgs p (a:as) (e:es) = do
            v <- evalExpr e
            case (a,v) of
                (Arg _ (Int _) ident, VInt v) -> do
                    modify (Map.insert ident (VInt v))
                    getFunctionArgs p as es
                (Arg _ (Str _) ident, VString v) -> do
                    modify (Map.insert ident (VString v))
                    getFunctionArgs p as es
                (Arg _ (Bool _) ident, VBool v) -> do
                    modify (Map.insert ident (VBool v))
                    getFunctionArgs p as es
                (Arg _ _ ident, _) -> throwError $ "Incorrect argument type for argument: " ++ show ident ++ " at: " ++ showPos p

        function :: [Expr] -> BNFC'Position -> Interpreter Var
        function xs p' = do
            initialEnv <- get
            modify (Map.delete returnValue)
            getFunctionArgs p' args xs
            execStmt (BStmt p' block)
            postStmtsEnv <- get
            modify (const initialEnv) -- returning to original, pre-function-call environment
            case Map.lookup returnValue postStmtsEnv of
                Nothing -> throwError $ "Function: " ++ show f ++ " didn't call return at: " ++ showPos p'
                (Just rVal) -> return rVal

execDefs :: [TopDef] -> Interpreter()
execDefs [] = return ()
execDefs (d:ds) = do
    execDef d
    execDefs ds

execDefsThenMain :: BNFC'Position -> [TopDef] -> Interpreter()
execDefsThenMain p defs = do
    execDefs defs
    env <- get
    case Map.lookup (funcIdent $ Ident "main") env of
        (Just (VFunction f)) -> do
            f [] p
            return ()
        _ -> throwError "No 'main' function defined"


execProgram :: Program -> IO ()
-- execProgram prog@(Program p def@[FnDef p' t f _ s]) = do
execProgram prog@(Program p defs) = do
    typeCheck <- runExceptT $ runStateT (execTypeCheck p defs) Map.empty
    case typeCheck of
        Left error -> hPutStrLn stderr $ "type check error: " ++ error
        Right _ -> do
            let initEnv = Map.fromList [(funcIdent $ Ident "printInt", VFunction printInt), (funcIdent $ Ident "printString", VFunction printString), (funcIdent $ Ident "printBool", VFunction printBool)]
            result <- runExceptT $ runStateT (execDefsThenMain p defs) initEnv

            case result of
                Left error -> hPutStrLn stderr $ "runtime error: " ++ error
                Right _ -> return ()
