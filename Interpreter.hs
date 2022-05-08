module Interpreter where
import Control.Monad.Except
import Data.Map as Map
import AbsFlatwhite
import Control.Monad.State ( modify, StateT (runStateT), MonadState (get) )
import Expressions (evalExpr, evalBool, evalInteger, evalString, showPos)
import Types
import System.IO (stderr, hPutStrLn)
import Control.Monad

printString :: [Expr] -> BNFC'Position -> Interpreter Var
printString [expr] p = do
    s <- evalString expr p
    liftIO $ putStr $ show s ++ "\n"
    return VVoid
printString [] p = throwError $ "no argument in print function at: " ++ showPos p
printString exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p

printStringFunctionVarInfo :: VarInfo
printStringFunctionVarInfo = VarInfo (VFunction printString) False

printInt :: [Expr] -> BNFC'Position -> Interpreter Var
printInt [expr] p = do
    n <- evalInteger expr p
    liftIO $ putStr $ show n ++ "\n"
    return VVoid
printInt [] p = throwError $ "no argument in print function at: " ++ showPos p
printInt exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p

printIntFunctionVarInfo :: VarInfo
printIntFunctionVarInfo = VarInfo (VFunction printInt) False

printBool :: [Expr] -> BNFC'Position -> Interpreter Var
printBool [expr] p = do
    b <- evalBool expr p
    liftIO $ putStr $ show b ++ "\n"
    return VVoid
printBool [] p = throwError $ "no argument in print function at: " ++ showPos p
printBool exprs p = throwError $ "multiple arguments in print function at: " ++ showPos p

printBoolFunctionVarInfo :: VarInfo
printBoolFunctionVarInfo = VarInfo (VFunction printBool) False


noInitVar :: Type -> Bool -> VarInfo
noInitVar (Int _) readonly = VarInfo (VInt 0) readonly
noInitVar (Str _) readonly = VarInfo (VString "") readonly
noInitVar (Bool _) readonly = VarInfo (VBool False) readonly
noInitVar (Void _) readonly = VarInfo VVoid readonly
noInitVar Fun {} readonly = error "cannot declare function as variable"

execStmt :: Stmt -> Interpreter ()
execStmt (Decl p t []) = throwError $ "Empty declaration at " ++ showPos p
execStmt (Decl p t [NoInit p' x]) = modify (Map.insert x (noInitVar t False)) -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
execStmt (Decl p t [Init p' x expr]) = do
    -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
    var <- evalExpr expr -- todo typecheckerować to czy coś idk
    modify (Map.insert x (VarInfo var False))
execStmt (Decl p t (x:xs)) = do
    execStmt (Decl p t [x])
    execStmt (Decl p t xs)-- todo idk czy to modyfikuje stan tak jak powinno

execStmt (ConstDecl p t []) = throwError $  "Empty declaration at " ++ showPos p
execStmt (ConstDecl p t [NoInit p' x]) = modify (Map.insert x (noInitVar t True)) -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
execStmt (ConstDecl p t [Init p' x expr]) = do
    -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
    var <- evalExpr expr -- todo typecheckerować to czy coś idk
    modify (Map.insert x (VarInfo var True))
execStmt (ConstDecl p t (x:xs)) = do
    execStmt (ConstDecl p t [x])
    execStmt (ConstDecl p t xs) -- todo idk czy to modyfikuje stan tak jak powinno

execStmt (Empty p) = return ()
execStmt (BStmt p (Block p' [])) = return ()
execStmt (BStmt p (Block p' (s:sx))) = do
    execStmt s
    execStmt (BStmt p (Block p' sx))
    --execStmt (BStmt p (Block p' sx)) $ execStmt s

execStmt (Ass p ident expr) = do
    vars <- get
    case Map.lookup ident vars of
        (Just (VarInfo _ False)) -> do
            val <- evalExpr expr
            modify (Map.insert ident (VarInfo val False)) -- todo typ w typechekerze
        (Just (VarInfo _ True)) -> throwError $ "Assignment to readonly variable: " ++ show ident ++ " at " ++ showPos p --chyba wyjdzie w TypeCheckerze ????
        Nothing -> throwError $ "Assignment to undeclared variable: " ++ show ident ++ " at " ++ showPos p --chyba wyjdzie w TypeCheckerze ????

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
    forLoop s start end =
        when (start <= end) $ do
            -- todo sprawdzenie czy wolny identyfikator, tak samo pewnie w innych miejscach
            modify (Map.insert loopVar (VarInfo (VInt start) True))
            execStmt s
            forLoop s (start + 1) end

execStmt (SExp p expr) = do
    evalExpr expr
    return ()

-- todo returny
execStmt (Ret p expr) = do
    val <- evalExpr expr
    return ()
execStmt (VRet p) = return ()

execDef :: TopDef -> Interpreter ()
execDef (FnDef p t f args s) = return ()

execProgram :: Program -> IO ()
execProgram prog@(Program p [def@(FnDef p' t f _ s)]) = do
    let initEnv = Map.fromList [(Ident "printInt", printIntFunctionVarInfo), (Ident "printString", printStringFunctionVarInfo), (Ident "printBool", printBoolFunctionVarInfo)]
    result <- runExceptT $ runStateT (execStmt (BStmt p' s)) initEnv
    case result of
        Left err -> hPutStrLn stderr $ "runtime error: " ++ err
        Right _ -> return ()
execProgram _ = undefined

--   typeResult <- runExceptT $ execType program
--   case typeResult of
--     Left err -> hPutStrLn stderr $ "Type Error: " ++ err
--     Right _ -> do
--       let initStore = DataMap.fromList [(loopLocation, loopNormal)]
--       let initEnv = DataMap.empty
--       result <- runExceptT $ flip runStateT initStore $ flip runReaderT initEnv $ execStmt (SBlock d s)
--       case result of
--         Left err -> hPutStrLn stderr $ "Runtime Error: " ++ err
--         Right _ -> return ()

-- exec :: Program -> IO ()
-- exec program@(Program d s) = do
--   typeResult <- runExceptT $ execType program
--   case typeResult of
--     Left err -> hPutStrLn stderr $ "Type Error: " ++ err
--     Right _ -> do
--       let initStore = DataMap.fromList [(loopLocation, loopNormal)]
--       let initEnv = DataMap.empty
--       result <- runExceptT $ flip runStateT initStore $ flip runReaderT initEnv $ execStmt (SBlock d s)
--       case result of
--         Left err -> hPutStrLn stderr $ "Runtime Error: " ++ err
--         Right _ -> return ()