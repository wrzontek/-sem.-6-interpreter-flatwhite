module Interpreter where
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as Map
import AbsFlatwhite
import Control.Monad.State (execState)
import Expressions (evalExpr, evalBool, evalInteger, evalString, showPos)
import Types
import System.IO (stderr, hPutStrLn)

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

execStmt :: Stmt -> Interpreter () -> Interpreter ()
execStmt (Decl p t []) interpreter = throwError $  "Empty declaration at " ++ showPos p
execStmt (Decl p t [NoInit p' x]) interpreter = local (Map.insert x (noInitVar t False)) interpreter -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
execStmt (Decl p t [Init p' x expr]) interpreter = do
    -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
    var <- evalExpr expr -- todo typecheckerować to czy coś idk
    local (Map.insert x (VarInfo var False)) interpreter
execStmt (Decl p t (x:xs)) interpreter = do
    execStmt (Decl p t xs) (execStmt (Decl p t [x]) interpreter) -- todo idk czy to modyfikuje stan tak jak powinno

execStmt (ConstDecl p t []) interpreter = throwError $  "Empty declaration at " ++ showPos p
execStmt (ConstDecl p t [NoInit p' x]) interpreter = local (Map.insert x (noInitVar t True)) interpreter -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
execStmt (ConstDecl p t [Init p' x expr]) interpreter = do
    -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
    var <- evalExpr expr -- todo typecheckerować to czy coś idk
    local (Map.insert x (VarInfo var True)) interpreter
execStmt (ConstDecl p t (x:xs)) interpreter = do
    execStmt (ConstDecl p t xs) (execStmt (ConstDecl p t [x]) interpreter) -- todo idk czy to modyfikuje stan tak jak powinno

execStmt (Empty p) _ = return ()
execStmt (BStmt p (Block p' [])) interpreter = return ()
execStmt (BStmt p (Block p' (s:sx))) interpreter = do
    execStmt s interpreter
    execStmt (BStmt p (Block p' sx)) interpreter
    --execStmt (BStmt p (Block p' sx)) $ execStmt s interpreter

execStmt (Ass p ident expr) interpreter = do
    vars <- ask
    case Map.lookup ident vars of
        (Just _) -> do
            val <- evalExpr expr
            local (Map.insert ident (VarInfo val False)) interpreter -- typ w typechekerze, const sprawdzenie w typechekerze
        Nothing -> throwError $ "Assignment to undeclared variable: " ++ show ident ++ " at " ++ showPos p --chyba wyjdzie w TypeCheckerze ????

execStmt (Cond p expr stmt) interpreter = do
    cond <- evalBool expr p
    when cond $ execStmt stmt interpreter

execStmt (CondElse p expr stmt1 stmt2) interpreter = do
    cond <- evalBool expr p
    if cond then execStmt stmt1 interpreter else execStmt stmt2 interpreter

execStmt w@(While p expr stmt) interpreter = do
    cond <- evalBool expr p
    when cond $ execStmt w (execStmt stmt interpreter)

execStmt (For p loopVar startExpr endExpr stmt) interpreter = do
    startVal <- evalInteger startExpr p
    endVal <- evalInteger endExpr p
    forLoop stmt startVal endVal interpreter
    where
    forLoop :: Stmt -> Integer -> Integer -> Interpreter () -> Interpreter ()
    forLoop s start end interp =
        when (start < end) $ do
            -- todo sprawdzenie czy wolny identyfikator, tak samo pewnie w innych miejscach
            local (Map.insert loopVar (VarInfo (VInt start) True)) interpreter
            forLoop s (start + 1) end (execStmt s interpreter)

execStmt (SExp p expr) interpreter = do
    evalExpr expr
    return ()

execStmt (Ret p expr) interpreter = undefined
execStmt (VRet p) interpreter = undefined

execDef :: TopDef -> Interpreter ()
execDef (FnDef p t f args s) = return ()

execProgram :: Program -> IO ()
execProgram prog@(Program p [def@(FnDef p' t f _ s)]) = do
    let initEnv = Map.fromList [(Ident "printInt", printIntFunctionVarInfo), (Ident "printString", printStringFunctionVarInfo), (Ident "printBool", printBoolFunctionVarInfo)]
    result <- runExceptT $ runReaderT (execStmt (BStmt p' s) $ execDef def) initEnv
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