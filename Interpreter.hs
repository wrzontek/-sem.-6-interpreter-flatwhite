module Interpreter where
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as Map
import AbsFlatwhite
import Control.Monad.State (execState)
import Expressions (evalExpr, evalBool)
import Types

-- todo printInt, printBool i printString, pewnie oddzielny moduł

noInitVar :: Type -> Bool -> VarInfo
noInitVar (Int _) readonly = VarInfo (VInt 0) readonly
noInitVar (Str _) readonly = VarInfo (VString "") readonly
noInitVar (Bool _) readonly = VarInfo (VBool False) readonly
noInitVar (Void _) readonly = VarInfo VVoid readonly
noInitVar Fun {} readonly = error "cannot declare function as variable"

execStmt :: Stmt -> Interpreter () -> Interpreter ()
execStmt (Decl p t [NoInit p' x]) interpreter = local (Map.insert x (noInitVar t False)) interpreter -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
execStmt (Decl p t [Init p' x expr]) interpreter = do
    -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
    var <- evalExpr expr -- todo typecheckerować to czy coś idk
    local (Map.insert x (VarInfo var False)) interpreter
execStmt (Decl p t (x:xs)) interpreter = do
    interpreter' <- execStmt (Decl p t [x]) interpreter -- todo idk czy to modyfikuje stan tak jak powinno
    execStmt (Decl p t xs) interpreter

execStmt (ConstDecl p t [NoInit p' x]) interpreter = local (Map.insert x (noInitVar t True)) interpreter -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
execStmt (ConstDecl p t [Init p' x expr]) interpreter = do
    -- todo pewnie trzeba sprawdzać czy już nie zadeklarowana, pewnie w typecheckerze
    var <- evalExpr expr -- todo typecheckerować to czy coś idk
    local (Map.insert x (VarInfo var True)) interpreter
execStmt (ConstDecl p t (x:xs)) interpreter = do
    interpreter' <- execStmt (ConstDecl p t [x]) interpreter -- todo idk czy to modyfikuje stan tak jak powinno
    execStmt (ConstDecl p t xs) interpreter

execStmt (Empty p) _ = return ()
execStmt (BStmt p block) interpreter = undefined

execStmt (Ass p ident expr) interpreter = do
    vars <- ask
    case Map.lookup ident vars of
        (Just _) -> do 
            val <- evalExpr expr
            local (Map.insert ident (VarInfo val False)) interpreter -- typ w typechekerze, const sprawdzenie w typechekerze
        Nothing -> throwError $  "Assignment to undeclared variable: " ++ show ident ++ " at " ++ show p --chyba wyjdzie w TypeCheckerze ????

execStmt (Cond p expr stmt) interpreter = do
    bool <- evalBool expr p
    case bool of
        True -> execStmt stmt interpreter
        False -> return ()

execStmt (CondElse p expr stmt1 stmt2) interpreter = do
    bool <- evalBool expr p
    case bool of
        True -> execStmt stmt1 interpreter
        False -> execStmt stmt2 interpreter

execStmt w@(While p expr stmt) interpreter = do
    bool <- evalBool expr p
    case bool of
        True -> do 
            interpreter' <- execStmt stmt interpreter -- todo idk czy modyfikuje stan
            execStmt w interpreter
        False -> return ()

execStmt _ interpreter = undefined

execProgram :: Program -> IO ()
execProgram prog@(Program pos d) = do
    return ()
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