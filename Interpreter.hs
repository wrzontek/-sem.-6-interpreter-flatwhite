module Interpreter where
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as Map
import AbsFlatwhite
import Control.Monad.State (execState)
import Expressions (evalExpr)
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


--execStmt (Decl p t x:xs) = undefined
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