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
import Text.XHtml (yellow)

printString :: [Expr] -> BNFC'Position -> Interpreter Var
printString [expr] p = do
    s <- evalString expr p
    liftIO $ putStr $ s ++ "\n"
    return VVoid
printString _ p = throwError $ "bad argument count in print function at: " ++ showPos p -- checked by typechecker, included here for complete pattern matching

printInt :: [Expr] -> BNFC'Position -> Interpreter Var
printInt [expr] p = do
    n <- evalInteger expr p
    liftIO $ putStr $ show n ++ "\n"
    return VVoid
printInt _ p = throwError $ "bad argument count in print function at: " ++ showPos p -- checked by typechecker, included here for complete pattern matching

printBool :: [Expr] -> BNFC'Position -> Interpreter Var
printBool [expr] p = do
    b <- evalBool expr p
    liftIO $ putStr $ show b ++ "\n"
    return VVoid
printBool _ p = throwError $ "bad argument count in print function at: " ++ showPos p -- checked by typechecker, included here for complete pattern matching


noInitVar :: Type -> Var
noInitVar (Int _) = VInt 0
noInitVar (Str _) = VString ""
noInitVar (Bool _) = VBool False
noInitVar (Void _) = VVoid
noInitVar Fun {} = error "cannot declare function as variable"

mapInsertOrAppend :: Ident -> Var -> BlockPos -> Interpreter ()
mapInsertOrAppend ident var b = do
    varMap <- get
    case Map.lookup ident varMap of
        Nothing -> modify (Map.insert ident [(var, b)])
        Just vs -> modify (Map.insert ident ((var, b):vs))

execDecl :: BlockPos -> BNFC'Position -> Type -> [Item] -> Bool -> Interpreter ()
execDecl _ p t [] ro = throwError $  "empty declaration at " ++ showPos p
execDecl b p t [NoInit p' x] ro = mapInsertOrAppend x (noInitVar t) b
execDecl b p t [Init p' x expr] ro = do
    var <- evalExpr expr
    mapInsertOrAppend x var b

execDecl b p t (x:xs) ro = do
    execDecl b p t [x] ro
    execDecl b p t xs ro

execStmts :: BlockPos -> [Stmt] -> Interpreter ()
execStmts _ [] = return ()
execStmts b (s:sx) = do
        execStmt b s
        env <- get
        case Map.lookup returnValue env of
            Nothing -> execStmts b sx
            Just _ -> return ()

execStmt :: BlockPos -> Stmt -> Interpreter ()
execStmt b (Decl p t x) = execDecl b p t x False
execStmt b (ConstDecl p t x) =  execDecl b p t x True

execStmt _ (Empty p) = return ()
execStmt _ (BStmt p (Block p' [])) = return ()
execStmt b (BStmt p (Block newBlock stmts)) = do
    execStmts newBlock stmts
    modify (Map.fromList . Prelude.map (removeInnerDeclarations newBlock) . toList)
    where
        removeInnerDeclarations :: BlockPos -> (Ident, [(Var, BlockPos)]) -> (Ident, [(Var, BlockPos)])
        removeInnerDeclarations b (ident, ds) = (ident, cutOutBlockDeclarations ds b)

        cutOutBlockDeclarations :: [(Var, BlockPos)] -> BlockPos -> [(Var, BlockPos)]
        cutOutBlockDeclarations [] _ = []
        cutOutBlockDeclarations (vi@(v, x) : xs) y
            | x == y = cutOutBlockDeclarations xs y
            | otherwise = vi : cutOutBlockDeclarations xs y

execStmt _ (Ass p ident expr) = do
    var <- evalExpr expr
    env <- get
    case Map.lookup ident env of
        Just ((_, b') : vs) -> modify (Map.insert ident ((var, b') : vs))
        _ -> throwError $ "assignment to undeclared variable: " ++ show ident ++ " at " ++ showPos p

execStmt b (Cond p expr stmt) = do
    cond <- evalBool expr p
    when cond $ execStmt b stmt

execStmt b (CondElse p expr stmt1 stmt2) = do
    cond <- evalBool expr p
    if cond then execStmt b stmt1 else execStmt b stmt2

execStmt b w@(While p expr stmt) = do
    cond <- evalBool expr p
    when cond $ do
        execStmt b stmt
        execStmt b w

execStmt b (For p loopVar startExpr endExpr stmt) = do
    startVal <- evalInteger startExpr p
    endVal <- evalInteger endExpr p
    forLoop stmt startVal endVal where

    forLoop :: Stmt -> Integer -> Integer -> Interpreter ()
    forLoop s iter end =
        when (iter <= end) $ do
            setIterator (VInt iter)
            execStmt b s
            forLoop s (iter + 1) end

    setIterator :: Var -> Interpreter ()
    setIterator iterVal = do
        env <- get
        case Map.lookup loopVar env of
            Just (y:ys) -> modify (Map.insert loopVar ((iterVal, b):ys))
            _ -> modify (Map.insert loopVar [(iterVal, b)])

execStmt b (SExp p expr) = do
    evalExpr expr
    return ()

execStmt b (Ret p expr) = do
    var <- evalExpr expr
    modify (Map.insert returnValue [(var, returnBlockPos)])
    return ()
execStmt b (VRet p) = do
    modify (Map.insert returnValue [(VVoid, returnBlockPos)])
    return ()

execDef :: TopDef -> Interpreter ()
execDef (FnDef p retT f args block) = do
    modify (Map.insert (funcIdent f) [(VFunction function, p)])
    where
        getFunctionArgs :: BNFC'Position -> [Arg] -> [Expr] -> Interpreter () -- count and type correctness also checked in typechecker, included here also because it's natural and easy
        getFunctionArgs p [] [] = return ()
        getFunctionArgs p [] e = throwError $ "incorrect argument count at: " ++ showPos p
        getFunctionArgs p a [] = throwError $ "incorrect argument count at: " ++ showPos p
        getFunctionArgs p (a:as) (e:es) = do
            v <- evalExpr e
            case (a,v) of
                (Arg _ (Int _) ident, VInt v) -> do
                    mapInsertOrAppend ident (VInt v) p
                    getFunctionArgs p as es
                (Arg _ (Str _) ident, VString v) -> do
                    mapInsertOrAppend ident (VString v) p
                    getFunctionArgs p as es
                (Arg _ (Bool _) ident, VBool v) -> do
                    mapInsertOrAppend ident (VBool v) p
                    getFunctionArgs p as es
                (Arg _ _ ident, _) -> throwError $ "incorrect argument type for argument: " ++ show ident ++ " at: " ++ showPos p

        function :: [Expr] -> BNFC'Position -> Interpreter Var
        function xs p' = do
            initialEnv <- get
            modify (Map.delete returnValue)
            getFunctionArgs p' args xs
            execStmt p' (BStmt p' block)
            postStmtsEnv <- get
            modify (const initialEnv) -- returning to original, pre-function-call environment
            case Map.lookup returnValue postStmtsEnv of
                (Just ((rVal, _):_)) -> return rVal
                _ -> throwError $ "function: " ++ show f ++ " didn't call return at: " ++ showPos p'

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
        (Just ((VFunction f, _):_)) -> do
            f [] p
            return ()
        _ -> throwError "no 'main' function defined" -- also checked in typechecker, here mainly for full pattern matching


execProgram :: Program -> IO ()
execProgram prog@(Program p defs) = do
    let initTypeCheckEnv = Map.fromList
          [(funcIdent $ Ident "printInt", [TypeInfo (TFunction [(TInt, Ident "toPrint")] TVoid (Block p [VRet p])) True p])
          , (funcIdent $ Ident "printString", [TypeInfo (TFunction [(TString, Ident "toPrint")] TVoid (Block p [VRet p])) True p])
          , (funcIdent $ Ident "printBool", [TypeInfo (TFunction [(TBool, Ident "toPrint")] TVoid (Block p [VRet p])) True p])]
    typeCheck <- runExceptT $ runStateT (execTypeCheck p defs) initTypeCheckEnv
    case typeCheck of
        Left error -> hPutStrLn stderr $ "type check error: " ++ error
        Right _ -> 
            do
            let initEnv = Map.fromList [(funcIdent $ Ident "printInt", [(VFunction printInt, p)]), (funcIdent $ Ident "printString", [(VFunction printString, p)]), (funcIdent $ Ident "printBool", [(VFunction printBool, p)])]
            result <- runExceptT $ runStateT (execDefsThenMain p defs) initEnv

            case result of
                Left error -> hPutStrLn stderr $ "runtime error: " ++ error
                Right _ -> return ()
