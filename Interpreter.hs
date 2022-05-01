module Interpreter where
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as DataMap
import AbsFlatwhite

-- todo printInt, printBool i printString, pewnie oddzielny moduł

data Var =
    VInt Integer
    | VBool Bool
    | VString String
    | VFunction ([Expr] -> Var) -- todo
    | VVoid

instance Eq Var where
  (VInt x) == (VInt y) = x == y
  (VBool x) == (VBool y) = x == y
  (VString x) == (VString y) = x == y
  VVoid == VVoid = True
  _ == _ = False

instance Show Var where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VString s) = s
  show (VFunction _) = "function"
  show VVoid = "void"

data VarInfo = VarInfo Var Bool -- variable and whether or not it's readonly

-- instance Eq VarInfo where
--     (VarInfo v b) == (VarInfo v' b') = (v == v') && (b == b')

type Env = DataMap.Map Ident VarInfo
type IExcept = ExceptT String IO
type Interpreter = ReaderT Env IExcept


evalBool :: Expr -> Interpreter Bool
evalBool e = do
  v <- evalExpr e
  case v of
    VBool b -> return b
    _ -> throwError "Expected Bool." -- todo position

-------------- wydzielić te głupoty nudne do osobnego modułu ----------------------------------

mulOpToFunction :: MulOp -> (Integer -> Integer -> Integer)
mulOpToFunction (Times _) a b = a * b
mulOpToFunction (Div _) a b = div a b
mulOpToFunction (Mod _) a b = mod a b 

addOpToFunction :: AddOp -> (Integer -> Integer -> Integer)
addOpToFunction (Plus _) a b = a + b
addOpToFunction (Minus _) a b = a - b

relOpToIntegerFunction :: RelOp -> (Integer -> Integer -> Bool)
-- LTH a | LE a | GTH a | GE a | EQU a | NE a
relOpToIntegerFunction (LTH _) a b = a < b
relOpToIntegerFunction (LE _) a b = a <= b
relOpToIntegerFunction (GTH _) a b = a > b
relOpToIntegerFunction (GE _) a b = a >= b
relOpToIntegerFunction (EQU _) a b = a == b
relOpToIntegerFunction (NE _) a b = a /= b


relOpToBooleanFunction :: RelOp -> (Bool -> Bool -> Bool)
relOpToBooleanFunction (EQU _) a b = a == b
relOpToBooleanFunction (NE _) a b = a /= b
relOpToBooleanFunction _ _ _ = error "Illegal comparison of Booleans" -- zwykły error, ale do tego i tak nie dojdzie przez wcześniejsze sprawdzenie

----------------------------------------------------------------------------------------------------


evalExpr :: Expr -> Interpreter Var
evalExpr (ELitInt _ n) = return (VInt n)
evalExpr (EString _ s) = return (VString s)
evalExpr (ELitTrue _)  = return (VBool True)
evalExpr (ELitFalse _) = return (VBool False)

evalExpr (EVar p ident) = do
    vars <- ask
    case DataMap.lookup ident vars of
        Just (VarInfo v _) -> return v
        _ -> throwError "Undefined variable" -- todo position

evalExpr (EApp p fIdent args) = do
    vars <- ask
    case DataMap.lookup fIdent vars of
        Just (VarInfo (VFunction f) p') -> return $ f args
        _ -> throwError "Undefined function" -- todo position

evalExpr (Neg p e) = do
    e' <- evalExpr e
    case e' of
        (VInt n) -> return $ VInt $ negate n
        _ -> throwError "Cannot negate expression" -- todo position, chyba wyjdzie w TypeCheckerze
evalExpr (Not p e) = do
    e' <- evalExpr e
    case e' of
        (VBool b) -> return $ VBool $ not b
        _ -> throwError "Cannot 'not' expression" -- todo position i język angielski, chyba wyjdzie w TypeCheckerze
evalExpr (EMul p e1 op e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case e1' of
        (VInt a) -> case e2' of
            (VInt b) -> return (VInt $ mulOpToFunction op a b)
            _ -> throwError "Integer operation on non-integer" -- todo position, chyba wyjdzie w TypeCheckerze
        _ -> throwError "Integer operation on non-integer" -- todo position, chyba wyjdzie w TypeCheckerze
evalExpr (EAdd p e1 op e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case e1' of
        (VInt a) -> case e2' of
            (VInt b) -> return (VInt $ addOpToFunction op a b)
            _ -> throwError "Integer operation on non-integer" -- todo position, chyba wyjdzie w TypeCheckerze
        _ -> throwError "Integer operation on non-integer" -- todo position, chyba wyjdzie w TypeCheckerze

evalExpr (ERel p e1 op e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case e1' of
        (VInt a) -> case e2' of
            (VInt b) -> return (VBool $ relOpToIntegerFunction op a b)
            _ -> throwError "Integer operation on non-integer" -- todo position, chyba wyjdzie w TypeCheckerze
        (VBool a) -> case e2' of
            (VBool b) -> return (VBool $ relOpToBooleanFunction op a b)
            _ -> throwError "Boolean operation on non-boolean" -- todo position, chyba wyjdzie w TypeCheckerze
        _ -> throwError "Integer/Boolean operation on non-integer/non-boolean" -- todo position, chyba wyjdzie w TypeCheckerze
    -- | ERel a (Expr' a) (RelOp' a) (Expr' a)
    -- | EAnd a (Expr' a) (Expr' a)
    -- | EOr a (Expr' a) (Expr' a)
evalExpr _ = undefined


-- Executes the program
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

-- -- Executes the program
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