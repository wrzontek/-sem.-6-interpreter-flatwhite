module Expressions where
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import AbsFlatwhite
import Control.Monad.State (execState)
import Types

-- todo printInt, printBool i printString, pewnie oddzielny moduł

evalBool :: Expr -> BNFC'Position -> Interpreter Bool
evalBool e p = do
  v <- evalExpr e
  case v of
    VBool b -> return b
    _ -> throwError $ "Expected Boolean at " ++ show p

evalInteger :: Expr -> BNFC'Position -> Interpreter Integer
evalInteger e p = do
  v <- evalExpr e
  case v of
    VInt n -> return n
    _ -> throwError $ "Expected Integer at " ++ show p


mulOpToFunction :: MulOp -> (Integer -> Integer -> Integer)
mulOpToFunction (Times _) a b = a * b
mulOpToFunction (Div _) a b = div a b
mulOpToFunction (Mod _) a b = mod a b 

addOpToFunction :: AddOp -> (Integer -> Integer -> Integer)
addOpToFunction (Plus _) a b = a + b
addOpToFunction (Minus _) a b = a - b

relOpToIntegerFunction :: RelOp -> (Integer -> Integer -> Bool)
relOpToIntegerFunction (LTH _) a b = a < b
relOpToIntegerFunction (LE _) a b = a <= b
relOpToIntegerFunction (GTH _) a b = a > b
relOpToIntegerFunction (GE _) a b = a >= b
relOpToIntegerFunction (EQU _) a b = a == b
relOpToIntegerFunction (NE _) a b = a /= b

relOpToBooleanFunction :: RelOp -> BNFC'Position -> Interpreter (Bool -> Bool -> Bool)
relOpToBooleanFunction (EQU _) _ = return equals where equals a b = a == b
relOpToBooleanFunction (NE _) _ = return notEquals where notEquals a b = a /= b
relOpToBooleanFunction _ p = throwError $ "Illegal comparison of Booleans at " ++ show p

evalExpr :: Expr -> Interpreter Var
evalExpr (ELitInt _ n) = return (VInt n)
evalExpr (EString _ s) = return (VString s)
evalExpr (ELitTrue _)  = return (VBool True)
evalExpr (ELitFalse _) = return (VBool False)

evalExpr (EVar p ident) = do
    vars <- ask
    case Map.lookup ident vars of
        Just (VarInfo v _) -> return v -- todo czy ma sens z funkcjami? chyba musi być EApp
        _ -> throwError $ "Undefined variable " ++ show ident ++ " at " ++ show p

evalExpr (EApp p ident args) = do
    vars <- ask
    case Map.lookup ident vars of
        Just (VarInfo (VFunction f) p') -> return $ f args -- todo
        _ -> throwError $ "Undefined function" ++ show ident ++ " at " ++ show p 

evalExpr (Neg p e) = do
    e' <- evalExpr e
    case e' of
        (VInt n) -> return $ VInt $ negate n
        _ -> throwError $ "Cannot negate expression at " ++ show p --chyba wyjdzie w TypeCheckerze
evalExpr (Not p e) = do
    e' <- evalExpr e
    case e' of
        (VBool b) -> return $ VBool $ not b
        _ -> throwError $ "Cannot 'not' expression at " ++ show p  --chyba wyjdzie w TypeCheckerze
evalExpr (EMul p e1 op e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case e1' of
        (VInt a) -> case e2' of
            (VInt b) -> return (VInt $ mulOpToFunction op a b)
            _ -> throwError $ "Integer operation on non-integer at " ++ show p --chyba wyjdzie w TypeCheckerze
        _ -> throwError $ "Integer operation on non-integer at " ++ show p --chyba wyjdzie w TypeCheckerze
evalExpr (EAdd p e1 op e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case e1' of
        (VInt a) -> case e2' of
            (VInt b) -> return (VInt $ addOpToFunction op a b)
            _ -> throwError $ "Integer operation on non-integer at " ++ show p --chyba wyjdzie w TypeCheckerze
        _ -> throwError $ "Integer operation on non-integer at " ++ show p --chyba wyjdzie w TypeCheckerze

evalExpr (ERel p e1 op e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case e1' of
        (VInt a) -> case e2' of
            (VInt b) -> return (VBool $ relOpToIntegerFunction op a b)
            _ -> throwError $ "Integer operation on non-integer at " ++ show p --chyba wyjdzie w TypeCheckerze
        (VBool a) -> do
            op' <- relOpToBooleanFunction op p 
            case e2' of
                (VBool b) -> return (VBool $ op' a b)
                _ -> throwError $ "Boolean operation on non-boolean at " ++ show p --chyba wyjdzie w TypeCheckerze
        _ -> throwError $ "Integer/Boolean operation on non-integer/non-boolean at " ++ show p --chyba wyjdzie w TypeCheckerze

evalExpr (EAnd p e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case e1' of
        (VBool a) -> 
            case e2' of
                (VBool b) -> return (VBool $ a && b)
                _ -> throwError $ "Boolean operation on non-boolean at " ++ show p -- chyba wyjdzie w TypeCheckerze
        _ -> throwError $ "Boolean operation on non-boolean at " ++ show p -- chyba wyjdzie w TypeCheckerze
evalExpr (EOr p e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case e1' of
        (VBool a) -> 
            case e2' of
                (VBool b) -> return (VBool $ a || b)
                _ -> throwError $ "Boolean operation on non-boolean at" ++ show p --chyba wyjdzie w TypeCheckerze
        _ -> throwError $  "Boolean operation on non-boolean at" ++ show p --chyba wyjdzie w TypeCheckerze