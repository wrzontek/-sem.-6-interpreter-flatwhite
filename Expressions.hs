module Expressions where
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import AbsFlatwhite
import Types

showPos :: BNFC'Position -> [Char]
showPos (Just x) = show x
showPos _ = "?"

evalBool :: Expr -> BNFC'Position -> Interpreter Bool
evalBool e p = do
  v <- evalExpr e
  case v of
    VBool b -> return b
    _ -> throwError $ "Expected Boolean at " ++ showPos p

evalInteger :: Expr -> BNFC'Position -> Interpreter Integer
evalInteger e p = do
  v <- evalExpr e
  case v of
    VInt n -> return n
    _ -> throwError $ "Expected Integer at " ++ showPos p

evalString :: Expr -> BNFC'Position -> Interpreter String
evalString e p = do
  v <- evalExpr e
  case v of
    VString s -> return s
    _ -> throwError $ "Expected String at " ++ showPos p


mulOpToFunction :: MulOp -> (Integer -> Integer -> Integer)
mulOpToFunction (Times _) a b = a * b
mulOpToFunction (Div _) a b = div a b
mulOpToFunction (Mod _) a b = mod a b

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
relOpToBooleanFunction _ p = throwError $ "Illegal comparison of Booleans at " ++ showPos p

evalExpr :: Expr -> Interpreter Var
evalExpr (ELitInt _ n) = return (VInt n)
evalExpr (EString _ s) = return (VString s)
evalExpr (ELitTrue _)  = return (VBool True)
evalExpr (ELitFalse _) = return (VBool False)

evalExpr (EVar p ident) = do
    vars <- get
    case Map.lookup ident vars of
        Just (VarInfo (VFunction _) _) -> throwError $ "Attempt to eval function as variable at: " ++ showPos p
        Just (VarInfo v _) -> return v
        _ -> throwError $ "Undefined variable " ++ show ident ++ " at: " ++ showPos p

evalExpr (EApp p ident args) = do
    vars <- get
    case Map.lookup ident vars of
        Just (VarInfo (VFunction f) p') -> f args p
        _ -> throwError $ "Undefined function" ++ show ident ++ " at: " ++ showPos p

evalExpr (Neg p e) = do
    e' <- evalExpr e
    case e' of
        (VInt n) -> return $ VInt $ negate n
        _ -> throwError $ "Cannot negate expression at: " ++ showPos p --chyba wyjdzie w TypeCheckerze
evalExpr (Not p e) = do
    e' <- evalExpr e
    case e' of
        (VBool b) -> return $ VBool $ not b
        _ -> throwError $ "Cannot 'not' expression at: " ++ showPos p  --chyba wyjdzie w TypeCheckerze

evalExpr (EMul p e1 op e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of
        (VInt a, VInt b) -> return (VInt $ mulOpToFunction op a b)
        _ -> throwError $ "Integer operation on non-integer at: " ++ showPos p --chyba wyjdzie w TypeCheckerze

evalExpr (EAdd p e1 (Plus _) e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of
        (VInt a, VInt b) ->  return (VInt $ a + b)
        (VString a, VString b) -> return (VString $ a ++ b)
        _ -> throwError $ "Illegal add operation on-adding types at:  " ++ showPos p --chyba wyjdzie w TypeCheckerze

evalExpr (EAdd p e1 (Minus _) e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of
        (VInt a, VInt b) -> return (VInt $ a - b)
        _ -> throwError $ "Integer operation on non-integer at: " ++ showPos p --chyba wyjdzie w TypeCheckerze

evalExpr (ERel p e1 op e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of
        (VInt a, VInt b) -> return (VBool $ relOpToIntegerFunction op a b)
        (VBool a, VBool b) -> do
            op' <- relOpToBooleanFunction op p
            return (VBool $ op' a b)
        _ -> throwError $ "Integer/Boolean operation on non-integer/non-boolean at: " ++ showPos p --chyba wyjdzie w TypeCheckerze

evalExpr (EAnd p e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of
        (VBool a, VBool b) -> return (VBool $ a && b)
        _ -> throwError $ "Boolean operation on non-boolean at: " ++ showPos p -- chyba wyjdzie w TypeCheckerze
evalExpr (EOr p e1 e2) = do
    e1' <- evalExpr e1
    e2' <- evalExpr e2
    case (e1', e2') of
        (VBool a, VBool b) -> return (VBool $ a || b)
        _ -> throwError $  "Boolean operation on non-boolean at: " ++ showPos p --chyba wyjdzie w TypeCheckerze