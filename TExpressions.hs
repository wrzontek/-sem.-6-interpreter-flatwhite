module TExpressions where
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import AbsFlatwhite
import Types

getExprType :: Expr -> TypeChecker TType
getExprType (ELitInt _ _) = return TInt
getExprType (EString _ s) = return TString
getExprType (ELitTrue _)  = return TBool
getExprType (ELitFalse _) = return TBool

getExprType (EVar p ident) = do
    vars <- get
    case Map.lookup ident vars of
        Just (TypeInfo (TFunction _) _) -> throwError $ "Attempt to eval function as variable at: " ++ showPos p
        Just (TypeInfo t _) -> return t
        _ -> throwError $ "Undefined variable " ++ show ident ++ " at: " ++ showPos p

getExprType (EApp p ident args) = do
    vars <- get
    case Map.lookup (funcIdent ident) vars of
        Just (TypeInfo (TFunction f) _) -> f args p 
        _ -> throwError $ "Undefined function" ++ show ident ++ " at: " ++ showPos p

getExprType (Neg p e) = do
    t <- getExprType e
    case t of
        TInt -> return TInt
        _ -> throwError $ "Cannot negate expression at: " ++ showPos p
getExprType (Not p e) = do
    t <- getExprType e
    case t of
        TBool -> return TBool
        _ -> throwError $ "Cannot 'not' expression at: " ++ showPos p

getExprType (EMul p e1 op e2) = do
    t1 <- getExprType e1
    t2 <- getExprType e2
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        _ -> throwError $ "Integer operation on non-integer at: " ++ showPos p

getExprType (EAdd p e1 (Plus _) e2) = do
    t1 <- getExprType e1
    t2 <- getExprType e2
    case (t1, t2) of
        (TInt, TInt) ->  return TInt
        (TString, TString) -> return TString
        _ -> throwError $ "Illegal add operation on-adding types at:  " ++ showPos p

getExprType (EAdd p e1 (Minus _) e2) = do
    t1 <- getExprType e1
    t2 <- getExprType e2
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        _ -> throwError $ "Integer operation on non-integer at: " ++ showPos p

getExprType (ERel p e1 op e2) = do
    t1 <- getExprType e1
    t2 <- getExprType e2
    case (t1, t2) of
        (TInt, TInt) -> return TBool
        (TBool, TBool) -> case op of
            (EQU _) -> return TBool
            (NE _) -> return TBool
            _ -> throwError $ "Illegal comparison of Booleans at " ++ showPos p
        _ -> throwError $ "Integer/Boolean operation on non-integer/non-boolean at: " ++ showPos p

getExprType (EAnd p e1 e2) = do
    t1 <- getExprType e1
    t2 <- getExprType e2
    case (t1, t2) of
        (TBool, TBool) -> return TBool
        _ -> throwError $ "Boolean operation on non-boolean at: " ++ showPos p

getExprType (EOr p e1 e2) = do
    t1 <- getExprType e1
    t2 <- getExprType e2
    case (t1, t2) of
        (TBool, TBool) -> return TBool
        _ -> throwError $  "Boolean operation on non-boolean at: " ++ showPos p
