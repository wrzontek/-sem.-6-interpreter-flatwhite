module Types where
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import AbsFlatwhite
import Control.Monad.State

returnValue :: Ident
returnValue = Ident "__return_value__"

data Var =
    VInt Integer
    | VBool Bool
    | VString String
    | VFunction ([Expr] -> BNFC'Position -> Interpreter Var)
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

type ExceptionT = ExceptT String IO
type InterpreterEnv = Map.Map Ident Var
type Interpreter = StateT InterpreterEnv ExceptionT

data TType  = -- Type but without pos as it's not needed
    TInt
    | TBool
    | TString
    | TFunction ([Expr] -> BNFC'Position -> TypeChecker TType)
    | TVoid

instance Eq TType where
  TInt == TInt = True
  TBool == TBool = True
  TString == TString = True
  TVoid == TVoid = True
  _ == _ = False

data TypeInfo = TypeInfo TType Bool -- variable type and whether or not it's readonly

type TypeCheckerEnv = Map.Map Ident TypeInfo
type TypeChecker = StateT TypeCheckerEnv ExceptionT

showPos :: BNFC'Position -> [Char]
showPos (Just x) = show x
showPos _ = "?"

funcIdent :: Ident -> Ident -- so variable names dont conflict with function names
funcIdent (Ident f) = Ident $ "__function__" ++ f ++ "__"