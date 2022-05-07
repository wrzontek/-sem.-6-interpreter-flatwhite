module Types where
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as DataMap
import AbsFlatwhite
import Control.Monad.State (execState)
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

type Env = DataMap.Map Ident VarInfo
type IExcept = ExceptT String IO
type Interpreter = ReaderT Env IExcept