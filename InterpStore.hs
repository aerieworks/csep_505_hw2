module InterpStore where

import Control.Applicative
import Control.Monad
import Expr
import Result

type Loc = Int

type Store = (Loc, [(Loc, Val)])

-- Store transformer that carries a Result of type a. If we get an
-- error, both the error and the store (at the point where the error
-- occurred) are propagated; the contents of the store may be useful
-- for debugging.
newtype STR a = STR (Store -> (Result a, Store))

alloc :: Val -> STR Loc
alloc val = fail "need to implement alloc"

fetch :: Loc -> STR Val
fetch loc = fail "need to implement lookup"

update :: Loc -> Val -> STR ()
update loc val = fail "need to implement update"

instance Functor STR where
  fmap f st = st >>= return . f

instance Monad STR where
  -- (>>=) :: STR a -> (a -> STR b) -> STR b
  (STR st) >>= f = fail "(>>=) not yet implemented for STR"

  -- return :: a -> STR a
  return v = fail "'return' not yet implemented for STR"

  fail msg = STR (\s -> (Err msg, s))

instance Applicative STR where
  pure = return
  (<*>) = ap

-- Values resulting from interpreting an expression.
data Val = NumV Integer
         | BoolV Bool
         | FunV Var CExpr Env
         | PrimV String (Val -> STR Val)
         | BoxV Loc

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (FunV var body env) = "(fun (" ++ var ++ ") " ++ (show body) ++ " | " ++
                             (show (map fst env)) ++ ")"
  
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"
  show (BoxV loc) = "<box@" ++ (show loc) ++ ">"

-- Populate initialEnv...
initialEnv :: Env
initialEnv = []

interp :: CExpr -> Env -> STR Val
interp expr env = fail "'interp' not yet implemented"
