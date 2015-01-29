module InterpStore where

import Control.Applicative
import Control.Monad
import Expr
import Result
import Data.Typeable

type Loc = Int

type Store = (Loc, [(Loc, Val)])

-- Store transformer that carries a Result of type a. If we get an
-- error, both the error and the store (at the point where the error
-- occurred) are propagated; the contents of the store may be useful
-- for debugging.
newtype STR a = STR (Store -> (Result a, Store))

alloc :: Val -> STR Loc
alloc val = STR (\store ->
  let (loc, contents) = store
  in (Ok loc, (loc + 1, (loc, val):contents))
  )

fetch :: Loc -> STR Val
fetch loc = STR (\store ->
  case lookup loc (snd store) of
    Just val -> (Ok val, store)
    Nothing  -> (Err ("Segmentation fault (Attempt to read from unallocated location: `" ++ show loc ++ "`)"), store)
  )

update :: Loc -> Val -> STR ()
update loc val = STR (\store ->
  let (next, contents) = store
  in case lookup loc contents of
       Just _   -> (Ok (), (next, (loc, val):contents))
       Nothing  -> (Err ("Segmentation fault (Attempt to write to unallocated location: `" ++ show loc ++ "`)"), store)
  )

instance Functor STR where
  fmap f st = st >>= return . f

instance Monad STR where
  -- (>>=) :: STR a -> (a -> STR b) -> STR b
  (STR st) >>= f = STR (\store ->
    case st store of
      (Ok v, store')    -> let STR x = f v in x store'
      (Err msg, store') -> (Err msg, store')
    )

  -- return :: a -> STR a
  return v = STR (\store -> (Ok v, store))

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
