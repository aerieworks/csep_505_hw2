module InterpStore where

import Control.Applicative
import Control.Monad
import Expr
import Result
import Data.Typeable
import System.IO

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
    Nothing  -> (Err ("Segmentation fault ;) (Attempt to read from unallocated location: `" ++ show loc ++ "`)"), store)
  )

update :: Loc -> Val -> STR ()
update loc val = STR (\store ->
  let (next, contents) = store
  in case lookup loc contents of
       Just _   -> (Ok (), (next, (loc, val):contents))
       Nothing  -> (Err ("Segmentation fault ;) (Attempt to write to unallocated location: `" ++ show loc ++ "`)"), store)
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

errTR :: String -> STR a
errTR msg = STR (\st -> (Err msg, st))

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 -> return (PrimV ("partial:" ++ name)
                     (\arg2 ->
                       case (arg1, arg2) of
                        (NumV lv, NumV rv) -> return (op lv rv)
                        nonNum -> errTR ("numeric op applied to: `" ++ show nonNum ++ "`"))))

-- Populate initialEnv...
initialEnv :: Env
initialEnv = [
  ("true", BoolV True),
  ("false", BoolV False),
  ("box", PrimV "box" (\x -> alloc x >>= (\loc -> return (BoxV loc)))),
  ("unbox", PrimV "unbox" (\box ->
    case box of
      BoxV loc  -> fetch loc
      otherwise -> errTR ("Cannot unbox non-box value `" ++ show box ++ "`"))
    ),
  ("set-box!", PrimV "set-box!" (\box ->
    case box of
      BoxV loc  -> fetch loc >>= (\_ ->
        return (PrimV "partial:set-box!" (\val -> update loc val >>= (\_ -> return val))))
      otherwise -> errTR ("Cannot set non-box value `" ++ show box ++ "`"))
    ),
  ("+", wrapBinaryArithOp "addition" (\x y -> NumV (x + y))),
  ("*", wrapBinaryArithOp "multiplication" (\x y -> NumV (x * y))),
  ("=", wrapBinaryArithOp "equals" (\x y -> BoolV (x == y))),
  ("<", wrapBinaryArithOp "less than" (\x y -> BoolV (x < y)))
  ]

interp :: CExpr -> Env -> STR Val
interp expr env =
  case expr of
    NumC num          -> return (NumV num)
    FunC v body       -> return (FunV v body env)
    VarC var          -> case lookup var env of
                           Nothing  -> fail ("Variable`" ++ show var ++ "` is undefined.")
                           Just val -> return val
    IfC test cons alt -> interpIf test cons alt env
    AppC func arg     -> interpApplication func arg env

interpIf :: CExpr -> CExpr -> CExpr -> Env -> STR Val
interpIf testExpr consExpr altExpr env =
  interp testExpr env >>= \x ->
    case x of
      BoolV True  -> interp consExpr env
      BoolV False -> interp altExpr env
      otherwise   -> fail (syntaxErrMsg "if conditional" "boolean expression" x)

interpApplication :: CExpr -> CExpr -> Env -> STR Val
interpApplication funcExpr argExpr env =
  do func <- interp funcExpr env
     arg  <- interp argExpr env
     case func of
       FunV var body fenv -> interp body ((var, arg):fenv)
       PrimV _ body       -> body arg
       otherwise          -> fail (syntaxErrMsg "application" "function or primitive" func)

interpStr :: String -> Result Val
interpStr input =
  let env      = initialEnv
      bound    = map (\x -> fst x) env
      reserved = ("if":"fun":bound)
  in do expr <- desugarStr input
        _    <- checkIds bound reserved expr
        let STR tr = interp expr env in fst (tr (0, []))

interpFile :: String -> IO ()
interpFile filename =
  do input <- readFile filename
     putStr (show (interpStr input) ++ "\n")

