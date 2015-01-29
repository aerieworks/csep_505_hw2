module InterpFun where

import Expr
import Result

-- Values resulting from interpreting an expression.
data Val = NumV Integer
         | BoolV Bool
         | FunV Var CExpr Env
         | PrimV String (Val -> Result Val)  -- name and implementation

type Env = [(Var, Val)]

instance Show Val where
  show (NumV n) = show n
  show (BoolV b) = show b
  show (FunV var body env) = "(fun (" ++ var ++ ") " ++ (show body) ++ " | " ++
                             (show env) ++ ")"
  show (PrimV name impl) = "<primitive: " ++ name ++ ">"

wrapBinaryArithOp :: String -> (Integer -> Integer -> Val) -> Val
wrapBinaryArithOp name op =
  PrimV name (
    \arg1 -> return (PrimV ("partial:" ++ name)
                     (\arg2 ->
                       case (arg1, arg2) of
                        (NumV lv, NumV rv) -> return (op lv rv)
                        nonNum -> fail ("numeric op applied to: " ++
                                        (show nonNum)))))

-- Populate initialEnv ...
initialEnv :: Env
initialEnv = [
  ("true", BoolV True),
  ("false", BoolV False),
  ("+", wrapBinaryArithOp "addition" (\x y -> NumV (x + y))),
  ("*", wrapBinaryArithOp "multiplication" (\x y -> NumV (x * y))),
  ("=", wrapBinaryArithOp "equals" (\x y -> BoolV (x == y))),
  ("<", wrapBinaryArithOp "less than" (\x y -> BoolV (x < y)))
  ]

interp :: CExpr -> Env -> Result Val
interp expr env =
  case expr of
    NumC num          -> Ok (NumV num)
    FunC v body       -> Ok (FunV v body env)
    VarC var          -> interpVariable var env
    IfC test cons alt -> interpIf test cons alt env
    AppC func arg     -> interpApplication func arg env

interpVariable :: Var -> Env -> Result Val
interpVariable var env =
  case lookup var env of
    Nothing  -> Err ("Variable `" ++ (show var) ++ "` is undefined.")
    Just val -> Ok val

interpIf :: CExpr -> CExpr -> CExpr -> Env -> Result Val
interpIf testExpr consExpr altExpr env =
  interp testExpr env >>= \x ->
    case x of
      BoolV True  -> interp consExpr env
      BoolV False -> interp altExpr env
      otherwise   -> syntaxErr "if conditional" "boolean expression" x

interpApplication :: CExpr -> CExpr -> Env -> Result Val
interpApplication funcExpr argExpr env =
  do func <- interp funcExpr env
     arg  <- interp argExpr env
     case func of
       FunV var body fenv -> interp body ((var, arg):fenv)
       PrimV _ body       -> body arg
       otherwise          -> syntaxErr "application" "function or primitive" func

interpStr :: String -> Result Val
interpStr input =
  let env      = initialEnv
      bound    = map (\x -> fst x) env
      reserved = ("if":"fun":bound)
  in desugarStr input >>= (\expr ->
    checkIds bound reserved expr >>= (\_ -> interp expr env))
