module Expr (Var, Expr(..), CExpr(..), parseExpr, desugar, checkIds, syntaxErr, syntaxErrMsg, parseStr, desugarStr, checkStr) where

import Data.List
import Result
import SExp

type Var = String

syntaxErr :: (Show a) => String -> String -> a -> Result b
syntaxErr container expected found = Err (syntaxErrMsg container expected found)

syntaxErrMsg :: (Show a) => String -> String -> a -> String
syntaxErrMsg container expected found =
  ("Invalid syntax in `" ++ container ++ "`: expected " ++ expected ++ " but found `" ++ (show found) ++ "`")

-- Expression syntax:
-- <e> ::= <number>
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x ...) <e>)
--       | (<e> <e> ...)
--       | (with* ([x <e>] ...) <e>)
data Expr = NumE Integer
          | IfE Expr Expr Expr
          | VarE Var
          | FunE [Var] Expr
          | AppE [Expr]
          | WithStarE [(Var, Expr)] Expr
          deriving (Eq, Show)

-- Core language (desugared from Expr).
-- <e> ::= <number>
--       | (if <e> <e> <e>)
--       | x
--       | (fun (x) <e>)
--       | (<e> <e>)
data CExpr = NumC Integer
           | IfC CExpr CExpr CExpr
           | VarC Var
           | FunC Var CExpr
           | AppC CExpr CExpr
           deriving (Eq, Show)

parseExpr :: SExp -> Result Expr
parseExpr sexp =
  case sexp of
    NumS n                    -> Ok (NumE n)
    IdS v                     -> Ok (VarE v)
    ListS ((IdS "if"):ses)    -> parseIfE ses
    ListS ((IdS "with*"):ses) -> parseWithStarE ses
    ListS ((IdS "fun"):ses)   -> parseFunE ses
    ListS ses                 -> parseAppE ses

parseIfE :: [SExp] -> Result Expr
parseIfE args =
  case args of
    testSExp:consSExp:altSExp:[] ->
      do test  <- parseExpr testSExp
         cons  <- parseExpr consSExp
         alt   <- parseExpr altSExp
         Ok (IfE test cons alt)
    otherwise -> syntaxErr "if" "test, consequent, and alternate expressions" args

parseWithStarE :: [SExp] -> Result Expr
parseWithStarE args =
  case args of
    (ListS varSExps):bodySExp:[] ->
      do vars <- parseWithVars varSExps []
         body <- parseExpr bodySExp
         Ok (WithStarE vars body)
    otherwise -> syntaxErr "with*" "variable list and body" args

parseWithVars :: [SExp] -> [(Var, Expr)] -> Result [(Var, Expr)]
parseWithVars args varDefs =
  case args of
    []          -> Ok varDefs
    varSExp:ses -> case varSExp of
                     ListS (nameSExp:valueSExp:[]) ->
                       do nameExpr  <- parseExpr nameSExp
                          valueExpr <- parseExpr valueSExp
                          rest      <- parseWithVars ses varDefs
                          case nameExpr of
                            VarE v    -> Ok ((v, valueExpr):rest)
                            otherwise -> syntaxErr "with*" "variable name" nameExpr
                     otherwise -> syntaxErr "with*" "variable declaration" varSExp

parseFunE :: [SExp] -> Result Expr
parseFunE args =
  case args of
    (ListS []):_                 -> syntaxErr "fun" "at least one variable binding" args
    (ListS varSExps):bodySExp:[] ->
      do vars <- parseFunVars varSExps []
         body <- parseExpr bodySExp
         Ok (FunE vars body)
    otherwise -> syntaxErr "fun" "variable list and body" args

parseFunVars :: [SExp] -> [Var] -> Result [Var]
parseFunVars args vars =
  case args of
    []          -> Ok vars
    varSExp:ses ->
      do varExpr <- parseExpr varSExp
         rest    <- parseFunVars ses vars
         case varExpr of
           VarE v    ->
             if v `elem` rest then Err ("Variable `" ++ v ++ "` cannot be bound twice in the same function")
             else Ok (v:rest)
           otherwise -> syntaxErr "fun" "variable binding" varExpr

parseAppE :: [SExp] -> Result Expr
parseAppE args =
  case args of
    x:y:rest  -> parseAppArgs args [] >>= \x -> Ok (AppE x)
    otherwise -> syntaxErr "app" "at least two expressions" args

parseAppArgs :: [SExp] -> [Expr] -> Result [Expr]
parseAppArgs args argExprs =
  case args of
    []          -> Ok argExprs
    argSExp:ses ->
      do argExpr <- parseExpr argSExp
         rest    <- parseAppArgs ses argExprs
         Ok (argExpr:rest)

desugar :: Expr -> Result CExpr
desugar expr =
  case expr of
    NumE n               -> Ok (NumC n)
    VarE v               -> Ok (VarC v)
    IfE testE consE altE ->
      do testC <- desugar testE
         consC <- desugar consE
         altC  <- desugar altE
         Ok (IfC testC consC altC)
    FunE vars body       -> desugarFunE vars body
    AppE (funE:rest)     -> desugar funE >>= (flip desugarAppE) rest
    WithStarE vars body  -> desugar body >>= desugarWithStarE vars
    otherwise            -> Err ("Unrecognized expression: `" ++ (show expr) ++ "`")

desugarFunE :: [Var] -> Expr -> Result CExpr
desugarFunE vars body =
  case vars of
    []     -> desugar body
    v:rest -> desugarFunE rest body >>= \x -> Ok (FunC v x)

desugarAppE :: CExpr -> [Expr] -> Result CExpr
desugarAppE func args =
  case args of
    []     -> Ok func
    v:rest -> desugar v >>= \x -> desugarAppE (AppC func x) rest

desugarWithStarE :: [(Var, Expr)] -> CExpr -> Result CExpr
desugarWithStarE vars bodyC =
  case vars of
    []     -> Ok bodyC
    v:rest ->
      let (name, value) = v in
      do innerC <- desugarWithStarE rest bodyC
         valueC <- desugar value
         Ok (AppC (FunC name innerC) valueC)

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr =
  case expr of
    VarC v            ->
      if v `elem` bound then Ok () else Err ("Variable `" ++ v ++ "` is unbound")
    FunC v body       ->
      if v `elem` reserved then Err("Attempt to rebind `" ++ v ++ "`")
      else checkIds (v:bound) reserved body
    AppC f v          -> checkIds bound reserved f >>= \x -> checkIds bound reserved v
    IfC test cons alt ->
      do _ <- checkIds bound reserved test
         _ <- checkIds bound reserved cons
         checkIds bound reserved alt
    otherwise         -> Ok ()

parseStr :: String -> Result Expr
parseStr input = parseSExp (tokenize input) >>= \(x, _) -> parseExpr x

desugarStr :: String -> Result CExpr
desugarStr input = parseStr input >>= desugar

checkStr :: String -> Result CExpr
checkStr input =
  let bound = ["*", "+", "=", "<", "true", "false"] in
  do expr <- desugarStr input
     _    <- checkIds bound ("if":"fun":bound) expr
     Ok expr

