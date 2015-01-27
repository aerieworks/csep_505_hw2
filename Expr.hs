module Expr (Var, Expr(..), CExpr(..), parseExpr, desugar, checkIds) where

import Data.List
import Result
import SExp

type Var = String

syntaxErr :: (Show a) => String -> String -> a -> Result b
syntaxErr container expected found =
  Err ("Invalid syntax in `" ++ container ++ "`: expected " ++ expected ++ " but found `" ++ (show found) ++ "`")

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
           VarE v    -> Ok (v:rest)
           otherwise -> syntaxErr "fun" "variable binding" varExpr

parseAppE :: [SExp] -> Result Expr
parseAppE args =
  case args of
    x:y:rest  ->
      do argExprs <- parseAppArgs args []
         Ok (AppE argExprs)
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
    AppE (funE:rest)     ->
      do funC <- desugar funE
         desugarAppE funC rest
    WithStarE vars body  ->
      do bodyC <- desugar body
         desugarWithStarE bodyC vars
    otherwise            -> Err ("Unrecognized expression: `" ++ (show expr) ++ "`")

desugarFunE :: [Var] -> Expr -> Result CExpr
desugarFunE vars body =
  case vars of
    []     -> desugar body
    v:rest ->
      do innerC <- desugarFunE rest body
         Ok (FunC v innerC)

desugarAppE :: CExpr -> [Expr] -> Result CExpr
desugarAppE funC args =
  case args of
    []     -> Ok funC
    v:rest ->
      do argC <- desugar v
         desugarAppE (AppC funC argC) rest

desugarWithStarE :: CExpr -> [(Var, Expr)] -> Result CExpr
desugarWithStarE bodyC vars =
  case vars of
    []     -> Ok bodyC
    v:rest ->
      let (name, value) = v in
      do innerC <- desugarWithStarE bodyC rest
         valueC <- desugar value
         Ok (AppC (FunC name innerC) valueC)

checkIds :: [String] -> [String] -> CExpr -> Result ()
checkIds bound reserved expr = Err "checkIds not implemented yet"

parseStr :: String -> Result Expr
parseStr input =
  do (sexp, _) <- parseSExp (tokenize input)
     parseExpr sexp

desugarStr :: String -> Result CExpr
desugarStr input =
  do expr <- parseStr input
     desugar expr

