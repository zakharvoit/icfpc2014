{-# LANGUAGE FlexibleInstances #-}
module Compiler where

import Opcode
import MonadicParser

import Data.Maybe
import Data.List
import Debug.Trace

data Expr = Name { unname :: String } | List [Expr] | Number Int
          deriving Show

data Func = Func { fname :: String
                 , fargs :: [String]
                 , fops :: [Opcode]
                 } deriving Show

data AFunc = AFunc { afname :: Int
                   , afargs :: [String]
                   , afops :: [Opcode]
                   } deriving Show

data AFuncBin = AFuncBin { afbname :: Int
                         , afbops :: [Opcode]
                         }

lexemToExpr :: Lexem -> Expr
lexemToExpr (LName s) = Name s
lexemToExpr (LList s) = List $ map lexemToExpr s
lexemToExpr (LNumber s) = Number $ fromIntegral s

allFunctionsToAddresses :: [Func] -> ([AFunc], [String])
allFunctionsToAddresses x = let addr = map fname x in
                           (map (\cur@(Func _ a o)
                                 -> AFunc (address cur x) a o) x
                              , addr)

address :: Func -> [Func] -> Int
address (Func name _ _) f = (fromIntegral $ sum $ map (length . fops) $
                           take (fromJust $ name `elemIndex` (map fname f)) f)
                            + 1

allCallsToAddresses :: [AFunc] -> [String] -> [Int] -> [AFunc]
allCallsToAddresses f a addr = map (\(AFunc i a' o) ->
                                AFunc i a'
                                (concatMap (\q -> callToAddress q a addr) o))
                          f

correctFunc :: Func -> Func
correctFunc f = f { fops = fops f ++ [RTN] }

callToAddress :: Opcode -> [String] -> [Int] -> [Opcode]
callToAddress (Call x y) a addr
  = [ LDF $ (addr !!) $ fromJust $ (x `elemIndex` a)
    , AP y]
callToAddress x _ _ = [x]

replaceArgs :: AFunc -> AFuncBin
replaceArgs (AFunc f args ops) = AFuncBin f $ map (\op -> repla op args) ops

repla :: Opcode -> [String] -> Opcode
repla (Var x) args = LD 0 $ fromIntegral $ fromJust $ x `elemIndex` args
repla x _ = x

processIfs :: [Opcode] -> [Opcode]
processIfs x = processIfs' x 1

processIfs' :: [Opcode] -> Int -> [Opcode]
processIfs' [] _ = []
processIfs' (If a b : xs) idx =
  TSEL (fromIntegral idx)
  (fromIntegral idx + a + 2)
  : processIfs' xs (idx + 1)
processIfs' (Jmp a : xs) idx =
  LDC 1 : TSEL (fromIntegral idx + a + 1) 255 : processIfs' xs (idx + 2)
processIfs' (x:xs) idx = x : processIfs' xs (idx + 1)

calcSize :: [Opcode] -> Int
calcSize [] = 0
calcSize (Call _ _ : xs) = 2 + calcSize xs
calcSize (If _ _ : xs) = 1 + calcSize xs
calcSize (Jmp _ : xs) = 2 + calcSize xs
calcSize (_:xs) = 1 + calcSize xs

generate :: Expr -> [Func]
generate (List []) = []
generate (List ((List (Name "defun" : Name f : List args : body : [])) : r)) =
  Func f (map unname args) (generate' body) : generate (List r)

generate' :: Expr -> [Opcode]

generate' (Name x) = [Var x]
generate' (Number x) = [LDC x]
generate' (List [Name "*"]) = error "* to few args"
generate' (List [Name "/"]) = error "/ to few args"
generate' (List [Name "+"]) = error "+ to few args"
generate' (List [Name "-"]) = error "- to few args"

generate' (List [Name "*", _]) = error "* to few args"
generate' (List [Name "/", _]) = error "/ to few args"
generate' (List [Name "+", _]) = error "+ to few args"
generate' (List [Name "-", _]) = error "- to few args"

generate' (List (Name "*" : r)) = concatMap generate' r ++ [MUL]
generate' (List (Name "/" : r)) = (concat $ reverse $ map generate' r) ++ [DIV]
generate' (List (Name "+" : r)) = concatMap generate' r ++ [ADD]
generate' (List (Name "-" : r)) = concatMap generate' r ++ [SUB]

generate' (List (Name "=" : r)) = concatMap generate' r ++ [CEQ]
generate' (List (Name ">=" : r)) = concatMap generate' r ++ [CGTE]
generate' (List (Name "<=" : [a, b])) = generate' b ++ generate' a ++ [CGTE]
generate' (List (Name ">" : r)) = concatMap generate' r ++ [CGT]
generate' (List (Name "<" : [a, b])) = generate' b ++ generate' a ++ [CGT]

generate' (List [Name "atom", x]) = generate' x ++ [ATOM]
generate' (List (Name "cons" : x : xs)) = generate' x
                                        ++ concatMap generate' xs
                                        ++ [CONS]
generate' (List [Name "car", x]) = generate' x ++ [CAR]
generate' (List [Name "cdr", x]) = generate' x ++ [CDR]

generate' (List [Name "if", x, t, e]) =
  generate' x ++
  [If (fromIntegral $ calcSize tb) (fromIntegral $ calcSize eb)] ++
  tb ++ (Jmp (calcSize eb) : eb)
  where
    tb = generate' t
    eb = generate' e

generate' (List (Name f : args)) = concatMap generate' args
                                   ++ [Call f (fromIntegral $ length args)]

generate' x = traceShow x undefined
