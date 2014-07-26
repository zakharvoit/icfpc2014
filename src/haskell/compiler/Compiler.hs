{-# LANGUAGE FlexibleInstances #-}
module Compiler where

import Opcode
import MonadicParser

import Data.Word
import Data.Maybe
import Data.List
import Debug.Trace

data Expr = Name { unname :: String } | List [Expr] | Number Word8
          deriving Show

data Func = Func { fname :: String
                 , fargs :: [String]
                 , fops :: [Opcode]
                 } deriving Show

data AFunc = AFunc { afname :: Word8
                   , afargs :: [String]
                   , afops :: [Opcode]
                   } deriving Show

data AFuncBin = AFuncBin { afbname :: Word8
                         , afbops :: [Opcode]
                         }

instance Show AFuncBin where
  show x = ";;; function on address " ++ show (afbname x) ++ ": \n"
           ++  ((concatMap $ (++ "\n") . show) . afbops) x

lexemToExpr :: Lexem -> Expr
lexemToExpr (LName s) = Name s
lexemToExpr (LList s) = List $ map lexemToExpr s
lexemToExpr (LNumber s) = Number $ fromIntegral s

allFunctionsToAddresses :: [Func] -> ([AFunc], [String])
allFunctionsToAddresses x = let addr = map fname x in
                           (map (\cur@(Func _ a o)
                                 -> AFunc (address cur x) a o) x
                              , addr)

address :: Func -> [Func] -> Word8
address (Func name _ _) f = fromIntegral $ sum $ map (length . fops) $
                           take (fromJust $ name `elemIndex` (map fname f)) f



allCallsToAddresses :: [AFunc] -> [String] -> [Word8] -> [AFunc]
allCallsToAddresses f a addr = map (\(AFunc i a' o) ->
                                AFunc i a'
                                (concatMap (\q -> callToAddress q a addr) o))
                          f

correctFunc :: Func -> Func
correctFunc f = f { fops = genSt (length $ fargs f) ++ fops f ++ [Rtn] }
  where
    genSt :: Int -> [Opcode]
    genSt 0 = []
    genSt n = St 0 (fromIntegral $ n - 1) : genSt (n - 1)

callToAddress :: Opcode -> [String] -> [Word8] -> [Opcode]
callToAddress (Call x y) a addr
  = [ Ldf $ (addr !!) $ fromJust $ x `elemIndex` a
    , Ap y]
callToAddress x _ _ = [x]

replaceArgs :: AFunc -> AFuncBin
replaceArgs (AFunc f args ops) = AFuncBin f $ map (\op -> repla op args) ops

repla :: Opcode -> [String] -> Opcode
repla (Var x) args = Ld 0 $ fromIntegral $ fromJust $ x `elemIndex` args
repla x _ = x

generate :: Expr -> [Func]
generate (List []) = []
generate (List ((List (Name "defun" : Name f : List args : body : [])) : r)) =
  Func f (map unname args) (generate' body) : generate (List r)

generate' :: Expr -> [Opcode]

generate' (Name x) = [Var x]
generate' (Number x) = [Ldc x]
generate' (List [Name "*"]) = error "* to few args"
generate' (List [Name "/"]) = error "/ to few args"
generate' (List [Name "+"]) = error "+ to few args"
generate' (List [Name "-"]) = error "- to few args"

generate' (List [Name "*", _]) = error "* to few args"
generate' (List [Name "/", _]) = error "/ to few args"
generate' (List [Name "+", _]) = error "+ to few args"
generate' (List [Name "-", _]) = error "- to few args"

generate' (List (Name "*" : r)) = concatMap generate' r ++ [Mul]
generate' (List (Name "/" : r)) = (concat $ reverse $ map generate' r) ++ [Div]
generate' (List (Name "+" : r)) = concatMap generate' r ++ [Add]
generate' (List (Name "-" : r)) = concatMap generate' r ++ [Sub]

generate' (List (Name "=" : r)) = concatMap generate' r ++ [CEq]
generate' (List (Name ">=" : r)) = concatMap generate' r ++ [CGte]
generate' (List (Name "<=" : r)) = concatMap generate' r ++ [CLte]
generate' (List (Name ">" : r)) = concatMap generate' r ++ [CLt]
generate' (List (Name "<" : r)) = concatMap generate' r ++ [CGt]

generate' (List [Name "atom", x]) = generate' x ++ [Atom]
generate' (List (Name "cons" : x : xs)) = generate' x
                                        ++ concatMap generate' xs
                                        ++ [Cons]
generate' (List [Name "car", x]) = generate' x ++ [Car]
generate' (List [Name "cdr", x]) = generate' x ++ [Cdr]

generate' (List (Name f : args)) = concatMap generate' args
                                   ++ [Call f (fromIntegral $ length args)]

generate' x = traceShow x undefined
