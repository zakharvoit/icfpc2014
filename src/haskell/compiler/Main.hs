module Main where

import Compiler
import LispParser

main :: IO ()
main = do
  s <- getContents
  let p = map lexemToExpr $ lexer s
  let e = map correctFunc $ generate $ List p
  let addr = map (\f -> address f e) e
  let (e', i) = allFunctionsToAddresses e
  let e'' = map replaceArgs $ allCallsToAddresses e' i addr
  let ops = processIfs $ concatMap afbops e''
  mapM_ print ops
