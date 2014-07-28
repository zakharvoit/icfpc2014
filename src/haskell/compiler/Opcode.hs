module Opcode where

data Opcode = LDC Int
            | ADD | SUB | MUL | DIV
            | CEQ | CGTE | CGT
            | ATOM | CONS | CAR | CDR
            | TSEL Int Int
            | If Int Int
            | Jmp Int
            | Call String Int
            | Function String
            | AP Int | LDF Int
            | Var String
            | LD Int Int
            | ST Int Int
            | RTN
            | DBG
            | NOP

instance Show Opcode where
  show (LDC a) = "LDC " ++ show a
  show ADD = "ADD"
  show SUB = "SUB"
  show MUL = "MUL"
  show DIV = "DIV"
  show CEQ = "CEQ"
  show CGTE = "CGTE"
  show CGT = "CGT"
  show ATOM = "ATOM"
  show CONS = "CONS"
  show CAR = "CAR"
  show CDR = "CDR"
  show (TSEL a b) = "TSEL " ++ show a ++ " " ++ show b
  show (AP a) = "AP " ++ show a
  show (LD a b) = "LD " ++ show a ++ " " ++ show b
  show (LDF a) = "LDF " ++ show a
  show (ST a b) = "ST " ++ show a ++ " " ++ show b
  show RTN = "RTN"
  show DBG = "DBUG"
