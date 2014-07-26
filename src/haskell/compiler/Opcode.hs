module Opcode where

data Opcode = LDC Int
            | ADD | SUB | MUL | DIV
            | CEQ | CGTE | CGT
            | ATOM | CONS | CAR | CDR
            | TSEL Int Int
            | If Int Int
            | Jmp Int
            | Call String Int
            | AP Int | LDF Int
            | Var String
            | LD Int Int
            | ST Int Int
            | RTN
            | NOP
              deriving Show
