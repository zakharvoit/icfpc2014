module Opcode where

import Data.Word

data Opcode = LDC Word8
            | ADD | SUB | MUL | DIV
            | CEQ | CGTE | CGT
            | ATOM | CONS | CAR | CDR
            | TSEL Word8 Word8
            | If Word8 Word8
            | Jmp Word8
            | Call String Word8
            | AP Word8 | LDF Word8
            | Var String
            | LD Word8 Word8
            | ST Word8 Word8
            | RTN
            | NOP
              deriving Show
