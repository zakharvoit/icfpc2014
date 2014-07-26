module Opcode where

import Data.Word

data Opcode = LDC Word8
            | ADD | SUB | MUL | DIV
            | CEQ | CGTE | CLTE | CGT | CLT
            | ATOM | CONS | CAR | CDR
            | SEL Word8 Word8
            | SSel String String
            | Call String Word8
            | AP Word8 | LDF Word8
            | Var String
            | LD Word8 Word8
            | ST Word8 Word8
            | RTN
            | NOP
              deriving Show
