module Opcode where

import Data.Word

data Opcode = Ldc Word8
            | Add | Sub | Mul | Div
            | Eq | Ge | Le | Gt | Lt
            | Atom | Cons | Car | Cdr
            | Sel Word8 Word8
            | SSel String String
            | Call String Word8
            | Ap Word8 | Ldf Word8
            | Var String
            | Ld Word8 Word8
            | St Word8 Word8
            | Ret
            | Nop
              deriving Show