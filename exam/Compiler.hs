module Compiler where

import Datatype
import Monstupar
 
import Data.Word
import LLVM.Core
import LLVM.Util.Arithmetic
import LLVM.Util.Loop
import LLVM.ExecutionEngine


--printEXPR :: EXPR -> String 
printEXPR (IVAL s) = valueOf s
--printEXPR (PLUS l r) = mul i32 


