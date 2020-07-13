
module Config (fvsOnStack) where

-- Flag must be consistent between compiler & interpreters:
--      Eval_ClosureConverted, Eval_Linear & bc/engine.c
fvsOnStack :: Bool
fvsOnStack = False
