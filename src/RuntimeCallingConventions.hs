
module RuntimeCallingConventions(RT(..),ContFreeVars(..),fvsOnStack) where

{-
Where should the code for a continuation locate the values of it's free-vars?

The _normal_ strategy is to locate them via the frame-pointer - i.e in the same
way that a closure locates the values of it's free-vars.

However, if the engine wants to avoid allocating continuations in the heap, and
instead use a stack, then, when entering a continuation, the free-vars must be
first copied int the temps stack, because the new (cont)stack will get over
written.  The cost of this choice is: (1) the time spent to copy the values, and
(2) the continuation must record explicitly how many free-values it holds.
-}

data RT = RT
  { contFreeVars :: ContFreeVars
  }

data ContFreeVars
  = FOS -- Free-vars On Stack
  | FIF -- Free-vars In Frame
  deriving Show

fvsOnStack :: RT -> Bool
fvsOnStack = \case
  RT{contFreeVars=FOS} -> True
  RT{contFreeVars=FIF} -> False
