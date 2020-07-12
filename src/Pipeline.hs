
module Pipeline (CompilationError,Code,Value,Instrumentation,check,quietCompile,compile,execute,Opt(..)) where

--import Pipeline1
--import Pipeline2 -- Anf.LetFix{} -> undefined .. needs Opt(..)
--import Pipeline3 -- -> Ast -> Anf -> CC
import Pipeline4 -- -> Ast -> Anf -> CC -> Linear
