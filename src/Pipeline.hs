
module Pipeline (CompilationError,Code,Value,Instrumentation,check,compile,execute,Opt(..)) where

-- Choose a pipeline...

--import Pipeline1
--import Pipeline2 -- Anf.LetFix{} -> undefined .. needs Opt(..)
import Pipeline3
