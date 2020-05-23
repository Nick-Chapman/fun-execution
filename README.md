# fun-execution

Put together a full language execution pipeline.  Taking ideas from fun-console & NbE.
Memories of how I did this way back with nml.  And plans for a an actual byte code
interpreter in C/rust.

stages:
1 console
2 parse -> Exp
3 NbE (Exp -> Exp)
4 Exp -> Anf
5 Anf -> CC (Closure converted)
6 CC -> ByteCode
7 Bytecode -> export to external file
8 C/rust engine for execution

Evaluation is possible at any stage: Exp, Anf, CC, ByteCode.  But the goal is to get all
the way to actual byte-code which is exported for execution by a C/rust engine.

fun-console contains:
- console & parsing stuff (1,2)
- evaluation for it's Exp & some work on NbE (3)
(although the evaluation semantics there are lazy)

NbE contains:
- Nbe (3)
- Exp,Anf,CC
- compilers between stages (4,5)
- simplistic evaluator for Exp
- Cek based evaluators for Exp, Anf and CC
- an attempt at ByteCode (but coming from Anf, not CC) (6ish)

Main things not done:
- ByteCode + compiler from CC (6)
- External format for bytecode (7)
- Byte code execution in C/rust (8)

Plan:

A - The first step will be to collect and combine what I already have to get the
compilation pipeline stages (1,2,3,4,5) upto CC working. With Cek-based evaluation of CC
(within the Haskell console).

B - Then design bytecode, add compilation step from CC to ByteCode. With Cek based
evaluation of the ByteCode (within the Haskell console).

C - Export the bytecode & code execution engine in rust/C.
