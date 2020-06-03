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


TODO
- rewrite this README
- have batch compiled take a flag to control NBE/not --DONE
- run examples with/out NBE - have expect files for both -- DONE

- tag values as number/string/closure, so can print correctly, and also needed when do GC

- stop creating continuations in the heap
- bench nfib! -- DONE
- GC


Wed Jun  3 19:40:45 2020

perf notes...

nfib, in nfibs/us  [us = micro-second]

native -O
0 390
1 540
2 830
3 1100

inter -O (initial)
0 3.9
1 6.5
2 10
3 11.3

inter -O (cleanup, inlining, cut some runtime checks)
0 4.48
1 inlining failed in call to ‘argument’: --param large-function-growth limit reached [-Werror=inline]
2 16.9
3 17.1


TODO: fix -O1
reinstate the checks on a -D flag
-DONE


Add flag (`-nn`) to batch compiler to control opt (i.e.allow NBE to be switched off)
perf... goes from about 18 --> 5.2 -- WOW
