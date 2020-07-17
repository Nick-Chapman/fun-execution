
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "value.h"

//#define TRACE

static char* pap_code[];
static char** overapp_code;

#define noinline __attribute__ ((noinline))
#define must_use __attribute__ ((warn_unused_result))

// entry points. TODO: engine.h
void run_engine_show_info(int argc, char* argv[]);
value run_engine(int argc, char* argv[]);

static char* the_code;

static int my_argc;
static char** my_argv;

noinline static must_use func_p init_machine();
noinline static must_use func_p interpret_byte_code();

const static func_p the_interpreter = (func_p)interpret_byte_code;

inline static void set_code(char* c);

#define stack_size 100
static value stack[stack_size];

static value* the_fp;

value run_engine(int argc, char* argv[]) {
  my_argc = argc;
  my_argv = argv;
  func_p fn = init_machine();
  while (fn) {
    fn = fn();
  }
  return stack[0];
}

void set_code(char* c) {
#ifdef TRACE
  printf("set_code: %s\n",c);
#endif
  the_code = c;
}

inline static value* PUSH_K(int codeRef, int nFree);

inline static must_use func_p RET(value v1);
inline static must_use func_p ENTER(value funValue, unsigned nArgs);
inline static must_use func_p JUMP(int dest);
inline static must_use func_p JUMP_NZ(value cond,int dest);
inline static must_use func_p ARITY_CHECK(int need);

inline static must_use value* MAKE_CLO(int codeRef, int nFree);
inline static void SET_FRAME(value* frame, unsigned i, value v);

//unary
inline static void ShowChar(value v1);
inline static void ShowInt(value v1);
inline static void ReadInt(value v1);
inline static void Argv(value v1);
inline static void StrSize(value v1);
inline static void Error(value v1);

//binary
inline static void Add(value v1, value v2);
inline static void Sub(value v1, value v2);
inline static void Mul(value v1, value v2);
inline static void ModInt(value v1, value v2);
inline static void EqNumOrChar(value v1, value v2);
inline static void LessNumOrChar(value v1, value v2);
inline static void EqString(value v1, value v2);
inline static void StringAppend(value v1, value v2);
inline static void StrIndex(value v1, value v2);

inline static must_use char next();
inline static must_use int digit();
inline static must_use value argument();

inline static void push_stack(value v);
inline static must_use char* get_code_ref(char*,unsigned n);
inline static must_use func_p get_native_ref(char*,unsigned n);

//----------------------------------------------------------------------
// byte-code intepreter(func_p)(void*)

inline static must_use func_p continue_bc(char* code) {
  set_code(code); return(the_interpreter);
}

func_p interpret_byte_code() {

  char instr = 0;
  while ((instr = next())) {
    switch(instr) {

    case 'h': return 0; // halt
    case 'u': {
      int dest = digit();

      func_p fn = get_native_ref("u",dest);
      if (fn) {
        return fn;
      }
      char* code = get_code_ref("u",dest);
      return continue_bc(code);
    }
    case 'j': {
      value cond = argument();
      int dest = digit();
      { func_p fn = JUMP_NZ(cond, dest); if (fn) return fn; }
      break;
    }
    case 'r': {
      value v = argument();
      return RET(v);
    }
    case 't': {
      value funValue = argument();
      unsigned nArgs = digit();
      value the_args[nArgs];
      for (unsigned i = 0; i < nArgs; i++) {
        value v = argument();
        the_args[i] = v;
      }
      for (unsigned i = 0; i < nArgs; i++) {
        stack[i] = the_args[i];
      }
      return ENTER(funValue,nArgs);
    }
    case 'c': {
      int codeRef = digit();
      int nFree = digit();
      value* frame = MAKE_CLO(codeRef,nFree);
      for (int i = 0; i < nFree; i++) {
        value freeVal = argument();
        SET_FRAME(frame,i,freeVal);
      }
      break;
    }
    case 'p': {
      int codeRef = digit();
      int nFree = digit();
      value* frame = PUSH_K(codeRef,nFree);
      for (int i = 0; i < nFree; i++) {
        value freeVal = argument();
        SET_FRAME(frame,i,freeVal);
      }
      break;
    }
    case 'n': {
      int need = digit();
      { func_p fn = ARITY_CHECK(need); if (fn) return fn; }
      break;
    }

#define unary(X,FN) case X: { value a = argument(); FN(a); break; }
#define binary(X,FN) case X: { value a = argument(); value b = argument(); FN(a,b); break; }

        unary('C',ShowChar)
        unary('S',ShowInt)
        unary('R',ReadInt)
        unary('A',Argv)
        unary('B',StrSize)
        unary('!',Error)

        binary('+',Add)
        binary('-',Sub)
        binary('M',Mul)
        binary('%',ModInt)
        binary('=',EqNumOrChar)
        binary('<',LessNumOrChar)
        binary('~',EqString)
        binary('^',StringAppend)
        binary('I',StrIndex)

    default:
      printf("unknown byte: '%c'\n",instr);
      exit(1);
    }
  }
  printf("interpret_byte_code: run out of code!\n");
  exit(1);
}

//----------------------------------------------------------------------
// specific code sequences for the nfib example

func_p U5() {
  return JUMP(5);
}

func_p V() {
  { value* frame = MAKE_CLO(4,1); SET_FRAME(frame,0,stack[0]); }
  Argv(lits[1]);
  ReadInt(stack[1]);
  value func = stack[0];
  value arg0 = stack[2];
  stack[0] = arg0;
  return ENTER(func,1);
}

func_p W() {
  return RET(lits[1]);
}

func_p X() {
  Add(the_fp[0],stack[0]);
  Add(stack[1],lits[1]);
  return RET(stack[2]);
}

func_p X_fos() {
  Add(stack[0],stack[1]);
  Add(stack[2],lits[1]);
  return RET(stack[3]);
}

func_p Y() {
  Sub(the_fp[0],lits[0]);
  { value* frame = PUSH_K(2,1);
    SET_FRAME(frame,0,stack[0]);
  }
  value func = the_fp[1];
  value arg0 = stack[1];
  stack[0] = arg0;
  return ENTER(func,1);
}

func_p Y_fos() {
  Sub(stack[0],lits[0]);
  { value* frame = PUSH_K(2,1);
    SET_FRAME(frame,0,stack[2]);
  }
  value func = stack[1];
  value arg0 = stack[3];
  stack[0] = arg0;
  return ENTER(func,1);
}

func_p Z() {
  { func_p fn = ARITY_CHECK(1); if (fn) return fn; }
  LessNumOrChar(stack[0],lits[0]);
  { func_p fn = JUMP_NZ(stack[1],1); if (fn) return fn; }
  Sub(stack[0],lits[1]);
  { value* frame = PUSH_K(3,2);
    SET_FRAME(frame,0,stack[0]);
    SET_FRAME(frame,1,the_fp[0]);
  }
  value func = the_fp[0];
  value arg0 = stack[2];
  stack[0] = arg0;
  return ENTER(func,1);
}

//----------------------------------------------------------------------
// interpreting the byte-code string

#ifndef NDEBUG
static unsigned steps = 0;
#endif

char next() {
#ifndef NDEBUG
  steps++;
#endif
  char byte = *the_code++;
  //printf("[%d] '%c'\n",steps,byte);
  return byte;
}

int digit() {
  char c;
  switch (c = next()) {
  case '0'...'9':
    return c - '0';
  case 'x':
    return 10 * digit() + digit();
  default:
    printf("digit: '%c' not expected\n", c);
    exit(1);
  }
}

value argument() {
  char c;
  switch (c = next()) {
  /*case '0'...'9': {
    int n = c - '0';
    value res = stack[n];
    return res;
  }
  case 'x': {
    return stack[10 * digit() + digit()];
  }*/
  case '*': {
    int n = digit();
    value res = stack[n];
    return res;
  }
  case '$': {
    int n = digit();
    value res = lits[n];
    return res;
  }
  case '~': {
    int n = digit();
    value res = the_fp[n];
    return res;
  }
  default:
    printf("argument: '%c' not expected\n", c);
    exit(1);
  }
}

//----------------------------------------------------------------------
// machine components

static value* hp;
static value* sp;

static value* kont;
static value* this_closure;

//----------------------------------------------------------------------
// byte code effects

inline static must_use value* heap_alloc(int n);

static must_use value* (*push_continuation)(char* code,int nFree);
static must_use value* (*push_continuation_native)(func_p,int nFree);
static must_use func_p (*return_to_continuation)(value v);

noinline static must_use char* string_of_char(char arg);
noinline static must_use char* string_of_int(long arg);
noinline static must_use char* string_concat(char* s1, char* s2);

//unary

void ShowChar(value v1) {
  char c = (char)(long)v1;
  char* res = string_of_char(c);
  push_stack((value)res);
}

void ShowInt(value v1) {
  long a = (long)v1;
  char* res = string_of_int(a);
  push_stack((value)res);
}

void ReadInt(value v1) {
  char* s = (char*)v1;
  long n = 0;
  sscanf(s,"%ld",&n);
  push_stack((value)n);
}

void Argv(value v1) {
  long n = (long)v1;
  char* res = n<my_argc ? my_argv[n] : "";
  push_stack((value)res);
}

void StrSize(value v1) {
  char* a = (char*)v1;
  long res = strlen(a);
  push_stack((value)res);
}

void Error(value v1) {
  char* a = (char*)v1;
  printf("error: %s\n",a);
  exit(1);
}

//binary

void Add(value v1, value v2) {
  long a = (long)v1;
  long b = (long)v2;
  long res = a + b;
  push_stack((value)res);
}

void Sub(value v1, value v2) {
  long a = (long)v1;
  long b = (long)v2;
  long res = a - b;
  push_stack((value)res);
}

void Mul(value v1, value v2) {
  long a = (long)v1;
  long b = (long)v2;
  long res = a * b;
  push_stack((value)res);
}

void ModInt(value v1, value v2) {
  long a = (long)v1;
  long b = (long)v2;
  long res = a % b;
  push_stack((value)res);
}

void EqNumOrChar(value v1, value v2) {
  long a = (long)v1;
  long b = (long)v2;
  long res = a == b;
  push_stack((value)res);
}

void LessNumOrChar(value v1, value v2) {
  long a = (long)v1;
  long b = (long)v2;
  long res = a < b;
  push_stack((value)res);
}

void EqString(value v1, value v2) {
  char* s1 = (char*)v1;
  char* s2 = (char*)v2;
  long res = strcmp(s1,s2);
  push_stack((value)(res?0L:1L));
}

void StringAppend(value v1, value v2) {
  char* s1 = (char*)v1;
  char* s2 = (char*)v2;
  char* res = string_concat(s1,s2);
  push_stack((value)res);
}

void StrIndex(value v1, value v2) {
  char* s1 = (char*)v1;
  long n = (long)v2;
  char c = s1[n];
  push_stack((value)(long)c);
}

func_p RET(value v1) {
  return return_to_continuation(v1);
}

value* PUSH_K(int codeRef, int nFree) {
  func_p fn = get_native_ref("PUSH_K",codeRef);
  if (fn) {
    return push_continuation_native(fn,nFree);
  }
  char* futurecode = get_code_ref("PUSH_K",codeRef);
  return push_continuation(futurecode,nFree);
}

noinline static must_use value* make_pap(int got, int need);
noinline static void push_overApp(int got, int need);
noinline static must_use char* get_pap_got_need(unsigned got, unsigned need);
noinline static must_use char* get_overapp_extra(unsigned extra);

typedef func_p (*enter_p)();

static must_use func_p enter_byte_code() {
  char* code = the_fp[0];
  the_fp++;
  return continue_bc(code);
}

func_p ENTER(value funValue, unsigned nArgs) {
  value* clo = (value*)funValue;
  this_closure = clo;
  enter_p enterer = clo[0];
  sp = stack + nArgs;
  the_fp = &clo[1];
  return enterer();
}

value* MAKE_CLO(int codeRef, int nFree) {

  func_p fn = get_native_ref("MAKE_CLO",codeRef);
  if (fn) {
    value* clo = heap_alloc(1 + nFree);
    push_stack(clo);
    clo[0] = fn;
    return &clo[1];
  }

  value* clo = heap_alloc(2 + nFree);
  push_stack(clo);
  clo[0] = enter_byte_code;
  clo[1] = get_code_ref("MAKE_CLO",codeRef);
  return &clo[2];
}

void SET_FRAME(value* frame, unsigned i, value v) {
  frame[i] = v;
}

value* make_pap(int got, int need) {
  value* clo = heap_alloc(3 + got);
  clo[0] = enter_byte_code;
  clo[1] = get_pap_got_need(got,need);
  clo[2] = this_closure;
  for (int i = 0; i < got; i++) {
    clo[3 + i] = stack[i];
  }
  return clo;
}

func_p JUMP(int dest) {
  func_p fn = get_native_ref("u",dest);
  if (fn) {
    return fn;
  }
  char* code = get_code_ref("u",dest);
  return continue_bc(code);
}

func_p JUMP_NZ(value cond,int dest) {
  if (cond) {
    func_p fn = get_native_ref("JUMP_NZ",dest);
    if (fn) return fn;
    char* code = get_code_ref("JUMP_NZ",dest);
    return continue_bc(code);
  }
  return 0;
}

func_p ARITY_CHECK(int need) {
  int got = (sp - stack);
  if (got < need) {
    value* pap = make_pap(got,need);
    return return_to_continuation(pap);
  }
  if (got > need) {
    push_overApp(got,need);
  }
  return 0;
}


void push_overApp(int got, int need) {
  unsigned extra = got - need;
  char* futureCode = get_overapp_extra(extra);
  value* frame = push_continuation(futureCode,extra);
  for (int i = 0; i < extra; i++) {
    value v = stack[i+need];
    SET_FRAME(frame,i,v);
  }
  sp -= extra; //adjust for the number of args stashed in the oveapp kont
}

//----------------------------------------------------------------------

#ifndef NDEBUG
static unsigned max_code_ref = 0;
static void init_max_code_ref() {
  unsigned i = 0;
  for (; prog[i]; i++);
  max_code_ref = i;
}
static void check_code_ref(unsigned n) {
  if (n >= max_code_ref) {
    printf("check_code_ref: n too big: %d>=%d\n", n, max_code_ref);
    exit(1);
  }
}
static unsigned max_pap_ref = 0;
static void init_max_pap_ref() {
  unsigned i = 0;
  for (; pap_code[i]; i++);
  max_pap_ref = i;
}
void check_pap_ref(unsigned got, unsigned need, unsigned n) {
  if (got<1) {
    printf("check_pap_ref (got<1): got=%d\n", got); exit(1);
  }
  if (need<2) {
    printf("check_pap_ref (need<2): need=%d\n", need); exit(1);
  }
  if (got >= need) {
    printf("check_pap_ref (got>=need): %d>=%d\n", got,need); exit(1);
  }
  if (n >= max_pap_ref) {
    printf("check_pap_ref (got=%d, need=%d): n too big: %d>=%d\n", got, need, n, max_pap_ref);
    exit(1);
  }
}
static unsigned max_overapp_ref = 0;
static void init_max_overapp_ref() {
  unsigned i = 0;
  for (; overapp_code[i]; i++);
  max_overapp_ref = i;
}
void check_overapp_ref(unsigned n) {
  if (n >= max_overapp_ref) {
    printf("check_overapp_ref: n too big: %d>=%d\n", n, max_overapp_ref);
    exit(1);
  }
}
#endif

// FOS = "free variables on stack"
// FIF = "free variables in frame"

value* push_continuation_native_FOS(func_p fn,int nFree) {
  value* k = heap_alloc(3 + nFree);
  k[0] = kont;
  k[1] = (value)(long)nFree;
  k[2 + nFree] = fn;
  kont = k;
  return &k[2];
}

static value final_continuation_FOS[] =
  { 0, //kont
    0, //nFree
    enter_byte_code,
    (value)"h"
  };

value* push_continuation_FOS(char* code,int nFree) {
  value* k = heap_alloc(3 + nFree + 1);
  k[0] = kont;
  k[1] = (value)(long)nFree;
  k[2 + nFree] = enter_byte_code;
  k[3 + nFree] = code;
  kont = k;
  return &k[2];
}

func_p return_to_continuation_FOS(value v) {
  int nFree = (long)kont[1];
  stack[nFree] = v;
  sp = &stack[nFree+1];
  for (int i = 0; i < nFree; i++) {
    value v = kont[2 + i];
    stack[i] = v;
  }
  enter_p enterer = kont[2 + nFree];
  the_fp = &kont[3+nFree];
  kont = kont[0];
  return enterer();
}


value* push_continuation_native_FIF(func_p fn,int nFree) {
  value* k = heap_alloc(2 + nFree);
  k[0] = kont;
  k[1] = fn;
  kont = k;
  return &k[2];
}


static value final_continuation_FIF[] =
  {
   0, //kont
   enter_byte_code,
   (value)"h"
  };

value* push_continuation_FIF(char* code,int nFree) {
  value* k = heap_alloc(3 + nFree);
  k[0] = kont;
  k[1] = enter_byte_code;
  k[2] = code;
  kont = k;
  return &k[3];
}

func_p return_to_continuation_FIF(value v) {
  the_fp = &kont[2];
  sp = stack;
  push_stack(v);
  enter_p enterer = kont[1];
  kont = kont[0];
  return enterer();
}

static char* overapp_code_FOS[] =
  {
   "t*11*0",
   "t*22*0*1",
   "t*33*0*1*2",
   "t*44*0*1*2*3",
   "t*55*0*1*2*3*4",
   0,
  };

static char* overapp_code_FIF[] =
  {
   "t*01~0",
   "t*02~0~1",
   "t*03~0~1~2",
   "t*04~0~1~2~3",
   "t*05~0~1~2~3~4",
   0,
  };


#define meg (1024 * 1024)
#define heap_size (200 * meg)

static value heap[heap_size];

func_p init_machine() {

  overapp_code =
    config_fvs_on_stack
    ? overapp_code_FOS
    : overapp_code_FIF;

#ifndef NDEBUG
  init_max_code_ref();
  init_max_pap_ref();
  init_max_overapp_ref();
#endif

  kont =
    config_fvs_on_stack
    ? final_continuation_FOS
    : final_continuation_FIF;

  push_continuation =
    config_fvs_on_stack
    ? &push_continuation_FOS
    : &push_continuation_FIF;

  push_continuation_native =
    config_fvs_on_stack
    ? &push_continuation_native_FOS
    : &push_continuation_native_FIF;

  return_to_continuation =
    config_fvs_on_stack
    ? &return_to_continuation_FOS
    : &return_to_continuation_FIF;

  hp = &heap[0];
  sp = stack;

  func_p fn = get_native_ref("init",0);
  if (fn) return fn;
  char* code = get_code_ref("init",0); // can fail if there is no code
  return continue_bc(code);
}

static value* heap_end = &heap[heap_size];

static void heap_exhausted() {
  printf("heap exhausted\n"); exit(1);
}

value* heap_alloc(int n) {
  value* res = hp;
  hp += n;
  if (hp > heap_end) { heap_exhausted(); }
  return res;
}

void push_stack(value v) {
  *sp++ = v;
}

char* get_code_ref(char* who, unsigned n) {
  //printf("get_code_ref(%s,%d)\n",who,n);
#ifndef NDEBUG
  check_code_ref(n);
#endif
  return prog[n];
}

func_p get_native_ref(char* who, unsigned n) {
  //printf("get_native_ref(%s,%d)\n",who,n);
#ifndef NDEBUG
  check_code_ref(n);
#endif
  //func_p res = native[n];
  //printf("get_native_ref(%s,%d) -> %p\n",who,n,res);
  return native[n];
}

char* get_pap_got_need(unsigned got, unsigned need) {
  unsigned n = ((need-1) * (need-2)) / 2 + got - 1;
#ifndef NDEBUG
  check_pap_ref(got,need,n);
#endif
  return pap_code[n];
}

char* get_overapp_extra(unsigned extra) {
#ifndef NDEBUG
  check_overapp_ref(extra-1);
#endif
  return overapp_code[extra-1];
}

char* string_of_char(char arg) {
  unsigned len = 2;
  unsigned n = 1 + ((len-1) / bytes_per_value);
  char* res = (char*)heap_alloc(n);
  res[0] = arg;
  res[1] = 0;
  return res;
}

char* string_of_int(long arg) {
  //TODO: compute size needed from size of arg.. just log10 it!!
  static char buf[50];
  sprintf(buf,"%ld",arg);
  unsigned len = strlen(buf) + 1; // 0 terminated
  unsigned n = 1 + ((len-1) / bytes_per_value);
  char* res = (char*)heap_alloc(n);
  strcpy(res,buf);
  return res;
}

char* string_concat(char* s1, char* s2) {
  unsigned len = strlen(s1) + strlen(s2) + 1; //0-terminated
  unsigned n = 1 + ((len-1) / bytes_per_value);
  char* res = (char*)heap_alloc(n);
  sprintf(res,"%s%s",s1,s2);
  return res;
}

static char* pap_code[] =
  {
   "n1t~02~1*0", //pap1/2
   "n2t~03~1*0*1", //pap1/3
   "n1t~03~1~2*0", //pap2/3
   "n3t~04~1*0*1*2", //pap1/4
   "n2t~04~1~2*0*1", //pap2/4
   "n1t~04~1~2~3*0", //pap3/4
   "n4t~05~1*0*1*2*3", //pap1/5
   "n3t~05~1~2*0*1*2", //pap2/5
   "n2t~05~1~2~3*0*1", //pap3/5
   "n1t~05~1~2~3~4*0", //pap4/5
   "n5t~06~1*0*1*2*3*4", //pap1/6
   "?",
   "?",
   "?",
   "?",
   "?",
   "n5t~07~1~2*0*1*2*3*4", //pap2/7
   "?",
   "?",
   "?",
   "?",
   "n7t~08~1*0*1*2*3*4*5*6", //pap1/8
   0,
  };


// Short term hack to allow adding pythagorian to regression.  The
// real solution is to tag values to distinguish unboxed numbers from
// heap objects. And for heap objects to have a descriptor word. All
// this is needed anyway to implement GC.

static must_use bool_t temp_looks_like_string(value v) {
  return v > (value)(1L<<40);
}

void run_engine_show_info(int argc, char* argv[]) {
  value result = run_engine(argc,argv);
  if (temp_looks_like_string(result)) {
    printf("the final result is: '%s'\n", (char*)result);
  } else {
    printf("the final result is: %ld\n", (long)result);
  }
  printf("heap used, %ld cells\n", (hp-heap));
#ifndef NDEBUG
  printf("#steps = %d\n", steps);
#endif
}
