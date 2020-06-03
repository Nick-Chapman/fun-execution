
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "value.h"

typedef int bool_t;

#define BaseContinuationSize 3

#define heap_size 100000000
#define temps_size 100

static char* pap_code[];
static char* overapp_code[];

static value final_continuation[] = { (value)0, (value)"", (value)0 };

static value heap[heap_size];
static value temps1[temps_size];
static value temps2[temps_size];

#define noinline __attribute__ ((noinline))

inline static char* get_code_ref(unsigned n);
inline static value* heap_alloc(int n);
inline static char next();
inline static int digit();
inline static value argument();
inline static void push_stack(value v);
inline static void push_arg(value v);
inline static void push_continuation(char* code,int nFree);
inline static void return_to_continuation(value v);
inline static value* make_closure(int codeRef, int nFree);
inline static void enter_closure(value* clo);
inline static void function_arity_check(int need);

noinline static void init_machine();
noinline static value* make_pap(int got, int need);
noinline static void push_overApp(int got, int need);
noinline static char* string_of_int(long arg);
noinline static char* string_concat(char* s1, char* s2);
noinline static char* get_pap_got_need(unsigned got, unsigned need);
noinline static char* get_overapp_extra(unsigned extra);

static value* stack;
static value* args;

static value* hp;
static value* sp;
static value* ap;

static char* code;
static value* kont;
static value* frame;
static value* this_closure;

value run_engine(int argc, char* argv[]) {
  init_machine();
  char instr = 0;
  while ((instr = next())) {
    switch(instr) {
    case 'u': {
      int dest = digit();
      code = get_code_ref(dest);
      break;
    }
    case 'j': {
      value cond = argument();
      int dest = digit();
      if (cond) {
        code = get_code_ref(dest);
      }
      break;
    }
    case 'r': {
      value v = argument();
      return_to_continuation(v);
      break;
    }
    case 't': {
      value funValue = argument();
      int nArgs = digit();
      for (int i = 1; i <= nArgs; i++) {
        value v = argument();
        push_arg(v);
      }
      value* clo = (value*)funValue;
      enter_closure(clo);
      break;
    }
    case 'c': {
      int codeRef = digit();
      int nFree = digit();
      value* clo = make_closure(codeRef,nFree);
      for (int i = 0; i < nFree; i++) {
        value freeVal = argument();
        clo[i+1] = freeVal;
      }
      break;
    }
    case 'p': {
      int codeRef = digit();
      int nFree = digit();
      char* futurecode = get_code_ref(codeRef);
      push_continuation(futurecode,nFree);
      for (int i = 0; i < nFree; i++) {
        value freeVal = argument();
        kont[i + BaseContinuationSize] = freeVal;
      }
      break;
    }
    case 'n': {
      int need = digit();
      function_arity_check(need);
      break;
    }
    case '=': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a == b;
      push_stack((value)res);
      break;
    }
    case '<': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a < b;
      push_stack((value)res);
      break;
    }
    case '-': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a - b;
      push_stack((value)res);
      break;
    }
    case '+': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a + b;
      push_stack((value)res);
      break;
    }
    case '*': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a * b;
      push_stack((value)res);
      break;
    }
    case '%': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a % b;
      push_stack((value)res);
      break;
    }
    case 'S': {
      long a = (long)argument();
      char* res = string_of_int(a);
      push_stack((value)res);
      break;
    }
    case 'R': {
      char* s = (char*)argument();
      long n = 0;
      sscanf(s,"%ld",&n);
      push_stack((value)n);
      break;
    }
    case 'A': {
      long n = (long)argument();
      char* res = n<argc ? argv[n] : "";
      push_stack((value)res);
      break;
    }
    case '^': {
      char* s1 = (char*)argument();
      char* s2 = (char*)argument();
      char* res = string_concat(s1,s2);
      push_stack((value)res);
      break;
      }
    default:
      printf("unknown byte: '%c'\n",instr);
      exit(1);
    }
  }
  return stack[0];
}

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

void init_machine() {
#ifndef NDEBUG
  init_max_code_ref();
  init_max_pap_ref();
  init_max_overapp_ref();
#endif
  kont = &final_continuation[0];
  code = get_code_ref(0); // can fail if there is no code
  stack = &temps1[0];
  args = &temps2[0];
  hp = &heap[0];
  sp = stack;
  ap = args;
}


void push_continuation(char* code,int nFree) {
  value* k = heap_alloc(nFree + BaseContinuationSize);
  k[0] = (value)(long)nFree;
  k[1] = code;
  k[2] = kont;
  kont = k;
}

void return_to_continuation(value v) {
  int nFree = (long)kont[0];
  code = kont[1];
  frame = 0;
  sp = stack;
  for (int i = 0; i < nFree; i++) {
    value v = kont[i + BaseContinuationSize];
    push_stack(v);
  }
  push_stack(v);
  kont = kont[2];
}

value* make_closure(int codeRef, int nFree) {
  value* clo = heap_alloc(nFree+1);
  push_stack(clo);
  clo[0] = get_code_ref(codeRef);
  return clo;
}

void enter_closure(value* clo) {
  this_closure = clo;
  code = clo[0];
  frame = &clo[1];
  value* tmp;
  tmp = stack; stack = args; args = tmp;
  sp = ap; ap = args;
}

void function_arity_check(int need) {
  int got = (sp - stack);
  if (got < need) {
    value* pap = make_pap(got,need);
    return_to_continuation(pap);
  }
  if (got > need) {
    push_overApp(got,need);
  }
}

value* make_pap(int got, int need) {
  value* pap = heap_alloc(got+2);
  pap[0] = get_pap_got_need(got,need);
  pap[1] = this_closure;
  for (int i = 0; i < got; i++) {
    pap[i+2] = stack[i];
  }
  return pap;
}

void push_overApp(int got, int need) {
  unsigned extra = got - need;
  char* futureCode = get_overapp_extra(extra);
  push_continuation(futureCode,extra);
  for (int i = 0; i < extra; i++) {
    value v = stack[i+need];
    kont[i + BaseContinuationSize] = v;
  }
  sp -= extra; //adjust for the number of args stashed in the oveapp kont
}

#ifndef NDEBUG
static value* heap_end = &heap[heap_size];
#endif

value* heap_alloc(int n) {
  value* res = hp;
  hp += n;
#ifndef NDEBUG
  if (hp > heap_end) {
    printf("heap exhausted\n"); exit(1);
  }
#endif
  return res;
}

void push_stack(value v) {
  *sp++ = v;
}

void push_arg(value v) {
  *ap++ = v;
}

char* get_code_ref(unsigned n) {
#ifndef NDEBUG
  check_code_ref(n);
#endif
  return prog[n];
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

#ifndef NDEBUG
static unsigned steps = 0;
#endif

char next() {
#ifndef NDEBUG
  steps++;
#endif
  char byte = *code++;
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
  case '0'...'9': {
    int n = c - '0';
    value res = stack[n];
    return res;
  }
  case 'x': {
    return stack[10 * digit() + digit()];
  }
  case '$': {
    int n = digit();
    value res = lits[n];
    return res;
  }
  case '~': {
    int n = digit();
    value res = frame[n];
    return res;
  }
  default:
    printf("argument: '%c' not expected\n", c);
    exit(1);
  }
}

static char* pap_code[] =
  {
   "t~02~10", //pap1/2
   "t~03~101", //pap1/3
   "t~03~1~20", //pap2/3
   "t~04~1012", //pap1/4
   "t~04~1~201", //pap2/4
   "t~04~1~2~30", //pap3/4
   "t~05~10123", //pap1/5
   "t~05~1~2012", //pap2/5
   "t~05~1~2~301", //pap3/5
   "t~05~1~2~3~40", //pap4/5
   0,
  };

static char* overapp_code[] =
  {
   "t110",
   "t2201",
   "t33012",
   "t440123",
   0,
  };


// Short term hack to allow adding pythagorian to regression.  The real solution is to tag
// values to distinguish unboxed numbers from heap objects. And for heap objects to have a
// descriptor word. All this is needed anyway to implement GC.

static bool_t temp_looks_like_string(value v) {
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
