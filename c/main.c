
// first C program I have written for a file...
// byte code interpreter!

#include <stdio.h>
#include <stdlib.h>

typedef void* value;

//#include "nfib.h"
#include "nthPrime.h"


static unsigned max_code_ref = 0;
void init_max_code_ref() {
  //printf("bytecode program=\n");
  unsigned i = 0;
  for (; prog[i]; i++) {
    //printf("%i: %s\n", i, prog[i]);
  }
  max_code_ref = i;
  //printf("max_code_ref = %d\n",max_code_ref); //legal refs indexed from 0..max-1
}
static char* get_code_ref(unsigned n) {
  //printf("get_code_ref: %d (max=%d)\n", n,max_code_ref);
  if (n >= max_code_ref) {
    printf("code_ref too big: %d>=%d\n", n, max_code_ref); exit(1);
  }
  return prog[n];
}


static unsigned max_pap_ref = 0;
static char* pap_code[] =
  {
   "?-pap0/1",
   "?-pap0/2",
   "t~02~10", //pap1/2
   //"?-pap0/3",
   //"?-pap1/3",
   //"?-pap2/3",
   0,
  };
void init_max_pap() {
  //printf("pap codes=\n");
  unsigned i = 0;
  for (; pap_code[i]; i++) {
    //printf("%i: %s\n", i, pap_code[i]);
  }
  max_pap_ref = i;
  //printf("max_pap_ref = %d\n",max_pap_ref);
}
static char* get_pap_ref(unsigned n) {
  //printf("get_pap_ref: %d (max=%d)\n", n,max_pap_ref);
  if (n >= max_pap_ref) {
    printf("pap_ref too big: %d>=%d\n", n, max_pap_ref); exit(1);
  }
  return pap_code[n];
}
static char* get_pap_got_need(unsigned got, unsigned need) {
  if (got >= need) {
    printf("get_pap_got_need (got>=need): %d>=%d\n", got,need); exit(1);
  }
  //need is at least 1
  unsigned n = (need * (need - 1)) / 2 + got;
  //printf("get_pap_got_need: %d/%d -> %d\n", got, need, n);
  return get_pap_ref(n);
}


#define heap_size 1000000
static value heap[heap_size];
static value* hp = &heap[0];
static value* heap_end = &heap[heap_size];

value* grab(int n) {
  //printf("grab %d\n",n);
  value* res = hp;
  hp += n;
  if (hp > heap_end) {
    printf("heap exhausted\n"); exit(1);
  }
  return res;
}


#define stack_size 10
static value stack1[stack_size];
static value* stack1_start = &stack1[0];
static value* stack1_end = &stack1[stack_size];

static value stack2[stack_size];
static value* stack2_start = &stack2[0];
static value* stack2_end = &stack2[stack_size];


static value* stack;
static value* sp;
static value* sp_limit;

static value* args;
static value* ap;
static value* ap_limit;


void push_stack(value v) {
  //printf("push_stack(%ld) %p\n",(sp-stack),v);
  if (sp >= sp_limit) {
    printf("stack exhausted"); exit(1);
  }
  *sp++ = v;
}

void push_arg(value v) {
  //printf("push_arg %p\n",v);
  if (ap >= ap_limit) {
    printf("args exhausted"); exit(1);
  }
  *ap++ = v;
}

static value* kont = 0;
static char* code = 0;
static value* frame = 0;


static int count = 0;

static char next() {
  char c = *code++;
  //printf("[%d]'%c'...\n",count,c);
  count++;
  return c;
}

int digit() {
  char c;
  switch (c = next()) {
  case '0'...'9': { return c-'0'; }
  case 'x': {
    return 10 * digit() + digit();
  }
  }
  printf("digit: %c not expected\n", c); exit(1);
}

value argument() {
  char c;
  switch (c = next()) {
  case '0'...'9': {
    int n = c - '0';
    value res = stack[n];
    //printf("argument: stack(%d) is %p\n",n,res);
    return res;
  }
  case '$': {
    int n = digit();
    value res = lits[n];
    //printf("argument: lit(%d) is %p\n",n,res);
    return res;
  }
  case '~': {
    int n = digit();
    value res = frame[n];
    //printf("argument: frame(%d) is %p\n",n,res);
    return res;
  }
  }
  printf("argument: %c not expected\n", c); exit(1);
}


static value final_result = 0;

static void set_code(char* newCode) {
  //printf("set_code: %s\n", newCode);
  code = newCode;
}


static void return_to_kont(value v) {
  //printf("return %p -> %p\n",v,kont);
  if (kont) {
    set_code(kont[0]);
    frame = &kont[2];
    //printf("return, newCode = %s, frame=%p, frame[0]=%p\n",code,frame, frame[0]);
    kont = kont[1];
    sp = stack;
    push_stack(v);
  } else {
    //printf("kont=0, we must be done!\n");
    final_result = v;
    //next instr will be 0 and so main for-loop will terminate
  }
}

static value* this_closure = 0;
static void enter_clo(value* clo) {
  this_closure = clo;
  set_code(clo[0]);
  frame = &clo[1];
  //printf("tail, newCode = %s, frame=%p, frame[0]=%p\n",code,frame, frame[0]);
  value* tmp;
  tmp = stack; stack = args; args = tmp;
  tmp = sp_limit; sp_limit = ap_limit; ap_limit = tmp;
  sp = ap; ap = args;
}



int main() {

  init_max_code_ref();
  init_max_pap();

  stack = stack1_start;
  sp = stack1_start;
  sp_limit = stack1_end;

  args = stack2_start;
  ap = stack2_start;
  ap_limit = stack2_end;

  set_code(get_code_ref(0)); // can fail if there is no code
  char instr = 0;

  //printf("running...\n");

  while ((instr = next())) {
    switch(instr) {
    case 'u': {
      int dest = digit();
      //printf("unconditional jump, dest = %d\n", dest);
      set_code(get_code_ref(dest));
      break;
    }
    case 'j': {
      value cond = argument();
      int dest = digit();
      //printf("jump, dest = %d, condition=%p\n", dest,cond);
      if (cond) {
        set_code(get_code_ref(dest));
      }
      break;
    }
    case 'r': {
      value v = argument();
      return_to_kont(v);
      break;
    }
    case 't': {
      value funValue = argument();
      int nArgs = digit();
      //printf("tail, funValue=%p, nargs=%d\n",funValue,nArgs);
      for (int i = 1; i <= nArgs; i++) {
        value v = argument();
        //printf("tail arg(%d): value=%p\n",i, v);
        push_arg(v);
      }
      value* clo = (value*)funValue;
      enter_clo(clo);
      break;
    }
    case 'c': {
      int codeRef = digit();
      int nFree = digit();
      //printf("make closure, codeRef = %d, nFree = %d\n",codeRef,nFree);
      value* clo = grab(nFree+1);
      push_stack(clo);
      //printf("clo = %p\n",clo);
      clo[0] = prog[codeRef];
      for (int i = 0; i < nFree; i++) {
        value freeVal = argument();
        //printf("make closure, free(%d) is value=%p\n",i,freeVal);
        clo[i+1] = freeVal;
      }
      break;
    }

    case 'p': {
      int codeRef = digit();
      int nFree = digit();
      //printf("push_continuation, codeRef = %d, nFree = %d\n",codeRef,nFree);
      value* k = grab(nFree+2);
      //printf("k = %p\n",k);
      k[0] = prog[codeRef];
      k[1] = kont;
      kont = k;
      for (int i = 0; i < nFree; i++) {
        value freeVal = argument();
        //printf("push_continuation, free(%d) is value=%p\n",i,freeVal);
        k[i+2] = freeVal;
      }
      break;
    }
    case 'n': {
      int need = digit();
      int got = (sp - stack);
      //printf("arg check: expect=%d, got=%d\n",expect,got);
      if (got < need) {
        //printf("PAP\n");
        value* pap = grab(got+2);
        pap[0] = get_pap_got_need(got,need);
        pap[1] = this_closure;
        for (int i = 0; i < got; i++) {
          //printf("pap, save arg(%d) as free\n",i);
          pap[i+2] = stack[i];
        }
        return_to_kont(pap);
        //exit(1);
      }
      if (got > need) {
        printf("OVERAPP\n"); exit(1);
      }
      break;
    }
    case '=': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a == b;
      //printf("equal: a=%ld, b=%ld, res=%ld\n",a,b,res);
      push_stack((value)res);
      break;
    }
    case '<': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a < b;
      //printf("less-than: a=%ld, b=%ld, res=%ld\n",a,b,res);
      push_stack((value)res);
      break;
    }
    case '-': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a - b;
      //printf("subtract: a=%ld, b=%ld, res=%ld\n",a,b,res);
      push_stack((value)res);
      break;
    }
    case '+': {
      long a = (long)argument();
      long b = (long)argument();
      long res = a + b;
      //printf("add: a=%ld, b=%ld, res=%ld\n",a,b,res);
      push_stack((value)res);
      break;
    }
    case '%': {
      long a = (long)argument();
      long b = (long)argument();
      //printf("modulus: a=%ld, b=%ld, res...\n",a,b);
      long res = a % b;
      //printf("modulus: a=%ld, b=%ld, res=%ld\n",a,b,res);
      push_stack((value)res);
      break;
    }
    default: { printf("unknown byte: %d('%c')\n",instr,instr); exit(1); }
    }
  }

  printf("the final result is: %ld\n", (long)final_result);
  printf("heap used, %ld cells\n", (hp-heap));
  printf("#steps = %d \n", count);
  exit(0);
}
