
#include <stdio.h>
#include <time.h>

#include "value.h"

extern value run_engine(int argc, char* argv[]);

int nfib(char* n) {
  int argc = 2;
  char* argv[] = {"",n};
  value v = run_engine(argc,argv);
  return (long)v;
}

int main() {

  char* n = "30";

  for (int i=0;i<20;i++) {
    clock_t start = clock();
    long result = nfib(n);
    clock_t end = clock();

    double duration = (end-start)/1000000.0;
    double nfibs_per_ms = (double)result / (end - start);

    printf("n = '%s', res = %ld, duration(s) = %.3g, nfibs_per_ms = %.3g\n",
           n, result, duration, nfibs_per_ms);
  }
}
