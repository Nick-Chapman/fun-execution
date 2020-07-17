
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

  int n = 25;

  long acc_result = 0;
  double acc_duration_us = 0;

  for (;;) {
    static char arg[50];
    sprintf(arg,"%d",n);

    clock_t start = clock();
    long result = nfib(arg);
    clock_t end = clock();

    double duration_us = (end-start);
    double duration_s = duration_us/1000000.0;
    double nfibs_per_us = (double)result / duration_us;

    // show (hopefully) converging numbers
    acc_result += result;
    acc_duration_us += duration_us;
    double conv_nfibs_per_us = (double)acc_result / acc_duration_us;

    printf("n = %d, res = %8ld, time(s) = %.03g, speed(nfibs/us) = %.3g (%.3g)\n",
           n, result, duration_s, nfibs_per_us, conv_nfibs_per_us);

    if (duration_s < 0.2) n++;

  }
}
