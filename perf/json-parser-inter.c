
#include <stdio.h>
#include <time.h>

#include "value.h"

extern value run_engine(int argc, char* argv[]);

int json_parser_pipeline(char* n) {
  int argc = 2;
  char* argv[] = {"",n};
  value v = run_engine(argc,argv);
  return (long)v;
}

int main() {

  char* n = "15"; // 16 causes Segmentation fault. TODO: why?

  for (;;) {
    clock_t start = clock();
    long size = json_parser_pipeline(n);
    clock_t end = clock();

    long size_k = size / 1000;

    double duration_s = (end-start)/1000000.0;
    double speed_k_per_s = size_k / duration_s;

    printf("n = '%s', size(k) = %ld, duration(s) = %.3g, speed(k/s) = %.3g\n",
           n, size_k, duration_s, speed_k_per_s);
  }
}
