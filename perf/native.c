
#include <stdio.h>
#include <time.h>

int nfib(int n) {
  if (n < 2) {
    return 1;
  } else {
    return nfib(n - 1) + nfib(n - 2) + 1;
  }
}

int main() {

  int n = 39;

  for (int i=0;i<20;i++) {
    clock_t start = clock();
    long result = nfib(n);
    clock_t end = clock();

    double duration = (end-start)/1000000.0;
    double nfibs_per_ms = result / (end - start);

    printf("n = %d, res = %ld, duration(s) = %.3g, nfibs_per_ms = %g\n",
           n, result, duration, nfibs_per_ms);
  }
}
