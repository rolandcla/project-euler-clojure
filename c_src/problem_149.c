#include <stdio.h>

#define SIDE 2000

long ns[SIDE*SIDE];
long buf[SIDE];

int cp_hrow_to_buf(int y) {
  int ns_ix = y*SIDE;
  for (int i=0; i<SIDE; i++) buf[i] = ns[ns_ix++];
  return SIDE;
}

int cp_vcol_to_buf(int x) {
  int ns_ix = x;
  for (int i=0; i<SIDE; i++, ns_ix += SIDE) buf[i] = ns[ns_ix];
  return SIDE;
}

int cp_diag1_to_buf(int x) {
  int i,j;
  int ns_ix = x;
  for (i=0, j=x; j<SIDE; i++, j++, ns_ix += SIDE + 1) buf[i] = ns[ns_ix];
  return i;
}

int cp_diag2_to_buf(int x) {
  int i,j;
  int ns_ix = x;
  for (i=0, j=x; j>=0; i++, j--, ns_ix += SIDE - 1) buf[i] = ns[ns_ix];
  return i;
}

int cp_diag3_to_buf(int x) {
  int i,j;
  int ns_ix = SIDE*(SIDE-1)+x;
  for (i=0, j=x; j<SIDE; i++, j++, ns_ix -= SIDE - 1) buf[i] = ns[ns_ix];
  return i;
}

int cp_diag4_to_buf(int x) {
  int i,j;
  int ns_ix = SIDE*(SIDE-1)+x;
  for (i=0, j=x; j>=0; i++, j--, ns_ix -= SIDE - 1) buf[i] = ns[ns_ix];
  return i;
}

long best_sum(int n) {
  int i, j;
  long sum;
  long max_sum = 0;
  for (i=0; i<n; i++) {
    sum = 0;
    for (j=i; j<n; j++) {
      sum += buf[j];
      if (sum > max_sum) max_sum = sum;
    }
  }
  return max_sum;
}

int main(int argc, char *argv[])
{
  long i,k;
  long sk;
  long sum, max_sum;

  for(i=0; i<55; i++) {
    k = i + 1;
    sk = (100003 - 200003*k + 300007*k*k*k) % 1000000;
    ns[i] = sk - 500000;
  }

  for(i=55; i<SIDE*SIDE; i++) {
    sk = (ns[i-24] + ns[i-55] + 1000000) % 1000000;
    ns[i] = sk - 500000;
  }

  //printf("ns[9]=%d      ns[99]=%d\n", ns[9], ns[99]);

  max_sum = 0;
  for(i=0; i<SIDE; i++) {
    sum = best_sum(cp_hrow_to_buf(i));
    if (sum > max_sum) max_sum = sum;

    sum = best_sum(cp_vcol_to_buf(i));
    if (sum > max_sum) max_sum = sum;

    sum = best_sum(cp_diag1_to_buf(i));
    if (sum > max_sum) max_sum = sum;

    sum = best_sum(cp_diag2_to_buf(i));
    if (sum > max_sum) max_sum = sum;

    sum = best_sum(cp_diag3_to_buf(i));
    if (sum > max_sum) max_sum = sum;

    sum = best_sum(cp_diag4_to_buf(i));
    if (sum > max_sum) max_sum = sum;
  }

  //printf("max_sum=%d\n", max_sum);
  printf("%d", max_sum);
}


