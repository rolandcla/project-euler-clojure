#include <stdio.h>

#define HEIGHT 1000
#define N (HEIGHT * (HEIGHT+1))/2

int triangle[N];
int row_ixs[HEIGHT+1];

void init()
{
  int i, j, ix, t, sk;
  t = 0;
  for (i=0; i<N; i++) {
    t = (615949*t + 797807) & 0xfffff;
    sk = t - 0x80000;
    triangle[i] = sk;
  }
  for (i=0, ix=0, j=1; i<HEIGHT+1; i++, ix+=j++) {
    row_ixs[i] = ix;
  }

  //printf("s1=%d s2=%d s3=%d\n", triangle[0], triangle[1], triangle[2]);
  //printf("ixs: %d %d %d ... %d %d %d\n",
  //       row_ixs[0], row_ixs[1], row_ixs[2], row_ixs[HEIGHT-2], row_ixs[HEIGHT-1], row_ixs[HEIGHT]);
}

int min_trg_from(int y, int x) {
  int ix, i, j;
  int sum = 0;
  int min_sum = 0;
  for (i=0; i<HEIGHT-y; i++) {
    ix = row_ixs[i+y]+x;
    for(j=0; j<=i; j++) sum += triangle[ix+j];
    if (sum < min_sum) {
      min_sum = sum;
    }
  }
  return min_sum;
}

void print_triangle() {
  for(int i=0, ix=0; i<HEIGHT; i++) {
    for(int j=0; j<=i; j++, ix++)
      printf("%8d ", triangle[ix]);
    printf("\n");
  }
}

int main(int argc, char *argv[])
{
  int h,i;
  int sum;
  int min_sum = 0;

  init();
  //print_triangle();

  for(h=0; h<HEIGHT; h++)
    for(i=0; i<=h; i++) {
      sum = min_trg_from(h, i);
      if (sum < min_sum) min_sum = sum;
    }
  printf("%d", min_sum);
}
