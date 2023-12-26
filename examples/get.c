#include <assert.h>
#include <stdio.h>

int geti() {
  int ret = 0;
  assert(scanf("%d", &ret) == 1 && "get integer fail!");
  return ret;
}

float getf() {
  float ret = 0.0;
  assert(scanf("%f", &ret) == 1 && "get float fail!");
  return ret;
}
