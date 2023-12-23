#include <stdio.h>
extern int is_even(int x);

int main() {
  for (int a = 0; a < 16; a++) {
    printf("%d", is_even(a));
  }
  return 0;
}
