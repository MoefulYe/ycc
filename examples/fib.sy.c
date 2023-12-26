int geti();
void puti(int x);
void sp();
void cr();

int fib(int x) {
  if (x <= 0) {
    return 0;
  } else if (x == 1 || x == 2) {
    return 1;
  } else {
    return fib(x - 1) + fib(x - 2);
  }
}

int main() {
  while (true) {
    int x = geti();
    if (x < 0) {
      break;
    }
    int y = fib(x);
    puti(y);
    cr();
  }
  return 0;
}
