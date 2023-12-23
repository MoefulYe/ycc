void puti(int i);
void cr();

int main() {
  int n = 100;
  while (n < 1000) {
    int hun = n / 100;
    int ten = (n - hun * 100) / 10;
    int ind = n % 10;
    if (n == hun * hun * hun + ten * ten * ten + ind * ind * ind) {
      puti(n);
      cr();
    }
    n = n + 1;
  }
  return 0;
}
