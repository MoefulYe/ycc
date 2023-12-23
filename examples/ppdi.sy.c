void puti(int i);
void putcr();

int main() {
  int n = 100;
  while (n < 1000) {
    int hun = n / 100;
    int ten = (n - hun * 100) / 10;
    int ind = n % 10;
    if (n == hun * hun * hun + ten * ten * ten + ind * ind * ind) {
      puti(n);
      putcr();
    }
    n = n + 1;
  }
  return 0;
}
