void puti(int i);
void putspace();
void putcr();

int main() {
  int i = 0;
  while (i < 3) {
    int j = 0;
    while (j < 3) {
      int k = 0;
      while (k < 3) {
        puti(i);
        putspace();
        puti(j);
        putspace();
        puti(j);
        putcr();
        k = k + 1;
      }
      j = j + 1;
    }
    i = i + 1;
  }
  return 0;
}
