void puti(int i);
void sp();
void cr();

int main() {
  int a[2][2] = {{1, 2}, {3, 4}};
  int i;
  i = 0;
  while (i < 2) {
    int j = 0;
    while (j < 2) {
      int elem = a[i][j];
      a[i][j] = elem + 1;
      j = j + 1;
    }
    i = i + 1;
  }
  i = 0;
  while (i < 2) {
    int j = 0;
    while (j < 2) {
      puti(a[i][j]);
      sp();
      j = j + 1;
    }
    cr();
    i = i + 1;
  }
  return 0;
}
