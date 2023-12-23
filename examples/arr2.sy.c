void puti(int x);
void sp();
void cr();

int arr[2][2] = {{1, 2}, {3, 4}};

void inc(int arr[][2]) {
  int i = 0;
  while (i < 2) {
    int j = 0;
    while (j < 2) {
      int val = arr[i][j];
      arr[i][j] = val + 1;
      j = j + 1;
    }
    i = i + 1;
  }
}

int main() {
  inc(arr);
  int i = 0;
  while (i < 2) {
    int j = 0;
    while (j < 2) {
      puti(arr[i][j]);
      sp();
      j = j + 1;
    }
    cr();
    i = i + 1;
  }
  return 0;
}
