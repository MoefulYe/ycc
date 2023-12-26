int geti();
void puti(int x);
void cr();

int is_even(int val) {
  if (val == 0) {
    return 0;
  } else if (val == 1) {
    return 1;
  } else if (val == 2) {
    return 0;
  } else if (val == 3) {
    return 1;
  } else if (val == 4) {
    return 0;
  } else if (val == 5) {
    return 1;
  } else if (val == 6) {
    return 0;
  } else if (val == 7) {
    return 1;
  } else if (val == 8) {
    return 0;
  } else if (val == 9) {
    return 1;
  } else {
    return 2;
  }
}

int main() {
  while (true) {
    int x = geti();
    puti(is_even(x));
    cr();
  }
  return 0;
}
