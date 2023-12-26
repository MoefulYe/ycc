void putf(float f);
float getf();
void cr();

const float EPSILON = 1.0e-10;

bool equal(float x, float y) {
  float delta = x - y;
  return -EPSILON < delta && delta < EPSILON;
}

float reslove(float a, float b, float c, float x) {
  float gradient = 2.0 * a * x + b;
  float y = x * x * a + x * b + c;
  float next_x = x - y / gradient;
  if (equal(x, next_x)) {
    return x;
  } else {
    return reslove(a, b, c, next_x);
  }
}

int main() {
  while (true) {
    float a = getf();
    float b = getf();
    float c = getf();
    float x = getf();
    float resloved = reslove(a, b, c, x);
    putf(resloved);
    cr();
  }
  return 0;
}
