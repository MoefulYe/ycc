void putf(float f);

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
  float a = 1.0;
  float b = -4.0;
  float c = 4.0;
  float x = 0.0;
  float resloved = reslove(a, b, c, x);
  putf(resloved);
  return 0;
}
