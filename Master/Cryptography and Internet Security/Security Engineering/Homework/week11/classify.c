#include <klee/klee.h>

int classify(int x)
{
  if (x == 42)
    return 1;

  if (x > 100 && x < 110)
    return 2;

  if (x * x == 25)
    return 3;

  if (x > 10 && x < 5)
    return 99;

  return 0;
}

int main()
{
  int a;

  klee_make_symbolic(&a, sizeof(a), "a");

  return classify(a);
}
