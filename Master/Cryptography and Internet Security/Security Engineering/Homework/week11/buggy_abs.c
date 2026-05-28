#include <assert.h>
#include <klee/klee.h>

int buggy_abs(int x)
{
  if (x < 0)
    return -x;

  return x;
}

int main()
{
  int a;

  klee_make_symbolic(&a, sizeof(a), "a");

  int r = buggy_abs(a);

  klee_assert(r >= 0);

  return 0;
}
