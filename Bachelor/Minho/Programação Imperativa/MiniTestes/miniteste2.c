#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <math.h>

int sumhtpo(int n)
{
  int r = 0;
  int y = 0;
  while (n != 1)
  {
    printf("%d +", n);
    r += n;
    if (n % 2 == 0)
    {
      n = n / 2;
    }
    else
    {
      n = 1 + (3 * n);
    }

    y++;
  }
  printf("= %d", n);
  return y;
}

int fib(int n)
{
  if (!n)
    return 0;
  int a = 1, b = 1, j = 0;
  while (n > 2)
  {
    int aux = a;
    a = a + b;
    b = aux;
    n--;
    j++;
  }
  return j;
}

int main()
{
  int z = sumhtpo(6);
  /* int z = fib(47);*/
  printf(" %d", z);
}
