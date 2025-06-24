#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <math.h>

int func(int x)
{
  int r = 0;
  while (x > 0)
  {
    r += 1;
    x = x - r;
  }
  return r;
}

void pergunta2(int z)
{
  int x, y;
  for (y = 0; y < 8; y++)
  {
    for (x = 0; x < 8; x++)
    {
      if (x * y != z)
        putchar('#');
      else
        putchar('.');
    }
    putchar('\n');
  }
}

int main()
{
  int z;
  z = func(10);
  printf("\n%d\n\n", z);
  pergunta2(0);
}
