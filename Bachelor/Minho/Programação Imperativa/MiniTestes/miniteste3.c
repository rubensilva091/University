#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <math.h>

/*PERGUNTA 1*/

int hash(char s[])
{
  int r = 0, count=0;
  count =strlen(s);
  while (*s)
  {
    r += (*s * count);
    s++;
    count--;
  }
  return r;
}

int main()
{
  int z =hash ("programacao funcional");
  printf("%d",z);
}

/*int hash(char s[])
{
  int r = 0, count = 0;
  while (*s)
  {
    r += *s;
    s++;
    count--;
  }
  return r;
}

int next(char s[], int n)
{
  while (n > 0)
  {
    n--;
    if ((s[n] < 'z' && s[n] >= 'a') || (s[n] >=0 && s[n] < 10))
    {
      s[n]++;
      return 1;
    }
    else
    {
      if (s[n] < 'z' && s[n] >= 'a')
      {
        s[n] =0;
      }
      else
      {
        s[n] = 'a';
      }
    }
  }
  return 0;
}

int main()
{
  char s[10];
  int n, i, z;
  for (n = 0; n < 10; n++)
  {
    for (i = 0; i < n; i++)
      s[i] = 'a';
    s[n] = '\0';
    do
    {
      z = hash(s);
      if (hash(s) == 294)
      {
        printf("%s ->%d\n", s, z);
        break;
      }
    } while (next(s, n));
  }
  return 0;
}*/