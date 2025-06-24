#include <stdio.h>
#include <math.h>

//FICHA 1

float multInt1(int n, float m);
float multInt2(int n, float m);
float multInt3(int n, float m, int *count);

int mdc1(int a, int b);
int mdc2(int a, int b);
int mdc3(int a, int b, int *count);
int mdc4(int a, int b, int *count);

int fib(int n);
int fastfib(int n);

int main()
{
  int a, b, r1, r2, r3, r4, c1 = 0, c2 = 0;
  float f, f1, f2, f3;

  printf("Introduza dois numeros para input das funcoes de multiplicacao: ");
  /*scanf("%d", &a);
  scanf("%f", &f);*/
  a = 81, f = 423;
  f1 = multInt1(a, f);
  f2 = multInt2(a, f);
  f3 = multInt3(a, f, &c1);
  printf("Resultados das multiplicacoes: %f, %f, %f (%d)\n", f1, f2, f3, c1);

  printf("Introduza dois numeros para input das funcoes de multiplicacao: ");
  /*scanf("%d", &a);
  scanf("%d", &b);*/
  a = 126, b = 45;
  r1 = mdc1(a, b);
  r2 = mdc2(a, b);
  r3 = mdc3(a, b, &c1);
  r4 = mdc4(a, b, &c2);
  printf("Resultados do mdc: %d, %d, %d (%d), %d (%d)\n", r1, r2, r3, c1, r4, c2);

  printf("Introduza um numero para calcular o fib: ");
  /*scanf("%d", &a);*/
  a = 20;
  printf("FastFib (%d) = %d \n", a, fastfib(a));
  printf("Fib (%d) = %d \n", a, fib(a));

  return 0;
}

float multInt1(int n, float m)
{
  float r = 0;
  for (int i = 1; i <= n; i++)
  {
    r += m;
  }
  return r;
}

float multInt2(int n, float m)
{
  float r = 0;
  while (n > 0)
  {
    if (n % 2 == 1)
    {
      r += m;
    }
    m = m * 2;
    n = n / 2;
  }
  return r;
}

float multInt3(int n, float m, int *count)
{
  float r = 0;
  int counter = 0;
  while (n > 0)
  {
    counter++;
    if (n % 2 == 1)
    {
      r += m;
    }
    m = m * 2;
    n = n / 2;
  }
  *count = counter;
  return r;
}

int mdc1(int a, int b)
{
  int i, max;
  if (b > a)
  {
    int troca = a;
    a = b;
    b = troca;
  }
  for (i = 1; i <= b; i++)
  {
    if ((a % i == 0) && (b % i == 0))
    {
      max = i;
    }
  }
  return max;
}

int mdc2(int a, int b)
{
  int i, max;
  if (b > a)
  {
    int troca = a;
    a = b;
    b = troca;
  }
  while (a > 0)
  {
    if (a > b)
    {
      a = a - b;
    }
    if (a < b)
    {
      b = b - a;
    }
    if (a == b)
    {
      return a;
    }
  }
  return 0;
}

int mdc3(int a, int b, int *count)
{
  int i, max, counter = 0;
  if (b > a)
  {
    int troca = a;
    a = b;
    b = troca;
  }
  while (a > 0)
  {
    counter++;
    if (a > b)
    {
      a = a - b;
    }
    if (a < b)
    {
      b = b - a;
    }
    if (a == b)
    {
      *count = counter;
      return a;
    }
  }
  *count = counter;
  return 0;
}

int mdc4(int a, int b, int *count) /*Nao funciona e nao tenho paciencia*/
{
  int i, max, counter = 0;
  if (b > a)
  {
    int troca = a;
    a = b;
    b = troca;
  }
  while (a > 0)
  {
    counter++;
    if (a > b)
    {
      a = a - b; /*Substituir subtraçoes por %*/
    }
    if (a < b)
    {
      b = b - a; /*Substituir subtraçoes por %*/
    }
    if (a == b)
    {
      *count = counter;
      return a;
    }
  }
  *count = counter;
  return 0;
}

int fib(int n) /*Nao sei fds*/
{
}

int fastfib(int n)
{
  return 0;
}