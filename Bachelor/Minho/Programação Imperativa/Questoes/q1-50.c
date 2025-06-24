#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <math.h>

/*1*/

void ex1()
{
  int x, max = 0;
  do
  {
    scanf("%d", &x);
    if (x > max)
    {
      max = x;
    }
  } while (x != 0);
  printf("%d", max);
}

/*2*/

void ex2()
{
  int x, soma = 0, counter = 0;
  float median;
  do
  {
    scanf("%d", &x);
    soma += x;
    counter++;
  } while (x != 0);
  median = (float)soma / counter;
  printf("%f", median);
}

/*3*/

void ex3()
{
  int x, max = 0, secondMax = 0;
  do
  {
    scanf("%d", &x);
    if (x > secondMax)
    {
      if (x > max)
      {
        secondMax = max;
        max = x;
      }
      else
      {
        secondMax = x;
      }
    }
  } while (x != 0);
  printf("%d", secondMax);
}

/*4*/

int ex4(unsigned int x)
{
  int r = 0;
  if (x % 2 != 0)
  {
    r = 1;
  }
  do
  {
    x = x / 2.0;
    if (x % 2 == 1)
    {
      r++;
    }
  } while (x > 0);
  return r;
}

/*5*/ /*Nao percebi a puta da pergunta E PQ QUE A ENGENHEIRA FAZ RECURSIVIDADE EM C CARALHOOOOO*/

/*int trailingZ (unsigned int n) {
    if(n % 2) return 0;
    else return 1 + trailingZ(n >> 1);
}*/

/*6*/

int ex6(unsigned int n)
{
  int counter = 0;
  do
  {
    n = n / 10;
    counter++;
  } while (n >= 1);
  return counter;
}

/*7*/

char *ex7(char s1[], char s2[])
{
  int i, j;
  for (i = 0; s1[i] != '\0'; i++)
  {
    for (j = 0; s2[j] != '\0'; j++)
    {
      s1[i + j] = s2[j];
    }
  }
  s1[i + j] = '\0';
  return s1;
}

/*8*/

char *ex8(char dest[], char source[]) /*DECORAR*/
{
  int i;
  for (i = 0; source[i] != '\0'; i++)
  {
    dest[i] = source[i];
  }
  dest[i + 1] = '\0'; /*Senao rip Destination*/
  return dest;
}

/*9*/

char *ex9(char s1[], char s2[])
{
  int i, t;
  for (i = 0; s1[i] != '\0' || s2[i] != '\0'; i++)
  {
    if (s1[i] != s2[i])
    {
      if (s1[i] > s2[i])
      {
        return 1;
      }
      else
      {
        return -1;
      }
    }
  }
  return 0;
}

/*10*/ /*AINDA AINDA NAO ACABEI*/

char *ex10(char s1[], char s2[])
{
  int i, t, j;
  for (i = 0; s1[i] != '\0'; i++)
  {
    for (t = 0; s2[t] != '\0'; t++)
    {
      if (s1[i + t] == s2[t] && s2[t + 1] == '\0') /*WHAT A LEGEND, FCK RISING FAN*/
      {
        return s1;
      }
    }
  }
  return NULL;
}

/*11*/

void ex11(char s[]) /*Mel, nao funciona aqui, mas no compilador do stor, funfa*/
{
  int i, end = 0;
  for (i = 0; s[i]; i++)
  {
    end++;
  }
  end--;
  for (i = 0; i < end; i++)
  {
    char temp = s[i];
    s[i] = s[end];
    s[end] = temp;
    end--;
  }
}

void ex12(char s[])
{
  int i, j;
  for (i = 0; s[i]; i++)
  {
    if (s[i] == 'A' || s[i] == 'a' || s[i] == 'E' || s[i] == 'e' || s[i] == 'I' || s[i] == 'i' || s[i] == 'O' || s[i] == 'o' || s[i] == 'U' || s[i] == 'u')
    {
      for (j = i; s[j]; j++)
      {
        s[j] = s[j + 1];
      }
      i--;
    }
  }
}

void ex13(char t[], int n) /*Nao funciona*/
{
  int i, j = 0, z;
  for (i = 0; t[i]; i++)
  {
    if (t[i] == '\n' || t[i] == ' ' || t[i] == '\t' || j >= n)
    {
      j = 0;
      for (z = i; t[z]; z++)
      {
        if (t[z] == '\n' || t[z] == ' ' || t[z] == '\t') /*Aqui IG*/
        {
          continue;
        }
        else
        {
          t[z] = t[z + 1];
        }
      }
      i--;
    }
  }
}

char ex14(char s[])
{
  if (!s)
  {
    return 0;
  }
  int counter, counterS = 0;
  char max;
  int i, j;
  for (i = 0; s[i]; i++)
  {
    counter = 0;
    for (j = 0; s[j]; j++)
    {
      printf("%c == %c?\n", s[i], s[j]);
      if (s[i] == s[j])
      {
        counter++;
      }
    }
    if (counter >= counterS)
    {
      max = s[i];
      counterS = counter;
    }
  }
  return max;
}

int ex15(char s[])
{
  int count = 1, calc = 0, i;
  for (i = 0; s[i]; i++)
  {
    if (s[i] == s[i + 1])
    {
      count++;
    }
    else
    {
      count = 1;
    }
    if (count > calc)
    {
      calc = count;
    }
  }
  return calc;
}

int ex16(char s[]) /*TA CORRETO, NEM O DA ENGENHEIRA FUNFA*/
{
  int count = 1, counterS = 0, i;
  for (i = 0; i < s[i]; i++)
  {
    if (s[i] != s[i + 1] && s[i] != ' ' && s[i + 1] != ' ')
    {
      count++;
      if (count > counterS)
      {
        counterS = count;
      }
    }
    else
    {
      count = 1;
    }
    if (count > counterS)
    {
      counterS = count;
    }
  }
  return counterS;
}

int ex17(char s1[], char s2[])
{
  int i, count = 0;
  for (i = 0; s1[i] && s2[i]; i++)
  {
    if (s1[i] == s2[i])
    {
      count++;
    }
    else
    {
      break;
    }
  }
  return count;
}
int calTamanho(char s[])
{
  int i;
  for (i = 0; s[i]; i++)
  {
    i++;
  }
  return i;
}

int ex18(char s1[], char s2[])
{
  int sizeS1 = strlen(s1) - 1;
  int sizeS2 = strlen(s2) - 1;
  int i, count = 0;
  while (s1[sizeS1] && s2[sizeS2])
  {
    if (s1[sizeS1] == s2[sizeS2])
    {
      count++;
    }
    else
    {
      break;
    }
    sizeS2--;
    sizeS1--;
  }
  return count;
}

int ex19(char s1[], char s2[])
{
  int ans = 0, i, j = 0;
    for(i = 0; s1[i]; i++) {
        if(s1[i] == s2[j++]) ans++;
        else ans = j = 0;
    }
    return ans;
}

int main()
{
  int x = ex18("ola mundo", "Ola Mundo");
  printf("%d", x);
}
