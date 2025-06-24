#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <math.h>

int sumhtpo(int n, int *z)
{
  int r = 0, j = 0;
  while (n != 1)
  {
    r += n;
    if (n % 2 == 0)
    {
      n = n / 2;
    }
    else
    {
      n = 1 + (3 * n);
    }
    z[j] = n;
    j++;
  }
  return j;
}

void bubbleSort(int *key, int nVariaveis)
{
  for (int w = 0; w < nVariaveis; w++) /*Bubble Sort*/
  {
    for (int z = 0; z < nVariaveis - w - 1; z++)
    {
      if (key[z] > key[z + 1])
      {
        int troca;
        troca = key[z];
        key[z] = key[z + 1];
        key[z + 1] = troca;
      }
    }
  }
}

int selectionSort(int arr[], int n)
{
  int i, j, min_idx, counter = 0;
  for (i = 0; i < n - 1; i++)
  {
    min_idx = i;
    for (j = i + 1; j < n; j++)
      if (arr[j] > arr[min_idx])
      {
        int troca;
        min_idx=j;
        troca = arr[min_idx];
        arr[min_idx] = arr[j];
        arr[j] = troca;
        counter++;
      }
  }
  return counter;
}

int main()
{
  int z[1000], counter1, counter2;
  counter1 = sumhtpo(6, z);
  bubbleSort(z, counter1);
  printf("%d", counter2);
}