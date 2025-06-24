#include <stdio.h>
#include <math.h>

//FICHA 1

void ex1_quadrado();
void ex2_xadrez();
void ex3_triangulos();
int ex4_circulo();

int main()
{
  int key, count;
  printf("--------------------MENU--------------------\n\n");
  printf("(1) -> Quadrado com #\n");
  printf("(2) -> Quadrado com Xadrez\n");
  printf("(3) -> Quadrado com triangulos\n");
  printf("(4) -> Quadrado com circulo\n\n");
  printf("Outra qualquer para sair\nEscolha a opcao: ");
  scanf("%d", &key);
  system("CLS");
  switch (key)
  {
  case 1:
    ex1_quadrado();
    system("PAUSE");
    break;
  case 2:
    ex2_xadrez();
    system("PAUSE");
    break;
  case 3:
    ex3_triangulos();
    system("PAUSE");
    break;
  case 4:
    count = ex4_circulo();
    printf("\n\nForam usados %d # na criacao do Circulo\n", count);
    system("PAUSE");
    break;
  default:
    break;
  }
}

void ex1_quadrado()
{
  int key;
  printf("Que tamanho deseja para o quadrado: ");
  scanf("%d", &key);
  for (int t = 1; t <= key; t++)
  {
    printf("\n");
    for (int i = 1; i <= key; i++)
    {
      printf("#");
    }
  }
}

void ex2_xadrez()
{
  int key, counter = 1, counterLinhas = 1;
  ;
  printf("Que tamanho deseja para o Xadrez: ");
  scanf("%d", &key);
  for (int t = 1; t <= (key * key); t++)
  {
    if (counterLinhas % 2 != 0)
    {
      if (t % 2 != 0)
      {
        printf("#");
      }
      else
      {
        printf("_");
      }
    }
    else
    {
      if (t % 2 == 0)
      {
        printf("#");
      }
      else
      {
        printf("_");
      }
    }

    if (counter == key)
    {
      printf("\n");
      counter = 0;
      counterLinhas++;
    }
    counter++;
  }
}

void ex3_triangulos()
{
  int key, counter = 0;
  printf("Que altura deseja para o Triangulo: ");
  scanf("%d", &key);
  for (int i = 0; i < key; i++)
  {
    printf("\n");
    for (int t = 0; t < i; t++)
    {
      printf("#");
    }
  }
  for (int i = key; i > 0; i--)
  {
    printf("\n");
    for (int t = i; t > 0; t--)
    {
      printf("#");
    }
  }
  printf("\n\n");
  for (int i = key; i > 0; i--)
  {
    for (int t = key - counter; t > 0; t--)
    {
      printf(" ");
    }
    for (int j = counter + (counter + 1); j > 0; j--) /*Somei counter + counter + 1 pois é o lado da esquerda + direita + centro*/
    {
      printf("#");
    }
    printf("\n");
    counter++;
  }
}

int ex4_circulo()
{
  int raio, perimetro, counter = 0;
  printf("Que raio deseja para o circulo: ");
  scanf("%d", &raio);
  perimetro = 2 * raio;
  for (int i = 0; i <= perimetro; i++)
  {
    for (int j = 0; j <= perimetro; j++)
    {
      double distancia = sqrt((double)(i - raio) * (i - raio) + (j - raio) * (j - raio));
      if (distancia <= raio)
      {
        printf("#");
        counter++;
      }
      else
      {
        printf(" ");
      }
    }
    printf("\n");
  }
  return counter;
}