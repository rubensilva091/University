#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ITEMS 4

static void greet_user(const char *name)
{
  char buf[16];
  strcpy(buf, name);
  printf("Hello, %s!\n", buf);
}

static int read_bonus(const char *path)
{
  FILE *f = fopen(path, "r");
  int bonus;

  fscanf(f, "%d", &bonus);
  fclose(f);

  return bonus;
}

int main(int argc, char **argv)
{
  char *message = malloc(24);
  int *v, *values = malloc(MAX_ITEMS * sizeof(int));
  int admin;
  int i;
  int total = 0;

  if(argc < 3)
  { fprintf(stderr, "Usage: %s <name> <bonus-file>\n", argv[0]);
    return 1;
  }

  greet_user(argv[1]);

  for(i=0, v=values; i <= MAX_ITEMS; i++)
  { v[i] = i * 10;
    total += v[i];
  }
  free(v);

  if (admin)
  { puts("Admin mode enabled"); }

  sprintf(message, "Total with bonus: %d", total + read_bonus(argv[2]));
  puts(message);

  free(values);
  return 0;
}
