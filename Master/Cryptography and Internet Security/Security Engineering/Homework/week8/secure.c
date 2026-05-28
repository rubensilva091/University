#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ITEMS 4

static void greet_user(const char *name)
{
  printf("Hello, %s!\n", name);
}

static int read_bonus(const char *path, long long *bonus_out)
{
  FILE *file = fopen(path, "r");
  char buffer[128];
  char *endptr;
  long long bonus_value;

  if (file == NULL)
  {
    return -1;
  }

  if (fgets(buffer, sizeof buffer, file) == NULL)
  {
    (void)fclose(file);
    return -1;
  }

  if (strchr(buffer, '\n') == NULL && !feof(file))
  {
    (void)fclose(file);
    return -1;
  }

  errno = 0;
  bonus_value = strtoll(buffer, &endptr, 10);
  if (errno != 0 || endptr == buffer)
  {
    (void)fclose(file);
    return -1;
  }

  while (*endptr == ' ' || *endptr == '\t' || *endptr == '\n' || *endptr == '\r' || *endptr == '\v' || *endptr == '\f')
  {
    endptr++;
  }

  if (*endptr != '\0' || bonus_value < LLONG_MIN || bonus_value > LLONG_MAX)
  {
    (void)fclose(file);
    return -1;
  }

  if (fclose(file) != 0)
  {
    return -1;
  }

  *bonus_out = bonus_value;
  return 0;
}

int main(int argc, char **argv)
{
  int values[MAX_ITEMS];
  long long total = 0;
  long long bonus = 0;
  int i;

  if (argc != 3)
  {
    fprintf(stderr, "Usage: %s <name> <bonus-file>\n", argv[0]);
    return 1;
  }

  greet_user(argv[1]);

  for (i = 0; i < MAX_ITEMS; i++)
  {
    values[i] = i * 10;
    total += values[i];
  }

  if (read_bonus(argv[2], &bonus) != 0)
  {
    fprintf(stderr, "Error: could not read bonus from %s\n", argv[2]);
    return 1;
  }

  if ((bonus > 0 && total > LLONG_MAX - bonus) || (bonus < 0 && total < LLONG_MIN - bonus))
  {
    fprintf(stderr, "Error: total would overflow\n");
    return 1;
  }

  printf("Total with bonus: %lld\n", total + bonus);
  return 0;
}
