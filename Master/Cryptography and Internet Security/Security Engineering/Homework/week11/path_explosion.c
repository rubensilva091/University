#include <assert.h>
#include <klee/klee.h>

int main(void)
{
  unsigned char input[16];
  int score = 0;

  klee_make_symbolic(input, sizeof(input), "input");

  for (int i = 0; i < 16; i++)
  {
    if (input[i] == 'A')
      score += 1;
    else
      score -= 1;
  }

  if (score == 16)
  { klee_assert(0 && "all bytes were A"); }

  return 0;
}
