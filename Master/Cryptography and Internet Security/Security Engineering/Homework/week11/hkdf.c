#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include <klee/klee.h>

void HKDF_expand(size_t out_len) {
  const size_t digest_len = 32;
  size_t n, done = 0;
  unsigned i;

  n = (out_len + digest_len - 1) / digest_len;
  if (out_len + digest_len < out_len || n > 255) {
    return;
  }

  for (i = 0; i < n; i++) {
    size_t todo = digest_len;
    klee_assert(done + todo >= done);
    if (done + todo > out_len) {
      todo = out_len - done;
    }
    done += todo;
  }
}

int main() {
  size_t out_len;
  klee_make_symbolic(&out_len, sizeof(out_len), "out_len");
  HKDF_expand(out_len);
  return 0;
}
