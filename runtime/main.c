#include <stdint.h>
#include <stdlib.h>

typedef uint8_t *cenv_ty;
#define closure_ty(...) struct { void(*func)(cenv_ty, __VA_ARGS__); cenv_ty env; }

typedef closure_ty(int64_t) exit_closure;

extern _Noreturn void hm_main(cenv_ty, exit_closure);

_Noreturn void exit_fn(cenv_ty env, int64_t status) {
  exit(status);
}

exit_closure exit_c = { .func = exit_fn, .env = NULL };

int main(int argc, char **argv) {
  hm_main(NULL, exit_c);
  exit(EXIT_FAILURE);
}

uint8_t *gcalloc(uint64_t size) {
  if (!size) {
    return NULL;
  }
  uint8_t *buf = malloc(size);
  if (!buf) {
    exit(EXIT_FAILURE);
  }
  return buf;
}
