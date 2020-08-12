#include "types.h" // for fixnum, gc_ptr
#include "gc.h" // for thread_init
#include <stdlib.h> // for exit, EXIT_FAILURE

#define closure_ty(...) \
  struct { void(*func)(gc_ptr env, __VA_ARGS__); gc_ptr env; }

typedef closure_ty(fixnum) exit_closure;

extern _Noreturn void hm_main(gc_ptr, exit_closure);

_Noreturn void exit_fn(gc_ptr env, fixnum status) {
  exit(status);
}

exit_closure exit_c = { .func = exit_fn, .env = NULL };

void thread_init() {
  gc_thread_init();
}

int main(int argc, char **argv) {
  thread_init();
  hm_main(NULL, exit_c);
  exit(EXIT_FAILURE);
}
