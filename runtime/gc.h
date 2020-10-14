#pragma once
#include "types.h"

// these functions are generated by the compiler for every gc
// allocation, and are responsible only for calling
// `collect_and_relocate` on each of the gc pointers in `old` and
// placing the results in `new`. `new` may be the same as `old`, and
// if not, it will have `old` memcpy'd into it prior to a call to this
// function.
typedef void(*gc_recurse_func)(gc_ptr old, gc_ptr new);

gc_ptr gcalloc(fixnum size,
               gc_recurse_func recurse,
               gc_ptr live_ptrs[],
               fixnum live_ptr_ct);

gc_ptr collect_and_relocate(gc_ptr old);

void gc_thread_init();