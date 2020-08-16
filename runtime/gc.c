#include "types.h" // for fixnum, gc_ptr
#include "gc.h"
#include <sys/mman.h> // for mmap
#include <stdbool.h> // for bool
#include <errno.h> // for errno
#include <stdlib.h> // for exit, EXIT_FAILURE
#include <string.h> // for memcpy
#include <stdio.h> // for fputs, stderr, perror

struct nursery_header {
  gc_recurse_func recurse;
  fixnum size;
};

enum gc_mark {
  GC_RED, GC_BLACK
};

struct large_space_header {
  gc_recurse_func recurse;
  fixnum size;
  fixnum generation;
  enum gc_mark mark;
};

struct generation {
  fixnum number;
  struct large_space_header **contents_start;
  struct large_space_header **contents_fill;
  struct large_space_header **contents_end;
};

const fixnum PAGE_SIZE = 0x1000;
const fixnum NURSERY_SIZE = PAGE_SIZE * 0x10;
const fixnum GEN_NURSERY = 0;
const fixnum FIRST_LARGE_GEN = 1;

_Thread_local gc_ptr nursery_start;
_Thread_local gc_ptr nursery_fill_ptr;
_Thread_local gc_ptr nursery_end;
_Thread_local fixnum nursery_ct;

_Thread_local fixnum this_collection_gen;

_Thread_local struct generation *generations;

const fixnum MAX_GEN = PAGE_SIZE / sizeof(struct generation);

static fixnum generation_size(fixnum n) {
  if (n == GEN_NURSERY) {
    return 0;
  }
  return PAGE_SIZE << n;
}

static void *mmap_anon(fixnum size) {
  return mmap(NULL,
              size,
              PROT_READ | PROT_WRITE,
              MAP_ANON | MAP_PRIVATE,
              -1,
              0);
}

_Thread_local enum gc_mark gc_live_mark;

static void alloc_generation(fixnum n) {
  if (generations[n].contents_start) {
    return;
  }
  fixnum size = generation_size(n);
  void *buf = mmap_anon(size);
  if (buf == MAP_FAILED) {
    perror("alloc_generation");
    exit(errno);
  }
  generations[n].contents_start = buf;
  generations[n].contents_fill = buf;
  generations[n].contents_end = buf + size;
}

static void large_space_init() {
  generations = mmap(NULL,
                     PAGE_SIZE,
                     PROT_READ | PROT_WRITE,
                     MAP_ANON | MAP_PRIVATE,
                     -1,
                     0);
  if (generations == MAP_FAILED) {
    perror("large_space_init");
    exit(errno);
  }
  for (fixnum n = 0; n < MAX_GEN; ++n) {
    generations[n].number = n;
  }
  gc_live_mark = GC_RED;
  alloc_generation(FIRST_LARGE_GEN);
}

static void push_into_gen(fixnum gen, struct large_space_header *alloc) {
  *(generations[gen].contents_fill++) = alloc;
}

static void nursery_init() {
  nursery_start = mmap(NULL,
                       NURSERY_SIZE,
                       PROT_READ | PROT_WRITE,
                       MAP_ANON | MAP_PRIVATE,
                       -1,
                       0);
  if (nursery_start == MAP_FAILED) {
    perror("nursery_init");
    exit(errno);
  }
  nursery_fill_ptr = nursery_start;
  nursery_end = nursery_start + NURSERY_SIZE;
  nursery_ct = 0;
}

void gc_thread_init() {
  nursery_init();
  large_space_init();
}

static fixnum total_size_in_nursery(fixnum size) {
  return size + sizeof(struct nursery_header);
}

static bool fits_in_nursery(fixnum size) {
  return total_size_in_nursery(size) < NURSERY_SIZE;
}

static gc_ptr nursery_alloc(fixnum body_size, gc_recurse_func recurse) {
  ++nursery_ct;

  fixnum total_size = total_size_in_nursery(body_size);
  gc_ptr alloc = nursery_fill_ptr;
  gc_ptr new_fill = alloc + total_size;
  if (new_fill >= nursery_end) {
    return NULL;
  }
  nursery_fill_ptr = new_fill;
  struct nursery_header *header = (struct nursery_header *)alloc;
  header->recurse = recurse;
  header->size = body_size;
  gc_ptr body = alloc + sizeof(struct nursery_header);
  return body;
}

static fixnum total_size_in_large_space(fixnum body_size) {
  return body_size + sizeof(struct large_space_header);
}

static gc_ptr large_space_alloc(fixnum body_size, gc_recurse_func recurse) {
  gc_ptr alloc = malloc(total_size_in_large_space(body_size));
  if (!alloc) {
    perror("large_space_alloc");
    exit(errno);
  }
  struct large_space_header *header = (struct large_space_header *)alloc;
  header->recurse = recurse;
  header->size = body_size;
  header->generation = FIRST_LARGE_GEN;
  header->mark = gc_live_mark;
  push_into_gen(FIRST_LARGE_GEN, header);
  gc_ptr body = alloc + sizeof(struct large_space_header);
  return body;
}

static bool gen_could_alloc(fixnum gen, fixnum count) {
  struct large_space_header **after = generations[gen].contents_fill + count;
  return after < generations[gen].contents_end;
}

static fixnum generation_count(fixnum gen) {
  return generations[gen].contents_fill - generations[gen].contents_start;
}

static fixnum highest_gen_to_collect() {
  if (gen_could_alloc(FIRST_LARGE_GEN, nursery_ct)) {
    return 0;
  }
  for (fixnum gen = FIRST_LARGE_GEN; gen != (MAX_GEN - 1); ++gen) {
    if (gen_could_alloc(gen + 1, generation_count(gen))) {
      return gen;
    }
  }
  return MAX_GEN;
}

static struct nursery_header *nursery_header_from_body(gc_ptr body) {
  return ((struct nursery_header *)body) - 1;
}

static gc_ptr move_from_nursery(gc_ptr old) {
  struct nursery_header *header = nursery_header_from_body(old);
  gc_ptr new = large_space_alloc(header->size, header->recurse);
  memcpy(new, old, header->size);
  (header->recurse)(old, new);
  return new;
}

static struct large_space_header *large_space_header_from_body(gc_ptr body) {
  return ((struct large_space_header *)body) - 1;  
}

static void mark(gc_ptr live) {
  struct large_space_header *header = large_space_header_from_body(live);
  if (header->generation > this_collection_gen) {
    return;
  }
  header->mark = gc_live_mark;
  header->generation += 1;
  (header->recurse)(live, live);
}

static bool in_nursery(gc_ptr obj) {
  return (obj >= nursery_start) && (obj < nursery_end);
}

gc_ptr collect_and_relocate(gc_ptr old) {
  if (in_nursery(old)) {
    return move_from_nursery(old);    
  } else if (this_collection_gen) {
    mark(old);
  }
  return old;
}

static void swap_live_mark() {
  switch (gc_live_mark) {
  case GC_RED:
    gc_live_mark = GC_BLACK;
    return;
  case GC_BLACK:
    gc_live_mark = GC_RED;
    return;
  }
}

static void sweep() {
  alloc_generation(this_collection_gen + 1);
  for (fixnum gen = this_collection_gen; gen != GEN_NURSERY; ++gen) {
    struct large_space_header **start = generations[gen].contents_start;
    for (struct large_space_header **allocation = generations[gen].contents_fill;
         allocation != start;
         ++allocation) {
      if ((*allocation)->mark == gc_live_mark) {
        push_into_gen(gen + 1, *allocation);
      } else {
        free(*allocation);
      }
    }
    generations[gen].contents_fill = start;
  }
}

static void collect(gc_ptr live_ptrs[], fixnum live_ptr_ct) {
  this_collection_gen = highest_gen_to_collect();
  if (this_collection_gen) {
    swap_live_mark();
  }
  for (fixnum live_idx = 0; live_idx != live_ptr_ct; ++live_idx) {
    live_ptrs[live_idx] = collect_and_relocate(live_ptrs[live_idx]);
  }
  if (this_collection_gen) {
    sweep();
  }
  nursery_ct = 0;
}

gc_ptr gcalloc(fixnum size,
               gc_recurse_func recurse,
               gc_ptr live_ptrs[],
               fixnum live_ptr_ct) {
  if (!size) {
    fputs("Invalid to gcalloc 0 bytes\n", stderr);
    exit(EXIT_FAILURE);
  }
  if (!fits_in_nursery(size)) {
    if (!gen_could_alloc(FIRST_LARGE_GEN, 1)) {
      collect(live_ptrs, live_ptr_ct);
    }
    return large_space_alloc(size, recurse);
  }
  gc_ptr buf = nursery_alloc(size, recurse);
  if (!buf) {
    collect(live_ptrs, live_ptr_ct);
    gc_ptr post_collect = nursery_alloc(size, recurse);
    if (!post_collect) {
      fputs("No free space in nursery after a collection!\n", stderr);
      exit(EXIT_FAILURE);
    }
    return post_collect;
  }
  return buf;
}
