#ifndef INCLUDE_BUZHASH_H
#define INCLUDE_BUZHASH_H

#include "ringbuf.h"

typedef void (*split_ptr) (const unsigned int idx);

typedef struct buzhash {
  const unsigned int window_size;
  const unsigned int chunk_min_size;
  const unsigned int chunk_max_size;
  const unsigned int mask_bits;
  const uint32_t mask;

  ringbuf_t *restrict ringbuf;
  uint32_t hash;
  unsigned int chunk_total_size;
} buzhash_t;

buzhash_t *
buzhash_new (const unsigned int window_size_div_32,
             const unsigned int chunk_min_size,
             const unsigned int chunk_max_size,
             const unsigned int mask_bits);

buzhash_t *
buzhash_clone (const buzhash_t *restrict bh_orig);

void
buzhash_free (buzhash_t *restrict bh);

void
buzhash_process (buzhash_t *restrict bh,
                 const uint8_t *restrict buf,
                 const unsigned int buf_size,
                 split_ptr split);

extern const uint32_t buztable[256];

#endif // INCLUDE_BUZHASH_H
