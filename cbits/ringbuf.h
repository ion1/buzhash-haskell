#ifndef INCLUDE_RINGBUF_H
#define INCLUDE_RINGBUF_H

#include <errno.h>
#include <malloc.h>
#include <memory.h>

typedef struct ringbuf {
  const unsigned int size;
  unsigned int idx;
  uint8_t buf[];
} ringbuf_t;

static inline ringbuf_t *
ringbuf_new (const unsigned int size)
{
  if (size < 1) {
    errno = EINVAL;
    return NULL;
  }

  ringbuf_t *restrict rb =
    calloc (1, sizeof (ringbuf_t) + size * sizeof (uint8_t));
  if (! rb)
    return NULL;

  ringbuf_t rb_init = { size, 0 };

  return memcpy (rb, &rb_init, sizeof (ringbuf_t));
}

static inline void
ringbuf_free (ringbuf_t *rb)
{
  free (rb);
}

static inline void
ringbuf_copy (ringbuf_t *target, const ringbuf_t *source)
{
  (void) memcpy (target, source, sizeof (ringbuf_t) + source->size);
}

static inline uint8_t
ringbuf_put (ringbuf_t *restrict rb, const uint8_t new_val)
{
  const uint8_t old_val = rb->buf[rb->idx];
  rb->buf[rb->idx] = new_val;
  rb->idx = (rb->idx + 1) % rb->size;
  return old_val;
}

#endif  // INCLUDE_RINGBUF_H
