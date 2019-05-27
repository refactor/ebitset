#pragma once 

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <limits.h>

#include "portability.h"

#define TILE_SIZE 256
#define BITILE_BITSIZE (TILE_SIZE * TILE_SIZE)
#define BITILE_BYTESIZE (BITILE_BITSIZE / CHAR_BIT)
#define BITILE_ARRAYSIZE (BITILE_BYTESIZE / sizeof(uint64_t))

// 8KB for a 256x256 bitile
#define BITSET_256x256_ARRSZ 1024
// 32KB for a 512x512 bitile
#define BITSET_512x512_ARRSZ 4096

struct bitset_s {
    int32_t cardinality;
    uint64_t  array[];
};

typedef struct bitset_s bitset_t;

/* Create a new bitset. Return NULL in case of failure. */
bitset_t *bitset_create( void );

/* Create a new bitset able to contain size bits. Return NULL in case of failure. */
bitset_t *bitset_create_with_capacity( int32_t size );

/* Free memory. */
void bitset_free(bitset_t *bitset);

/* Set all bits to zero. */
void bitset_clear(bitset_t *bitset);

/* Create a copy */
bitset_t * bitset_copy(const bitset_t *bitset);

/* Resize the bitset. Return true in case of success, false for failure. Pad with zeroes new buffer areas if requested. */
bool bitset_resize( bitset_t *bitset,  size_t newarraysize, bool padwithzeroes );

/* returns how many bytes of memory the backend buffer uses */
static inline int32_t bitset_size_in_bytes(const bitset_t *bitset) {
  return BITILE_BYTESIZE;
}

/* returns how many bits can be accessed */
static inline int32_t bitset_size_in_bits(const bitset_t *bitset) {
  return BITILE_BITSIZE;
}

/* returns how many words (64-bit) of memory the backend buffer uses */
static inline int32_t bitset_size_in_words(const bitset_t *bitset) {
  return BITILE_ARRAYSIZE;
}

static inline void bitset_set_list(bitset_t *bitset, const uint32_t *list, uint32_t length) {
    uint32_t offset, pos, index;
    uint64_t load, newload;
    const uint32_t *end = list + length;
    while (list != end) {
        pos = *(const uint32_t *)list;
        offset = pos >> 6;
        index = pos % 64;
        if (offset < BITILE_ARRAYSIZE) {
            load = bitset->array[offset];
            newload = load | (UINT64_C(1) << index);
            bitset->array[offset] = newload;
            bitset->cardinality += (load ^ newload) >> index;
        }
        list++;
    }
}

/* Set the ith bit. Attempts to resize the bitset if needed (may silently fail) */
static inline bool bitset_set(bitset_t *bitset,  uint32_t pos ) {
  uint32_t shiftedi = pos >> 6;
  if (shiftedi >= BITILE_ARRAYSIZE) {
      return false;
  }
  uint64_t old_w = bitset->array[shiftedi];
  uint64_t new_w = old_w | (UINT64_C(1) << (pos & 63));
  bitset->cardinality += (old_w ^ new_w) >> (pos & 63);
  bitset->array[shiftedi] = new_w;
  return true;
}

static inline bool bitset_unset(bitset_t *bitset, uint32_t pos) {
    const uint32_t shiftedi = pos >> 6;
    if (shiftedi >= BITILE_ARRAYSIZE) {
      return false;
    }
    uint64_t old_word = bitset->array[shiftedi];
    const int index = pos & 63;
    const uint64_t new_word = old_word & (~(UINT64_C(1) << index));
    bitset->cardinality -= (uint32_t)((old_word ^ new_word) >> index);
    bitset->array[shiftedi] = new_word;
    return true;
}

/* Get the value of the ith bit.  */
static inline bool bitset_get(const bitset_t *bitset,  size_t i ) {
  size_t shiftedi = i >> 6;
  if (shiftedi >= BITILE_ARRAYSIZE) {
    return false;
  }
  return ( bitset->array[shiftedi] & ( ((uint64_t)1) << (i % 64))) != 0 ;
}

static inline int32_t bitset_precount(const bitset_t *bitset) {
    return bitset->cardinality;
}

/* Count number of bit sets.  */
int32_t bitset_count(const bitset_t *bitset);

/* Find the index of the first bit set.  */
int32_t bitset_minimum(const bitset_t *bitset);

/* Find the index of the last bit set.  */
int32_t bitset_maximum(const bitset_t *bitset);


/* compute the union in-place (to b1), returns true if successful, to generate a new bitset first call bitset_copy */
bool bitset_inplace_union(bitset_t * restrict b1, const bitset_t * restrict b2);

/* report the size of the union (without materializing it) */
int32_t bitset_union_count(const bitset_t * restrict b1, const bitset_t * restrict b2);

bool bitset_intersects(const bitset_t *restrict b1, const bitset_t * restrict b2);

/* compute the intersection in-place (to b1), to generate a new bitset first call bitset_copy */
void bitset_inplace_intersection(bitset_t * restrict b1, const bitset_t * restrict b2);

/* report the size of the intersection (without materializing it) */
int32_t bitset_intersection_count(const bitset_t * restrict b1, const bitset_t * restrict b2);


/* compute the difference in-place (to b1), to generate a new bitset first call bitset_copy */
void bitset_inplace_difference(bitset_t * restrict b1, const bitset_t * restrict b2);

/* compute the size of the difference */
int32_t  bitset_difference_count(const bitset_t *restrict b1, const bitset_t * restrict b2) ;

/* compute the symmetric difference in-place (to b1), return true if successful, to generate a new bitset first call bitset_copy */
void bitset_inplace_symmetric_difference(bitset_t * restrict b1, const bitset_t * restrict b2);

/* compute the size of the symmetric difference  */
int32_t  bitset_symmetric_difference_count(const bitset_t *restrict b1, const bitset_t * restrict b2);

/* iterate over the set bits
 like so :
  for(size_t i = 0; nextSetBit(b,&i) ; i++) {
    //.....
  }
  */
static inline bool nextSetBit(const bitset_t *bitset, size_t *i) {
      size_t x = *i >> 6;
      if (x >= BITILE_ARRAYSIZE) {
        return false;
      }
      uint64_t w = bitset->array[x];
      w >>= (*i & 63);
      if (w != 0) {
        *i += __builtin_ctzll(w);
        return true;
      }
      x ++;
      while (x < BITILE_ARRAYSIZE) {
        w = bitset->array[x];
        if (w != 0) {
          *i = x * 64 + __builtin_ctzll(w);
          return true;
        }
        x ++;
      }
      return false;
}

/* iterate over the set bits
 like so :
   size_t buffer[256];
   size_t howmany = 0;
  for(size_t startfrom = 0; (howmany = nextSetBits(b,buffer,256, &startfrom)) > 0 ; startfrom++) {
    //.....
  }
  */
static inline size_t nextSetBits(const bitset_t *bitset, size_t *buffer, size_t capacity, size_t * startfrom) {
      if(capacity == 0) return 0;// sanity check
      size_t x = *startfrom >> 6;
      if (x >= BITILE_ARRAYSIZE) {
          return 0;// nothing more to iterate over
      }
      uint64_t w = bitset->array[x];
      w >>= (*startfrom & 63);
      size_t howmany = 0;
      size_t base = x << 6;
      while(howmany < capacity) {
            while (w != 0) {
              uint64_t t = w & (~w + 1);
              int r = __builtin_ctzll(w);
              buffer[howmany++] = r + base;
              if(howmany == capacity) goto end;
              w ^= t;
            }
            x += 1;
            if(x == BITILE_ARRAYSIZE) {
              break;
            }
            base += 64;
            w = bitset->array[x];
      }
      end:
      if(howmany > 0) {
        *startfrom = buffer[howmany - 1];
      }
      return howmany;
}

typedef bool (*bitset_iterator)(size_t value, void *param);

// return true if uninterrupted
static inline bool bitset_for_each(const bitset_t *b, bitset_iterator iterator, void *ptr) {
  size_t base = 0;
  for (size_t i = 0; i < BITILE_ARRAYSIZE; ++i ) {
    uint64_t w = b->array[i];
    while (w != 0) {
      uint64_t t = w & (~w + 1);
      int r = __builtin_ctzll(w);
      if(!iterator(r + base, ptr)) return false;
      w ^= t;
    }
    base += 64;
  }
  return true;
}

static inline void bitset_print(const bitset_t *b) {
  printf("{");
  for(size_t i = 0; nextSetBit(b,&i) ; i++) {
    printf("%zu, ",i);
  }
  printf("}");
}

