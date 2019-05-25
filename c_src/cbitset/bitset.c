#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

#include "bitset.h"

/* Create a new bitset. Return NULL in case of failure. */
bitset_t *bitset_create() {
    return bitset_create_with_capacity(BITILE_BITSIZE);
}

/* Create a new bitset able to contain size bits. Return NULL in case of failure. */
bitset_t *bitset_create_with_capacity( int32_t size ) {
  bitset_t *bitset = NULL;
  /* Allocate the bitset itself. */
  if( ( bitset = (bitset_t*) calloc(1, sizeof(bitset_t) + BITILE_BYTESIZE ) ) == NULL ) {
      return NULL;
  }
  bitset->_ARRAYSIZE = (size + sizeof(uint64_t) * 8 - 1) / (sizeof(uint64_t) * 8);
  return bitset;
}

/* Create a copy */
bitset_t *bitset_copy( const bitset_t * bitset ) {
  bitset_t *copy = NULL;
  /* Allocate the bitset itself. */
  if( ( copy = (bitset_t*) malloc( sizeof(bitset_t) + BITILE_BYTESIZE) ) == NULL ) {
      return NULL;
  }
  memcpy(copy,bitset,sizeof(bitset_t) + BITILE_BYTESIZE);
  return copy;
}

void bitset_clear(bitset_t *bitset) {
  memset(bitset->array,0,sizeof(uint64_t) * bitset->_ARRAYSIZE);
  bitset->cardinality = 0;
}

/* Free memory. */
void bitset_free(bitset_t *bitset) {
  free(bitset);
}

int32_t bitset_count(const bitset_t *bitset) {
    int32_t card = 0;
    int32_t k = 0;
    // assumes that long long is 8 bytes
    for(; k + 7 < BITILE_ARRAYSIZE; k+=8) {
        card += __builtin_popcountll(bitset->array[k]);
        card += __builtin_popcountll(bitset->array[k+1]);
        card += __builtin_popcountll(bitset->array[k+2]);
        card += __builtin_popcountll(bitset->array[k+3]);
        card += __builtin_popcountll(bitset->array[k+4]);
        card += __builtin_popcountll(bitset->array[k+5]);
        card += __builtin_popcountll(bitset->array[k+6]);
        card += __builtin_popcountll(bitset->array[k+7]);
    }
    for(; k + 3 < BITILE_ARRAYSIZE; k+=4) {
        card += __builtin_popcountll(bitset->array[k]);
        card += __builtin_popcountll(bitset->array[k+1]);
        card += __builtin_popcountll(bitset->array[k+2]);
        card += __builtin_popcountll(bitset->array[k+3]);
    }
    for(; k < BITILE_ARRAYSIZE; k++) {
        card += __builtin_popcountll(bitset->array[k]);
    }
    if (card != bitset->cardinality) {
        printf("card: %d, cardinality: %d\r\n", card, bitset->cardinality);
        //exit(EXIT_FAILURE);
    }
    return card;
}


bool bitset_inplace_union(bitset_t * restrict b1, const bitset_t * restrict b2) {
  int32_t card = 0;
  for(int32_t k = 0 ; k < BITILE_ARRAYSIZE; ++k) {
    b1->array[k] |= b2->array[k];
    card += __builtin_popcountll(b1->array[k]);
  }
  b1->cardinality = card;
  return true;
}

int32_t bitset_minimum(const bitset_t *bitset) {
  for(int32_t k = 0; k < bitset->_ARRAYSIZE; k++) {
    uint64_t w = bitset->array[k];
    if ( w != 0 ) {
      return __builtin_ctzll(w) + k * 64;
    }
  }
  return 0;
}

int32_t bitset_maximum(const bitset_t *bitset) {
  for(int32_t k = bitset->_ARRAYSIZE ; k > 0 ; k--) {
    uint64_t w = bitset->array[k - 1];
    if ( w != 0 ) {
      return  63 - __builtin_clzll(w) + (k - 1) * 64;
    }
  }
  return 0;
}

bool bitset_intersects(const bitset_t *restrict b1, const bitset_t * restrict b2) {
  if (b1->cardinality + b2->cardinality > BITILE_BITSIZE)
      return true;

  for( int32_t k = 0; k < BITILE_ARRAYSIZE; ++k) {
    if (__builtin_popcountll ( b1->array[k] & b2->array[k]) > 0)
        return true;
  }
  return false;
}

int32_t bitset_union_count(const bitset_t *restrict b1, const bitset_t * restrict b2) {
  int32_t answer = 0;
  int32_t k = 0;
  for( ; k + 3 < BITILE_ARRAYSIZE; k += 4) {
    answer += __builtin_popcountll ( b1->array[k] | b2->array[k]);
    answer += __builtin_popcountll ( b1->array[k+1] | b2->array[k+1]);
    answer += __builtin_popcountll ( b1->array[k+2] | b2->array[k+2]);
    answer += __builtin_popcountll ( b1->array[k+3] | b2->array[k+3]);
  }
  return answer;
}

void bitset_inplace_intersection(bitset_t * restrict b1, const bitset_t * restrict b2) {
  int32_t card = 0;
  for(int32_t k = 0 ; k < BITILE_ARRAYSIZE; ++k) {
    b1->array[k] &= b2->array[k];
    card += __builtin_popcountll(b1->array[k]);
  }
  b1->cardinality = card;
}

int32_t bitset_intersection_count(const bitset_t * restrict b1, const bitset_t * restrict b2) {
  int32_t answer = 0;
  for(int32_t k = 0 ; k < BITILE_ARRAYSIZE; ++k) {
    answer += __builtin_popcountll ( b1->array[k] & b2->array[k]);
  }
  return answer;
}

void bitset_inplace_difference(bitset_t *restrict b1, const bitset_t * restrict b2) {
  int32_t card = 0;
  for( int32_t k = 0; k < BITILE_ARRAYSIZE; ++k) {
    b1->array[k] &= ~ (b2->array[k]);
    card += __builtin_popcountll(b1->array[k]);
  }
  b1->cardinality = card;
}


int32_t  bitset_difference_count(const bitset_t *restrict b1, const bitset_t * restrict b2) {
  int32_t answer = 0;
  for(int32_t k = 0; k < BITILE_ARRAYSIZE; ++k) {
    answer += __builtin_popcountll (b1->array[k] & ~ (b2->array[k]));
  }
  return answer;
}

bool bitset_inplace_symmetric_difference(bitset_t *restrict b1, const bitset_t * restrict b2) {
  int32_t card = 0;
  for( int32_t k = 0; k < BITILE_ARRAYSIZE; ++k) {
    b1->array[k] ^= b2->array[k];
    card += __builtin_popcountll(b1->array[k]);
  }
  b1->cardinality = card;
  return true;
}

int32_t  bitset_symmetric_difference_count(const bitset_t *restrict b1, const bitset_t * restrict b2) {
  int32_t k = 0;
  int32_t answer = 0;
  for( ; k < BITILE_ARRAYSIZE; ++k) {
    answer += __builtin_popcountll(b1->array[k] ^ b2->array[k]);
  }
  if(b2->_ARRAYSIZE > b1->_ARRAYSIZE) {
    for( ; k < b2->_ARRAYSIZE; ++k) {
      answer += __builtin_popcountll(b2->array[k]);
    }
  } else {
    for( ; k < b1->_ARRAYSIZE; ++k) {
      answer += __builtin_popcountll(b1->array[k]);
    }
  }
  return answer;
}

