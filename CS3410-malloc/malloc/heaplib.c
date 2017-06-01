#include <stdlib.h>
#include <stdio.h>
#include "heaplib.h"
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

/* You must implement these functions according to the specification
 * given in heaplib.h. You can define any types you like to this file.
 *
 * Student 1 Name: Yiwen Huang
 * Student 1 NetID: yh385
 * Student 2 Name: Mary (Yanying) Ji
 * Student 2 NetID: yj256
 *
 * Include a description of your approach here.
 * At first, we wanted to do implicit list, first fit with immediate coalesce
 * with next/previous blocks when freed, however, it did not work out as well
 * as if we just did implicit and first fit, so below is just implicit list and
 * first fit.
 */

/* returns a 8-byte aligned size */
#define MAKE_ALIGNED(size) (((size) + (ALIGNMENT - 1)) & ~(ALIGNMENT - 1))

#define ALIGN(ptr) ((char*)((((unsigned long)ptr >> 3) + 1) << 3))
#define NOT_ALIGNED(block) ((unsigned long)block & (ALIGNMENT - 1)) //1 if not aligned
/* Useful shorthand: casts a pointer to a (char *) before adding */
#define ADD_BYTES(base_addr, num_bytes) (((char *)(base_addr)) + (num_bytes))
#define HEAP_HEADER_SIZE (MAKE_ALIGNED(sizeof(heap_header_t)))
#define BLOCK_HEADER_SIZE (MAKE_ALIGNED(sizeof(block)))

#define IN_USE(block) (block->size & 1) //get integer 1 or 0 back

#define ACTUAL_SIZE(block) ((block->size) & -2)

typedef struct _heap_header_t {
  unsigned int max_size;
  unsigned int curr_size;
  unsigned int num_blocks;
} heap_header_t ;

typedef struct block {
  unsigned int size; //LSB is the "is use" bit
} block;

int hl_init(void *heap_ptr, unsigned int heap_size) {

    if (heap_ptr == NULL) return FAILURE;
    //the requested size is smaller than the size needed for all the meta data
    if (heap_size < HEAP_HEADER_SIZE) return FAILURE;

    //char* array = (char*)heap_ptr; //cast it to a char array, 8  bit
    char* arr = (char*)heap_ptr;
    for (int i = 0; i < heap_size; i++) arr[i] = 0;


    heap_header_t* heap = (heap_header_t *)heap_ptr;
    heap->max_size = heap_size;
    heap->curr_size = HEAP_HEADER_SIZE;
    heap->num_blocks = 0;

    return 1; //SUCCESS!
}


void *hl_alloc(void *heap_ptr, unsigned int payload_size) {

    if (heap_ptr == NULL) return FAILURE;


    heap_header_t* heap = (heap_header_t*)heap_ptr; //cast it to a heap
    unsigned int block_size = MAKE_ALIGNED(payload_size + BLOCK_HEADER_SIZE);

    if (heap->max_size < heap->curr_size + block_size) return FAILURE;


    // unsigned int p = heap_ptr;
    // unsigned int end = heap_ptr + max_size;
    // for ((p < end) && ((*p & 1) || (*p <= len))){ //doing first fit
    //   p = p + (*p & -2);
    // }

    block* b = (block*)ADD_BYTES(heap, HEAP_HEADER_SIZE); //incrementer block
    for (int i = 0; i < heap->num_blocks; i++) {
      if (i != 0) b = (block*)(ADD_BYTES(b, ACTUAL_SIZE(b))); //get the next block

      if ((ACTUAL_SIZE(b) >= block_size) && !(IN_USE(b))) {
        b->size = b->size | 1; //sets LSB to 1
        heap->curr_size = heap->curr_size + ACTUAL_SIZE(b);

        char* temp = ADD_BYTES(b, BLOCK_HEADER_SIZE);
        if(NOT_ALIGNED(temp)) return ALIGN(temp); //should already be aligned
        return temp; //found an already allocated block to return
      }
    }

    char *huge_size = ADD_BYTES(b, b->size + block_size);
    char *maximum = ADD_BYTES(heap, heap->max_size);
    if (huge_size > maximum) return FAILURE;
    block* new_block = (block*)ADD_BYTES(b, ACTUAL_SIZE(b));
    new_block->size = (block_size & -2) |  1; //sets LSB to 1

    heap->num_blocks = heap->num_blocks + 1;
    heap->curr_size = heap->curr_size + block_size;

    char* temp = ADD_BYTES(b, BLOCK_HEADER_SIZE);
    if(NOT_ALIGNED(temp)) return ALIGN(temp);
    return temp;
}


void hl_release(void *heap_ptr, void *payload_ptr) {
  if (heap_ptr == NULL) return;
  //if (heap_ptr == NULL) return FAILURE;
  // do nothing if payload_ptr == 0
  if (payload_ptr == 0) return;

  // double check of casting was done right
  heap_header_t *heap = (heap_header_t*)heap_ptr;
  block *block_hd = (block *)(ADD_BYTES(payload_ptr, -BLOCK_HEADER_SIZE));
  block_hd->size = ((block_hd->size) & -2);
  heap->curr_size -= ACTUAL_SIZE(block_hd);

}

// resizing the last block -> make the block bigger -> will it be bigger than heap? yes: failure
void *hl_resize(void *heap_ptr, void *payload_ptr, unsigned int new_size) {

  if (!heap_ptr) return FAILURE;
  // if payload_ptr == 0, behave like hl_alloc()
  if (payload_ptr == 0) return hl_alloc(heap_ptr, new_size);

  heap_header_t* heap = (heap_header_t*)heap_ptr;
  block* block_hd = (block*)ADD_BYTES(payload_ptr, -BLOCK_HEADER_SIZE);
  unsigned new_block_size = MAKE_ALIGNED(new_size+BLOCK_HEADER_SIZE);

  if (new_block_size <= ACTUAL_SIZE(block_hd)) {
    if (NOT_ALIGNED(payload_ptr)) payload_ptr = ALIGN(payload_ptr);
    return payload_ptr;
  }

  if (heap->curr_size + new_block_size > heap->max_size) return FAILURE;

  block* b = (block*)ADD_BYTES(heap, HEAP_HEADER_SIZE); //gets the first block
  while((IN_USE(b) || (ACTUAL_SIZE(b) < new_block_size)) &&
    ((ACTUAL_SIZE(b) != 0)) &&
    (ADD_BYTES(b,ACTUAL_SIZE(b)) < ADD_BYTES(heap, heap->max_size))) {
    b = (block*)ADD_BYTES(b, ACTUAL_SIZE(b));
  }

  if (ACTUAL_SIZE(b) == 0) {
    // we have reached the end of all the blocks
    char* maximum = ADD_BYTES(heap, heap->max_size);
    char* sizez = ADD_BYTES(b, new_block_size);
    if (maximum < sizez) return FAILURE;
    b->size = new_block_size;
  }

  block_hd->size = (block_hd->size) & -2;
  b->size = (b->size) | 1;
  heap->curr_size = heap->curr_size - ACTUAL_SIZE(block_hd) + ACTUAL_SIZE(b);

  char* new_payload = (char*)ADD_BYTES(b, BLOCK_HEADER_SIZE);
  if (NOT_ALIGNED(new_payload)) new_payload = ALIGN(new_payload);
  memcpy(new_payload, payload_ptr, new_size);

  return new_payload;
}

