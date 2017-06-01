#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "heaplib.h"

#define HEAP_SIZE 1024
#define ARRAY_LEN 16

// TODO: Add test descriptions as you add more tests...
const char* testDescriptions[] = {
    "init: heap should be created when enough space",
    "init: heap should not be created when not enough space",
    "alloc: block should be allocated when enough space",
    "alloc: block should not be allocated when not enough space",
    "alloc: block returned is aligned",
    "init: should support multiple heapptrs",
    "resize: should be 8-byte aligned; if blockptr is 0, should behave like alloc, else preserve contents of the block",
    "release: should do nothing if blockptr is NULL; released memory should be allocated again by calls to hl_alloc",
    "resize: block should be 8byte aligned even tho heapptr is not, resize with 0 ptr should work as alloc",
    "alloc: robustness test for alloc",
    "resize: robustness test for reslease",
    "release: robustness test for resize",
    "hello, robustness for alloc and release"
};

/* Checks whether a "heap" is created when there IS enough space.
 * THIS TEST IS COMPLETE.
 */
int test01() {

    char heap[HEAP_SIZE];

    int heap_created_f = hl_init(heap, HEAP_SIZE);

    if (heap_created_f) {
        return SUCCESS;
    }
    return FAILURE;
}

/* Checks whether a "heap" is created when there IS NOT enough space.
 * THIS TEST IS NOT COMPLETE.
 * Lab 12 TODO: COMPLETE THIS TEST!
 */
int test02() {

    char heap[HEAP_SIZE];

    int heap_created_f = hl_init(heap, 0);

    if (heap_created_f == 0){
      return SUCCESS;
    }
    return FAILURE;
}

/* Checks whether a block can be allocated when there is enough space.
 * THIS TEST IS NOT COMPLETE.
 * Lab 12 TODO: COMPLETE THIS TEST!
 */
int test03() {

    char heap[HEAP_SIZE];

    hl_init(heap, HEAP_SIZE);

    //shud work
    void *block = hl_alloc(heap, ARRAY_LEN * 17);

    if (block != NULL) {
      return SUCCESS;
    }
    return FAILURE;
}

/* Checks whether a block can be allocated when there is NOT enough space.
 * THIS TEST IS COMPLETE.
 */
int test04() {

    char heap[HEAP_SIZE];

    hl_init(heap, HEAP_SIZE);

    // should NOT work
    void *block = hl_alloc(heap, ARRAY_LEN * HEAP_SIZE);

    if (block == NULL) {
        return SUCCESS;
    }
    return FAILURE;
}

/* Checks whether hl_alloc returns a pointer that has the correct
 * alignment.
 * THIS TEST IS NOT COMPLETE.
 * LAB 12 TODO: COMPLETE THIS TEST! (it is not robust)
 */
int test05() {

    char array[HEAP_SIZE];
    void *block;
    bool aligned_f = false;

    hl_init(&array, HEAP_SIZE - 1);

    block = hl_alloc(&array, ARRAY_LEN); // doesn't really matter how much we allocate here
    void *block2 = hl_alloc(&array, ARRAY_LEN * 2 - 1);
    void *block3 = hl_alloc(&array, ARRAY_LEN * 3 + 1);



    // you may find this helpful. feel free to remove
#ifdef PRINT_DEBUG
    printf("blockptr = %16lx\n", (unsigned long)block);
    printf("mask =     %16lx\n", (unsigned long)(ALIGNMENT -1));
    printf("---------------------------\n");
    printf("ANDED =    %16lx\n", (unsigned long)block & (ALIGNMENT - 1));
    printf("!ANDED (ALIGNED) =   %6d\n", !((unsigned long)block & (ALIGNMENT - 1)));
#endif

    aligned_f = !((unsigned long)block & (ALIGNMENT - 1));
    bool aligned_f2 = !((unsigned long)block2 & (ALIGNMENT - 1));
    bool aligned_f3 = !((unsigned long)block3 & (ALIGNMENT - 1));

    if (aligned_f && aligned_f2 && aligned_f3) {
        return SUCCESS;
    }

    return FAILURE;
}

/* Your test.
 * Checks if any global arrays, variables, or structures were used by checking
 * whether the implementation supports multiple heapptrs. Code should naturally
 * support multiple heaps.
 */
int test06() {
    // char heap[HEAP_SIZE];
    // char* test_heap = (char *)((unsigned long)heap + HEAP_SIZE+ 512);

    // hl_init(heap, HEAP_SIZE);
    // hl_init(test_heap, HEAP_SIZE);
    // void* test_block = hl_alloc(test_heap,256);
    // if(test_block == NULL){ //why does this suppose to return NULL
    //     return FAILURE;
    // }
    // return SUCCESS;
    char heap[HEAP_SIZE];
    hl_init(heap, HEAP_SIZE);
    void* test_block2, *big_test_block;
    int x =6;
// for (int i = 0; i < HEAP_SIZE; i++) {
    int i = HEAP_SIZE - (16 + 8 + 8);
        test_block2 = hl_alloc(heap,i);
        if (test_block2 == NULL) {
            return FAILURE;
        }
        if (test_block2 != NULL){
            char *test_array1 = test_block2;

            for(int j = 0; j<i; j++){
                test_array1[j]=x;
            }

            big_test_block = hl_resize(heap, test_block2, 3*i);
            bool aligned_f2 = !((unsigned long)test_block2 & (ALIGNMENT - 1));
            if (!aligned_f2){ //tests if it is 8-byte aligned
                return FAILURE;
            }

            if (big_test_block != NULL) {
                // the location of the array may need to change
                test_array1 = big_test_block;
                // check if the content is preserved
                for (int j = 0; j < i; j++) {
                    if (test_array1[j] != x) {
                        return FAILURE;
                    }
                }
                hl_release(heap, big_test_block);
            } else {
                hl_release(heap, test_block2);
            } // If blockptr has the value 0 (NULL), the function should do nothing
        }

// }
 return SUCCESS;
}

/* Your test.
 * Checks whether hl_resize preserved the contents of the block when block is
 * resized to a smaller size, original size, and bigger size.
 * Checks if blockptr is 0, then hl_resize behaves like hl_alloc.
 */
int test07() {
    char heap[HEAP_SIZE];
    //void* test_heap =  (void *)((unsigned long)heap + 8);
    hl_init(heap,HEAP_SIZE);
    //hl_init(test_heap, HEAP_SIZE); // this part is fking us over I think; we are initing it wrong
    void *test_block, *small_test_block;
    char x = 7; // some value to write

    for (int i = 0; i < HEAP_SIZE; i++) {
        test_block = hl_alloc(heap,i);
        if (test_block != NULL) {
            char *test_array = test_block;

            for (int j = 0; j < i; j++) {
                test_array[j] = x;
            }

            // resize to i/2. This should always be possible
            small_test_block = hl_resize(heap, test_block, i/2);
            bool aligned_f1 = !((unsigned long)test_block & (ALIGNMENT - 1));
            if (!aligned_f1){
                return FAILURE;
            }

            // might fail because of requirement for meta-data size
            if (small_test_block != NULL) {
                test_array = small_test_block;
                for (int j = 0; j < i/2; j++) {
                    if (test_array[j] != x) {
                        return FAILURE;
                    }
                }
                hl_release(heap, small_test_block);
            } else {
                hl_release(heap,test_block);
            }
        }


    }

    return SUCCESS;

}

/* Your test.
 * Checks whether hl_alloc returns 0 (NULL) if the allocator cannot satisfy the
 * request.
 * Checks if hl_release frees the block of memory pointed to by blockptr.
 * If blockptr has the value 0 (NULL), the function should do nothing.
 */
int test08() {

    char heap[HEAP_SIZE];
    hl_init(heap, HEAP_SIZE);
    void *test_block1, *test_block2;

    test_block1 = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block1 == NULL) return FAILURE;
    test_block2 = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block2 != NULL) return FAILURE;
    //there is meta data so it will not fit

    hl_release(heap, test_block1); //release test block 1 to create space
    test_block2 = hl_alloc(heap, HEAP_SIZE/2);

    if (test_block2 == NULL) return FAILURE;

    char heap2[HEAP_SIZE];
    hl_init(heap2, HEAP_SIZE);
    hl_alloc(heap2, HEAP_SIZE/2);

    // blockptr has the value 0
    hl_release(heap2, 0);

    return SUCCESS;

}

/* Your test.
 * for hl_release:
 * 1) If blockptr has the value 0 (NULL), the function should do nothing
 * 2) Released memory should be able to be allocated again by subsequent calls
 *    to hl_alloc.
 */
int test09() {
    char heap[HEAP_SIZE];
    void* test_heap = (void *)((unsigned long)heap + 8);
    void *test_block1, *test_block2, *test_block3, *test_block4, *test_block5;

    hl_init(heap, HEAP_SIZE);

    test_block1 = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block1 != NULL){
        hl_release(heap, 0); //should do nothing

        test_block2 = hl_alloc(heap, HEAP_SIZE/2);
        if (test_block2 != NULL){ //should be NULL
            return FAILURE;
        }
    } else {
        return FAILURE;
    }

    hl_init(test_heap, HEAP_SIZE);

    test_block3 = hl_alloc(test_heap, HEAP_SIZE/2);
    if (test_block3 == NULL) {
        return FAILURE;
    }
    test_block4 = hl_alloc(test_heap, HEAP_SIZE/4);
    if (test_block4 == NULL) {
        return FAILURE;
    }
    hl_release(test_heap, test_block4);
    test_block5 = hl_alloc(test_heap, HEAP_SIZE/4);
    //should not work if the test_block4 was not released
    if (test_block5 == NULL){
        return FAILURE;
    }

    return SUCCESS;

}

/* Your test.
 * 1) Having a heap pointer that is NOT 8-byte aligned but allocation is still
 *    8-byte aligned.
 * 2) Resize with 0 pointer in a brand new heap and check if it returns a
 *    non-null pointer
 * 3) For hl_resize, when blockptr is 0 (should behave like hl_alloc) and the
 *    heap_size is too big
 */
int test10() {
    char heap[HEAP_SIZE];
    void* test_heap = (void *)((unsigned long)heap + 7);
    void *test_block, *test_block1;

    hl_init(heap, HEAP_SIZE);

    test_block = hl_resize(heap, 0, 2*HEAP_SIZE); //it is too big
    if( test_block != NULL){
        return FAILURE;
    }

    test_block = hl_resize(heap, 0, HEAP_SIZE/4);
    if (test_block == NULL){
        return FAILURE;
    }

    hl_init(test_heap, HEAP_SIZE); //heap is not 8-byte aligned
    //allocation should still be 8-byte aligned
    test_block1 = hl_alloc(test_heap, HEAP_SIZE/4);
    bool aligned_f1 = !((unsigned long)test_block1 & (ALIGNMENT - 1));
    if (!aligned_f1) {
        return FAILURE;
    }

    return SUCCESS;

}

/* Your test.
 * Robustness test for alloc WRITE THE CODE !!! HELLOOOO
 */
int test11() {
    char heap[HEAP_SIZE];
    void* test_heap = (void *)((unsigned long)heap + 7);
    void *test_block, *test_block1, *test_block2, *test_block3;

    hl_init(heap, HEAP_SIZE);
    test_block = hl_alloc(heap, HEAP_SIZE/4);
    if (test_block == NULL){
        return FAILURE;
    }

    test_block1 = hl_alloc(heap, HEAP_SIZE/4);
    if (test_block1 == NULL){
        return FAILURE;
    }

    test_block2 = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block2 != NULL) {
        return FAILURE;
    }

    test_block3 = hl_alloc(heap, 0);
    if (test_block3 == NULL) {
        return FAILURE;
    }

    hl_init(test_heap, HEAP_SIZE);
    test_block = hl_alloc(test_heap, HEAP_SIZE/4);
    if (test_block == NULL){
        return FAILURE;
    }

    test_block1 = hl_alloc(test_heap, HEAP_SIZE/4);
    if (test_block1 == NULL){
        return FAILURE;
    }

    test_block2 = hl_alloc(test_heap, HEAP_SIZE/2);
    if (test_block2 != NULL) {
        return FAILURE;
    }

    test_block3 = hl_alloc(test_heap, 0);
    if (test_block3 == NULL) {
        return FAILURE;
    }

    return SUCCESS;

}

/* Your test.
 * robustness test for hl_resize()
 */
int test12() {
    char heap[HEAP_SIZE];
    void *test_block, *test_block1;

    hl_init(heap, HEAP_SIZE);
    test_block = hl_alloc(heap, HEAP_SIZE/4);
    if (test_block == NULL){
        return FAILURE;
    }

    test_block = hl_resize(heap, test_block, HEAP_SIZE/5);
    if (test_block == NULL) {
        return FAILURE;
    }

    test_block = hl_resize(heap, test_block, 0);
    if (test_block == NULL) {
        return FAILURE;
    }

    test_block1 = hl_alloc(heap, HEAP_SIZE/3);
    if (test_block1 == NULL) {
        return FAILURE;
    }

    test_block1 = hl_resize(heap, test_block1, 1);
    if (test_block1 == NULL) {
        return FAILURE;
    }

    // test_block1 = hl_resize(heap, test_block1, HEAP_SIZE/2);
    // if (test_block1 == NULL) {
    //     return FAILURE;
    // }

    test_block1 = hl_resize(heap, 0, HEAP_SIZE+385); //it is too big
    if( test_block1 != NULL){
        return FAILURE;
    }

    hl_release(heap,test_block1);
    test_block1 = hl_resize(heap, 0, HEAP_SIZE/4);
    if(test_block1 == NULL){
        return FAILURE;
    }

    return SUCCESS;

}

/* Your test.
 * robustness test for hl_release
 */
int test13() {

    char heap[HEAP_SIZE];
    hl_init(heap, HEAP_SIZE);

    void* test_block;

    int i = 0;
    test_block = hl_alloc(heap, HEAP_SIZE/4);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;

    hl_release(heap, test_block);
    printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/6);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
   hl_release(heap, test_block);
   printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

     test_block = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

     test_block = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

     test_block = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

     test_block = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

     test_block = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);


    test_block = hl_alloc(heap, HEAP_SIZE/3);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/5);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/16);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/10);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

        test_block = hl_alloc(heap, HEAP_SIZE/10);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/4);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;

    hl_release(heap, test_block);
    printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/6);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
   hl_release(heap, test_block);
   printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/2);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
    printf("%d\n", i);

     test_block = hl_alloc(heap, HEAP_SIZE/2);
     test_block = hl_resize(heap, test_block, HEAP_SIZE/2 + 124);
    if (test_block == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block);
        printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  test_block = hl_resize(heap, test_block, HEAP_SIZE/3);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  test_block = hl_resize(heap, test_block, HEAP_SIZE/4);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/4);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/124);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/2);
   //    test_block = hl_resize(heap, test_block, HEAP_SIZE/1235);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/3);
   //    test_block = hl_resize(heap, test_block, HEAP_SIZE/2+1);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/5);
   //    test_block = hl_resize(heap, test_block, HEAP_SIZE/3);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/16);
   //    test_block = hl_resize(heap, test_block, HEAP_SIZE/2+4);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/10);
   //    test_block = hl_resize(heap, test_block, HEAP_SIZE/19);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //      test_block = hl_alloc(heap, HEAP_SIZE/10);
   //        test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/4);
   //    test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;

   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/6);
   //    test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   // hl_release(heap, test_block);
   // printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //      test_block = hl_resize(heap, test_block, HEAP_SIZE/2 + 41);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //      test_block = hl_resize(heap, test_block, HEAP_SIZE/2+256);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //      test_block = hl_resize(heap, test_block, HEAP_SIZE/2+214);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //      test_block = hl_resize(heap, test_block, HEAP_SIZE/2+200);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //      test_block = hl_resize(heap, test_block, HEAP_SIZE/2+218);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/2);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/2+230);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/3);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/5);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_resize(heap, test_block,HEAP_SIZE/16);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/10);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //      test_block = hl_alloc(heap, HEAP_SIZE/10);
   //         test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/4);
   //      test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;

   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/6);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   // hl_release(heap, test_block);
   // printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/2);
   //     test_block = hl_resize(heap, test_block, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //   test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);
   //  printf("%d\n", i);

   //  test_block = hl_alloc(heap, HEAP_SIZE/2);
   //  if (test_block == NULL) {
   //      return FAILURE;
   //  }
   //  i++;
   //  hl_release(heap, test_block);

   //  printf("%d\n", i);

    return SUCCESS;


}

int test14(){

    char heap[HEAP_SIZE];
    void *test_block, *test_block1, *test_block2;
    int i = 0;

    hl_init(heap, HEAP_SIZE);
    test_block = hl_alloc(heap, HEAP_SIZE/4);
    if (test_block == NULL){
        return FAILURE;
    }
    i++;

    hl_release(heap, test_block);
    printf("%d\n", i);

    test_block = hl_alloc(heap, HEAP_SIZE/4); // no block in use when entering
    if (test_block == NULL){
        return FAILURE;
    }

    test_block = hl_resize(heap, test_block, HEAP_SIZE/6); // one block in use, 2 blocks in total when exiting
    if (test_block == NULL){
        return FAILURE;
    }

    // i++;

    // hl_release(heap, test_block); // no block in use, 2 blocks in total when exiting
    // printf("%d\n", i);

    test_block = hl_resize(heap, test_block, HEAP_SIZE/6); // 1 block in use, 2 blocks in total when exiting
    if (test_block == NULL){
        return FAILURE;
    }

    test_block1 = hl_alloc(heap, HEAP_SIZE/4); // 2 blocks in use, 3 blocks in total when exiting
    if (test_block1 == NULL){
        return FAILURE;
    }

    test_block1 = hl_resize(heap, test_block1, HEAP_SIZE/6*2); // 2 blocks in use, 4 blocks in total when exiting
    if (test_block1 == NULL){
        return FAILURE;
    }

    test_block2 = hl_alloc(heap, HEAP_SIZE/2); // NULL pointer
    if (test_block2 != NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block2);
    printf("%d\n", i);

    test_block2 = hl_alloc(heap, HEAP_SIZE/10); // get something
    if (test_block2 == NULL) {
        return FAILURE;
    }
    i++;
    hl_release(heap, test_block2);
    printf("%d\n", i);

    return SUCCESS;
}
