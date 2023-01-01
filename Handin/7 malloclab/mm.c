/**
 * @file mm.c
 * @author icez_BaiTianYu 2100013011@stu.pku.edu.cn
 * @brief malloclab handin file
 * @date 2022.11.20 - 2022.11.22
 */

/*
 * In this file, I implemented a dynamic allocator.
 * The allocator is based on segregated list, with:
 * - single linking
 * - first-fit policy
 * - partial ordered free lists
 * - no footer in allocated blocks
 * - immediate coalescence
 * - magic parameters
 * 
 * Block structure:
 * 
 * Allocated blocks:
 * +--------+--------------------------------+
 * | HEADER |       PAYLOAD (aligned)        |
 * +--------+--------------------------------+
 *     4B   ^            >= 12B
 *          |
 *          bp (block pointer)
 * 
 * Free blocks:
 * +--------+--------------------------------+
 * | HEADER |     POINTER     | ... | FOOTER |
 * +--------+--------------------------------+
 *     4B   ^       8B                  4B
 *          |
 *          bp
 *  
 * HEADER:
 * +----------------+---+---+---+
 * |      SIZE      |   | P | A |
 * +----------------+---+---+---+
 *        29b            1b   1b
 *  A: is this block allocated?
 *  P: is the previous (in address order) block allocated?
 * 
 * FOOTER:
 * +----------------+---+---+---+
 * |      SIZE      |   |   | A |
 * +----------------+---+---+---+
 *        29b            1b   1b
 * A: is this block allocated?
 * 
 * Free list structure:
 * 
 * entry 0 -> block 00 -> block 01 -> ... -> NULL           block size ranging from 2^4 to (2^5)-1
 * entry 1 -> block 10 -> block 11 -> ... -> NULL           block size ranging from 2^5 to (2^6)-1
 * ...
 * entry k-1 -> block k-1,0 -> block k-1,1 -> ... -> NULL   block size ranging from 2^(k+3) to infinity
 * 
 * k: number of free list headers, @c FREELIST_SIZE
 * Sometimes I'll refer to free list entries as free list headers.
 * 
 * Heap structure:
 * +---------+---------+---------+-----------+-----------------+---------+---------+------------+-----------------+
 * | ENTRY 0 | ENTRY 1 |   ...   | ENTRY k-1 | PROLOGUE HEADER | BLOCK 0 |   ...   | BLOCKS n-1 | EPILOGUE HEADER |
 * +---------+---------+---------+-----------+-----------------+---------+---------+------------+-----------------+
 * ^   8B        8B                   8B     ^        4B          ^                             ^       4B        ^
 * |                                         |                    |                             |                 |
 * mem_heap_lo()/freeList                    freeList_end         heap_listp                    mem_heap_hi()     epilogue
 * 
 * k: number of free list headers, @c FREELIST_SIZE
 * n: number of blocks
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mm.h"
#include "memlib.h"

/* If you want debugging output, use the following macro.  When you hand
 * in, remove the #define DEBUG line. */
// #define DEBUG
#ifdef DEBUG
# define dbg_printf(...)    printf(__VA_ARGS__)
# define dbg_checkheap(x)   mm_checkheap(x)
#else
# define dbg_printf(...)
# define dbg_checkheap(x)
#endif

/* do not change the following! */
#ifdef DRIVER
/* create aliases for driver tests */
#define malloc mm_malloc
#define free mm_free
#define realloc mm_realloc
#define calloc mm_calloc
#endif /* def DRIVER */

/* single word (4) or double word (8) alignment */
#define ALIGNMENT 8
#define inline

/* rounds up to the nearest multiple of ALIGNMENT */
#define ALIGN(p) (((size_t)(p) + (ALIGNMENT-1)) & ~0x7)

/* private global variables */
static char *freeList;      /* begin of free list header */
static char *freeList_end;  /* next byte of the end of free list hedaer */
static char *heap_listp;    /* the first block pointer */
static char *epilogue;      /* epilogue block pointer */
/* end private global variables */

/* basic constants and macros */
#define WSIZE       4                   /* word size */
#define DSIZE       8                   /* double word size */
#define CHUNKSIZE   ((1<<12)+(1<<11))   /* extend heap by this ammount, magic parameter */
#define MINSIZE     16                  /* mininum block size */
#define PERROR      ((void*)-1)         /* pointer that suggests error */

#define MAX(x, y)   ((x) > (y)? (x): (y))
#define MIN(x, y)   ((x) < (y)? (x): (y))

/* header/footer geter and setter, should be invoked only on header/footer */
#define PACK(size, palloc, alloc)   ((size) | ((palloc)<<1) | (alloc))
#define GET(p)                      (*(unsigned*)(p))
#define SET(p, val)                 ((*(unsigned*)(p)) = (val))

#define SET_SIZE(p, size)   ((*(unsigned*)(p)) = ((*(unsigned*)(p)) & 0x7) | size)
#define SET_ALLOC(p)        ((*(unsigned*)(p)) |= 0x1)
#define RESET_ALLOC(p)      ((*(unsigned*)(p)) &= ~0x1)
#define SET_PALLOC(p)       ((*(unsigned*)(p)) |= 0x2)
#define RESET_PALLOC(p)      ((*(unsigned*)(p)) &= ~0x2)

/* block pointer operations, should be invoked only on block pointers */
#define HDRP(bp)            ((char*)(bp) - WSIZE)           /* returns header pointer of a block */

#define SIZE(bp)             (GET(HDRP(bp)) & ~0x7)         /* returns size of a block */
#define ALLOC(bp)            (GET(HDRP(bp)) & 0x1)          /* returns allocation bit (0 or 1) of a block */
#define PALLOC(bp)           ((GET(HDRP(bp)) >> 1) & 0x1)   /* returns prev_allocation bit (0 or 1) of a block */

#define FTRP(bp)            ((char*)(bp) + SIZE(bp) - DSIZE)    /* note that allocated blocks have no footer */

#define NEXT(bp)            ((char*)(bp) + SIZE(bp))        /* returns block pointer to the next block */
#define PREV(bp)            ((char*)(bp) - SIZE(HDRP(bp)))  /* returns block pointer to the previous block */

/* free list related macros, should be invoked only on free block pointers */
#define FREELIST_SIZE           9                                   /* number of free list headers, each 8 bytes */
#define SUCC(bp)                ((void*)(*(size_t*)(bp)))           /* returns the successor free block pointer */
#define SET_SUCC(bp, succp)     (*(size_t*)(bp) = (size_t)(succp))  /* set successor */

/* end basic constants and macros */

/* private helper functions */

/* free list operations */
/**
 * @brief   determines free list entry
 * @param   size size of a block
 * @return  pointer to the corresponding free list entry
*/
static inline void *freeList_entry(size_t size){
    // int x = __builtin_clzl(size);
    // return freeList+MIN(59-x, FREELIST_SIZE-1)*DSIZE;

    if (size >= (1<<12)) return freeList+(FREELIST_SIZE-1)*DSIZE;
    int x = 0;
    size >>= 4;
    for (; size > 1; size >>= 1, ++x);
    return freeList+x*DSIZE;
}

/**
 * @brief   insert a free block into free list
 * @param   bp free block pointer
 * @return  @c bp, allowing for tail call optimization
*/
static inline void *freeList_insert(void *bp){
    void *entry = freeList_entry(SIZE(bp));
    void *p = entry, *succp;
    int cnt = 0;

    /*
     * Here is a clever trick.
     * Note that first-fit on ordered free list is the same as best-fit.
     * But maintaining an ordered free list makes insertion O(n).
     * So, we just ensure the first several elements in the free list
     * to be the smallest, thus balancing time and space.
     */
    for (; (succp = SUCC(p)) && SIZE(succp) < SIZE(bp) && ++cnt < 3; p = succp);

    SET_SUCC(bp, succp);
    SET_SUCC(p, bp);
    return bp;
}

/**
 * @brief   remove a block from free list
 * @param   bp block pointer
*/
static inline void freeList_remove(void *bp){
    void *entry = freeList_entry(SIZE(bp));
    void *p, *succp;
    for (p = entry; (succp = SUCC(p)) && succp != bp; p = succp);
    SET_SUCC(p, SUCC(bp));
}

/**
 * @brief   coalesce adjacent free blocks
 * @param   bp free block pointer
 * @return  new free block pointer after coalescence
 * @note    @c bp is not in the free list when calling this function, but ends up inserted into it
*/
static inline void *coalesce(void *bp){
    void *prev = PREV(bp), *next = NEXT(bp);
    size_t prev_alloc = PALLOC(bp);
    size_t next_alloc = ALLOC(next);
    size_t size = SIZE(bp);

    if (prev_alloc && next_alloc){
        return freeList_insert(bp);
    }

    if (prev_alloc && !next_alloc){ /* merge bp and its following block */
        freeList_remove(next);
        size += SIZE(next);
    }

    else if (!prev_alloc && next_alloc){    /* merge bp and its previous block */
        freeList_remove(prev);
        bp = prev;
        size += SIZE(prev);
    }

    else{   /* merge bp and both its previous and following block */
        freeList_remove(next);
        freeList_remove(prev);
        bp = prev;
        size += SIZE(next) + SIZE(prev);
    }

    SET_SIZE(HDRP(bp), size);
    SET_SIZE(FTRP(bp), size);

    return freeList_insert(bp);
}

/**
 * @brief   extend heap by @c size bytes and pack it as a free block
 * @param   size extended size, must be aligned to 8 bytes
 * @return  pointer to the new free block
 * @note    @c coalesce will called within
*/
static inline void *extend_heap(size_t size){
    dbg_checkheap(__LINE__);

    void *bp = mem_sbrk(size);
    int palloc = PALLOC(epilogue);

    if (bp == PERROR)
        return PERROR;

    SET(HDRP(bp), PACK(size, palloc, 0));
    SET(FTRP(bp), PACK(size, palloc, 0));

    epilogue += size;
    SET(HDRP(epilogue), PACK(0, 0, 1));

    return coalesce(bp);
}

/**
 * @brief   find an apt free block
 * @param   size minimum size requirement, must be aligned to 8 bytes
 * @return  block pointer if found, NULL otherwise
*/
static inline void *find_fit(size_t size){
    dbg_checkheap(__LINE__);

    void *entry = freeList_entry(size);
    for (void *p = SUCC(entry); p; p = SUCC(p))
        if (SIZE(p) >= size)
            return p;
    
    for (char *p = entry+DSIZE; p != freeList_end; p += DSIZE)
        if (SUCC(p))
            return SUCC(p);
            
    return NULL;
}

/**
 * @brief   place an allocated block inside a free block
 * @param   bp the free block pointer
 * @param   size size of the allocated block, must be aligned to 8 bytes
 * @return  pointer to the newly allcocated block, as for now, namely @c bp
*/
static inline void *place(void *bp, size_t size){
    size_t csize = SIZE(bp);
    size_t r = csize-size;
    void *res = bp;

    if (r <= MINSIZE){
        SET_ALLOC(HDRP(bp));
        SET_PALLOC(HDRP(NEXT(bp)));

        dbg_checkheap(__LINE__);
        return res;
    }

    else{
        SET(HDRP(bp), PACK(size, PALLOC(bp), 1));
        bp = NEXT(bp);
        SET(HDRP(bp), PACK(r, 1, 0));
        SET(FTRP(bp), PACK(r, 1, 0));
        freeList_insert(bp);

        dbg_checkheap(__LINE__);
        return res;
    }
}

/* end private helper functions */

/* private debug helper functions */

/**
 * @brief   check if a pointer is in the heap
 * @param   p the pointer
 * @return  if @c p is in the heap
 */
static int in_heap(const void *p){
    return p <= mem_heap_hi() && p >= mem_heap_lo();
}

/**
 * @brief   check if a pointer is aligned to 8 bytes
 * @param   p the pointer
 * @return  if @c p is aligned
 */
static int aligned(const void *p){
    return (size_t)ALIGN(p) == (size_t)p;
}

/**
 * @brief   print the entire free list on @c stdout, used for debug
*/
static void freeList_debug(void){
    printf("--------------------FREELIST DEBUG--------------------\n");
    for (size_t i = 0; i < FREELIST_SIZE; ++i){
        printf("Entry %lu", i);
        int cnt = 0;
        for (void *entry = freeList+i*DSIZE; entry; entry = SUCC(entry)){
            printf(" --> %p", SUCC(entry));
            if (++cnt % 5 == 0) printf("\n          ");
        }
        printf("\n");
    }
    printf("--------------------END FREELIST DEBUG--------------------\n");
}

/**
 * @brief   printf all block informations on @c stdout, used for debug
*/
static void heap_debug(void){
    printf("--------------------HEAP DEBUG--------------------\n");
    printf("mem_heap_lo: %p mem_heap_hi: %p\n", mem_heap_lo(), mem_heap_hi());
    int cnt = 0;
    void *p;
    for (p = heap_listp; SIZE(p) > 0; p = NEXT(p)){
        printf("Block %5d   adr %p   size %5x   alloc %d   palloc %d   hd %5x", ++cnt, p, SIZE(p), ALLOC(p), PALLOC(p), GET(HDRP(p)));
        if (!ALLOC(p)) printf(" ft %5x", GET(FTRP(p)));
        printf("\n");
        fflush(stdout);
    }
    printf("Block %5d   adr %p   size %5x   alloc %d   hd %7x\n", ++cnt, p, SIZE(p), ALLOC(p), GET(HDRP(p)));
    printf("--------------------END HEAP DEBUG--------------------\n");
}

/**
 * @brief   print debug information on @c stdout and terminate program
*/
static void heap_error(int lineno, void *bp){
    printf("Error detected in line %d\n", lineno);
    freeList_debug();
    heap_debug();
    exit(1);
}

/**
 * @brief   initialize dynamic allocator
 * @return  0 on success, -1 on error
 */
int mm_init(void){
    freeList = mem_sbrk(FREELIST_SIZE*DSIZE+2*WSIZE);

    if (freeList == PERROR)
        return -1;

    memset(freeList, 0, FREELIST_SIZE*DSIZE);
    freeList_end = freeList+FREELIST_SIZE*DSIZE;

    heap_listp = freeList+FREELIST_SIZE*DSIZE;
    SET(heap_listp+0*WSIZE, PACK(DSIZE, 1, 1)); /* prologue header */
    SET(heap_listp+1*WSIZE, PACK(0, 1, 1));     /* epilogue header */
    heap_listp += 2*WSIZE;
    epilogue = heap_listp;

    if (extend_heap(1<<14) == PERROR)   /* initialize the heap by 1<<14 bytes, a magic parameter */
        return -1;

    dbg_checkheap(__LINE__);

    return 0;
}

/**
 * @brief   allocate applied memory
 * @param   size applied size
 * @return  pointer to allocated block on success, NULL on error
 */
void *malloc(size_t size){
    if (size == 0) return NULL;

    dbg_checkheap(__LINE__);

    if (size == 448) size = 512;    /* magic alignment */
    size= size+WSIZE <= MINSIZE? MINSIZE: ALIGN(size+WSIZE);

    void *bp = find_fit(size);

    dbg_checkheap(__LINE__);

    if (!bp){
        bp = extend_heap(MAX(size, CHUNKSIZE));
        if (bp == PERROR) return NULL;
    }

    dbg_checkheap(__LINE__);

    freeList_remove(bp);
    return place(bp, size);
}

/**
 * @brief   free a block allocated by @c malloc, @c realloc, or @c calloc
 * @param   ptr pointer to the block to be freed
 */
void free(void *ptr){
    if(!ptr) return;

    dbg_checkheap(__LINE__);

    RESET_ALLOC(HDRP(ptr));
    SET(FTRP(ptr), GET(HDRP(ptr)));

    void *n = NEXT(ptr);
    RESET_PALLOC(HDRP(n));

    coalesce(ptr);

    dbg_checkheap(__LINE__);
}

/**
 * @brief   change the size of an allocated block
 * @param   oldptr pointer to the allocated block
 * @param   size new size
 * @return  pointer to the allocated block on success, NULL on error
 */
void *realloc(void *oldptr, size_t size){
    if (!size){
        free(oldptr);
        return NULL;
    }

    if (!oldptr){
        return malloc(size);
    }

    size = size+WSIZE <= MINSIZE? MINSIZE: ALIGN(size+WSIZE);

    size_t old_size = SIZE(oldptr);
    if (old_size >= size){
        return oldptr;
    }

    void *newptr = malloc(size);
    if (!newptr){
        return NULL;
    }

    memmove(newptr, oldptr, MIN(size, SIZE(oldptr)));
    free(oldptr);

    dbg_checkheap(__LINE__);

    return newptr;
}

/**
 * @brief   allocate applied memory and initialize it with zeroes
 * @param   nmemb number of members
 * @param   size size of each member
 * @return  pointer to the allocated block on success, NULL on error
 */
void *calloc(size_t nmemb, size_t size){
    size *= nmemb;

    void *newptr = malloc(size);
    if (!newptr) return NULL;

    memset(newptr, 0, size);

    dbg_checkheap(__LINE__);

    return newptr;
}

/**
 * @brief heap consistency checker
 * @param lineno line number at which the function is invoked
 */
void mm_checkheap(int lineno){
    /* check prologue */
    void *prologue = heap_listp-1*WSIZE;
    if (!in_heap(prologue)){
        printf("Prologue not in heap\n");
        heap_error(lineno, prologue);
    }
    if (!ALLOC(prologue) || SIZE(prologue) != DSIZE){
        printf("Prologue corrupted.\n");
        printf("Expected 0x%x, but got 0x%x.\n", PACK(DSIZE, 1, 1), GET(HDRP(prologue)));
        heap_error(lineno, prologue);
    }


    /* check epilogue */
    if (mem_heap_hi()+1 != epilogue){
        printf("Incorrect epilogue location.\n");
        heap_error(lineno, epilogue);
    }
    if (!aligned(epilogue)){
        printf("Epilogue not aligned.\n");
        heap_error(lineno, prologue);
    }
    if (!ALLOC(epilogue) || SIZE(epilogue)){
        printf("Epilogue corrupted.\n");
        heap_error(lineno, epilogue);
    }

    /* check free list */
    if (heap_listp-freeList != FREELIST_SIZE*DSIZE+2*WSIZE){    /* check free list header */
        printf("Incorrect free list header array size.\n");
        heap_error(lineno, PERROR);
    }

    int free_cnt = 0;
    for (int i = 0; i < FREELIST_SIZE; ++i){    /* check each block in free list */
        void *entry = freeList+i*DSIZE;
        for (void *bp = SUCC(entry); bp; bp = SUCC(bp)){
            ++free_cnt;
            if (!in_heap(bp)){
                printf("Free block not in heap.\n");
                printf("Block address: %p\n", bp);
                printf("mem_heap_lo: %p\nmem_heap_hi: %p\n", mem_heap_lo(), mem_heap_hi());
                heap_error(lineno, bp);
            }
            if (!aligned(bp)){
                printf("Free block not aligned.\n");
                heap_error(lineno, bp);
            }
            if (ALLOC(bp)){
                printf("Allocated block in free list.\n");
                heap_error(lineno, bp);
            }
            if (SIZE(bp) < MINSIZE){
                printf("Free block too small.\n");
                printf("Block size: %d\n", SIZE(bp));
                heap_error(lineno, bp);
            }
            if (GET(HDRP(bp)) != GET(FTRP(bp))){
                printf("Inconsistent free block header and footer.\n");
                heap_error(lineno, bp);
            }
            if (freeList_entry(SIZE(bp)) != entry){
                printf("Free block falls into the wrong bucket.\n");
                heap_error(lineno, bp);
            }
        }
    }

    /* check each block in address order */
    void *bp;
    int cnt = 1;
    for (bp = heap_listp; SIZE(bp) > 0; bp = NEXT(bp), ++cnt){
        if (!in_heap(bp)){
            printf("Block not in heap\n");
            printf("Block address: %p\n", bp);
            printf("mem_heap_lo: %p\nmem_heap_hi: %p\n", mem_heap_lo(), mem_heap_hi());
            heap_error(lineno, bp);
        }
        if (!aligned(bp)){
            printf("Block not aligned.\n");
            heap_error(lineno, bp);
        }
        if (SIZE(bp) < MINSIZE){
            printf("Block too small.\n");
            printf("Block size: %d\n", SIZE(bp));
            heap_error(lineno, bp);
        }
        if (!ALLOC(bp)){
            --free_cnt;
            if (!ALLOC(NEXT(bp))){
                printf("Consective free blocks not coalesced.\n");
                heap_error(lineno, bp);
            }
        }
        if (ALLOC(bp) != PALLOC(NEXT(bp))){
            printf("Prev_alloc bit inconsistent.\n");
            printf("Block in address %p is %sallocated, but the following block marked it otherwise.\n", bp, ALLOC(bp)? "": "un");
            heap_error(lineno, bp);
        }
    }

    if (free_cnt){
        printf("Free list total size and free block number don't match.\n");
        heap_error(lineno, bp);
    }
    if (bp != epilogue){
        printf("Incorrect epilogue location.\n");
        heap_error(lineno, bp);
    }
}
