#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

#define fixnum_mask 3
#define fixnum_tag 0
#define fixnum_shift 2
#define char_mask 255
#define char_tag 15
#define char_shift 8
#define bool_mask 127
#define bool_tag 31
#define bool_shift 7
#define empty_list 47
#define pair_tag 1
#define pair_mask 7

#define STACK_SIZE 16 * 4096 // 16K cells
#define HEAP_SIZE 16 * 4096  // 16K cells

typedef struct
{
  void *rax; /* 0 scratch */
  void *rbx; /* 8 preserve */
  void *rcx; /* 16 scratch */
  void *rdx; /* 24 scratch */
  void *rsi; /* 32 preserve */
  void *rdi; /* 40 preserve */
  void *rbp; /* 48 preserve */
  void *rsp; /* 56 preserve */
} context;

extern long scheme_entry(context *ctxt, void *heap, void *stack_base);

void print_val(long val)
{
  if ((val & fixnum_mask) == fixnum_tag)
  {
    printf("%ld", val >> fixnum_shift);
  }
  else if ((val & char_mask) == char_tag)
  {
    printf("%c", (char)(val >> char_shift));
  }
  else if ((val & bool_mask) == bool_tag)
  {
    printf("%d", (int)(val >> bool_shift));
  }
  else if (val == empty_list)
  {
    printf("()");
  }
  else if ((val & pair_mask) == pair_tag)
  {
    long car_ptr = *((long *)((void *)val - 1));
    long cdr_ptr = *((long *)((void *)val + 7));
    // Print pair
    printf("(");
    print_val(car_ptr);
    printf(" . ");
    print_val(cdr_ptr);
    printf(")");
  }
  else
  {
    printf("Unknown value %ld\n", val);
  }
}

/*
 * Allocate a block of memory surrounded by two
 * protected pages. The program crashes if memory
 * above or below the dedicated block is accessed.
 */
static void *allocate_protected_space(int size)
{
  int status;
  int page_size = getpagesize();
  // Round down to be aligned with page boundaries
  int aligned_size = ((size + page_size - 1) / page_size) * page_size;
  void *p = mmap(NULL, aligned_size + 2 * page_size,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);

  if (p == MAP_FAILED)
  {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }

  status = mprotect(p, page_size, PROT_NONE);

  if (status != 0)
  {
    fprintf(stderr, "Memory allocation failed, unable to protect page boundaries\n");
    exit(1);
  }

  status = mprotect(p + page_size + aligned_size, page_size, PROT_NONE);

  if (status != 0)
  {
    fprintf(stderr, "Memory allocation failed, unable to protect page boundaries\n");
    exit(1);
  }

  return (p + page_size);
}

static void deallocate_protected_space(void *p, int size)
{
  int status;
  int page_size = getpagesize();
  int aligned_size = ((size + page_size - 1) / page_size) * page_size;

  status = munmap(p - page_size, aligned_size + 2 * page_size);

  if (status != 0)
  {
    fprintf(stderr, "Failed to deallocate previously allocated memory");
    exit(1);
  }
}

int main(int argc, char **argv)
{
  context ctxt; // Allocate space to be used to store register values
  void *stack_top = allocate_protected_space(STACK_SIZE);
  void *stack_base = stack_top + STACK_SIZE;
  void *heap = allocate_protected_space(STACK_SIZE);

  long res = scheme_entry(&ctxt, stack_base, heap);

  print_val(res);
  printf("\n");

  deallocate_protected_space(stack_top, STACK_SIZE);
  deallocate_protected_space(heap, STACK_SIZE);

  return 0;
}
