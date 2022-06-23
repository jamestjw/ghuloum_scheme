#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

#define fixnum_mask       3
#define fixnum_tag        0
#define fixnum_shift      2
#define char_mask       255
#define char_tag         15
#define char_shift        8
#define bool_mask       127
#define bool_tag         31
#define bool_shift        7
#define empty_list       47

#define STACK_SIZE 16 * 4096 // 16K cells

extern int scheme_entry(void* stack_base);

void print_result(int val) {
  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%d\n", val >> fixnum_shift);
  } else if ((val & char_mask) == char_tag) {
    printf("%c\n", val >> char_shift);
  } else if ((val & bool_mask) == bool_tag) {
    printf("%d\n", val >> bool_shift);
  } else if (val == empty_list){
    printf("()\n");
  }
}

/*
 * Allocate a block of memory surrounded by two
 * protected pages. The program crashes if memory
 * above or below the dedicated block is accessed.
 */
static void* allocate_protected_space(int size) {
  int status;
  int page_size = getpagesize();
  // Round down to be aligned with page boundaries
  int aligned_size = ((size + page_size - 1) / page_size) * page_size;
  void* p = mmap(0, aligned_size + 2 * page_size,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);

  if (status != 0) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }

  status = mprotect(p, page_size, PROT_NONE);

  if (status != 0) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }

  status = mprotect(p + page_size + aligned_size, page_size, PROT_NONE);

  if (status != 0) {
    fprintf(stderr, "Memory allocation failed\n");
    exit(1);
  }

  return (p + page_size);
}


static void deallocate_protected_space(void* p, int size){
  int status;
  int page_size = getpagesize();
  int aligned_size = ((size + page_size - 1) / page_size) * page_size;

  status = munmap(p - page_size, aligned_size + 2 * page_size);

  if (status != 0) {
    fprintf(stderr, "Failed to deallocate previously allocated memory");
    exit(1);
  }
}

int main(int argc, char** argv) {
  void* stack_top = allocate_protected_space(STACK_SIZE);
  void* stack_base = stack_top + STACK_SIZE;

  print_result(scheme_entry(stack_base));

  deallocate_protected_space(stack_top, STACK_SIZE);

  return 0;
}
