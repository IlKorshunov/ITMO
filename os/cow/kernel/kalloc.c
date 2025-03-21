// Physical memory allocator, for user processes,
// kernel stacks, page-table pages,
// and pipe buffers. Allocates whole 4096-byte pages.

#include "constants.h"
#include "defs.h"
#include "memlayout.h"
#include "spinlock.h"
#include "types.h"

void freerange(void *pa_start, void *pa_end);

extern char end[];  // first address after kernel.
                    // defined by kernel.ld.

struct run {
  struct run *next;
};

struct {
  struct spinlock lock;
  struct run *freelist;
  int count[GET_INDEX(PHYSTOP)];
} kmem;

int get_count(void *pa) {
  acquire(&kmem.lock);
  int a = kmem.count[GET_INDEX(pa)];
  release(&kmem.lock);
  return a;
}

int increase_ref(void *pa) {
  int ind = GET_INDEX(pa);
  acquire(&kmem.lock);
  int a = ++kmem.count[ind];
  release(&kmem.lock);
  return a;
}

int decrease_ref(void *pa) {
  int ind = GET_INDEX(pa);
  acquire(&kmem.lock);
  int a = --kmem.count[ind];
  release(&kmem.lock);
  return a;
}

void kinit() {
  initlock(&kmem.lock, "kmem");
  freerange(end, (void *)PHYSTOP);
}

void freerange(void *pa_start, void *pa_end) {
  char *p;
  p = (char *)PGROUNDUP((uint64)pa_start);
  for (; p + PGSIZE <= (char *)pa_end; p += PGSIZE) {
    kmem.count[GET_INDEX(p)] = 1;
    kfree(p);
  }
}

// Free the page of physical memory pointed at by pa,
// which normally should have been returned by a
// call to kalloc().  (The exception is when
// initializing the allocator; see kinit above.)
void kfree(void *pa) {
  struct run *r;

  if (((uint64)pa % PGSIZE) != 0 || (char *)pa < end || (uint64)pa >= PHYSTOP)
    panic("kfree");

  acquire(&kmem.lock);
  // decrease
  if (--kmem.count[GET_INDEX(pa)]) {
    release(&kmem.lock);
    return;
  }

  if (kmem.count[GET_INDEX(pa)] < 0) {
    panic("черт");
  }

  r = (struct run *)pa;

  // Fill with junk to catch dangling refs.
  memset(pa, 1, PGSIZE);

  r->next = kmem.freelist;
  kmem.freelist = r;
  release(&kmem.lock);
}

// Allocate one 4096-byte page of physical memory.
// Returns a pointer that the kernel can use.
// Returns 0 if the memory cannot be allocated.
void *kalloc(void) {
  struct run *r;

  acquire(&kmem.lock);
  r = kmem.freelist;
  if (r) {
    kmem.count[GET_INDEX(r)] = 1;
    kmem.freelist = r->next;
  }
  release(&kmem.lock);

  if (r) memset((char *)r, 5, PGSIZE);  // fill with junk
  return (void *)r;
}