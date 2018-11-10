// um.c - Implementation of the ICFP 2006 "Universal Machine"
//
// This file is an implementation of the "Universal Machine" specification part
// of the ICFP 2006 programming contest. The full specification is here:
// http://boundvariable.org/um-spec.txt.
//
// As far as virtual machines go, this implementation is an indirect-threaded
// interpreter with a simple free-list allocator for allocating arrays. It's
// fast enough to do the rest of ICFP 2006 with reasonable performance.

#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <immintrin.h>

#ifdef __AVX__
 // um_alloc uses an avx move on the free list, so the entire free list must
 // be 32-byte aligned.
 #define FREE_LIST_ALIGNMENT 32
#else
 #define FREE_LIST_ALIGNMENT 8
#endif

// The machine shall consist of the following components:

// An infinite supply of sandstone platters, with room on each
// for thirty-two small marks, which we call "bits."
typedef uint32_t platter_t;

struct um_array {
  platter_t *ptr;
  size_t size;
};

struct um_state {
  // Eight distinct general-purpose registers, capable of holding one
  // platter each.
  platter_t regs[8];

  // A collection of arrays of platters, each referenced by a distinct
  // 32-bit identifier. One distinguished array is referenced by 0
  // and stores the "program." This array will be referred to as the
  // '0' array.
  struct um_array *arrays;
  size_t num_arrays;

  // A debug flag, for hacking and debugging.
  bool debug;

  // A "finger", the instruction pointer.
  uint32_t *finger;

  // A free list. See um_alloc and um_abandon for details.
  uint64_t *free_list;

  int first_nonzero_index;
};

void
die(const char *msg) {
  if (errno != 0) {
    perror(msg);
  } else {
    fputs(msg, stderr);
  }

  exit(1);
}

int
min(int a, int b) {
  return a < b ? a : b;
}

// For simplicity, this VM uses a fixed-size free list. It would not be hard to
// expand the free list when it runs out of space, but I haven't had the need to
// do that when doing the rest of the puzzle.
#define FREE_LIST_LENGTH (1 << 18)

// um_alloc allocates a new array and returns a number representing an index
// into the um_state's array table. um_state maintains a free list, which is a
// list of 64-bit integers, where the presence of a set bit at bit N of an
// integer implies that the N'th slot in the state's array table is free for
// allocation.
//
// The free list contains FREE_LIST_LENGTH 64-bit unsigned integers. If the i'th
// bit of the j'th index into the free list is set, it means that the i + (64 *
// j)'th array is free to be allocated.
//
// When allocating an array, um_alloc efficiently scans the list of 64-bit
// integers looking for a non-zero entry. When one is found, a one bit is
// extracted from this entry, cleared, and allocated.
//
// um_alloc never allocates the zero array, since it is specially reserved for
// the currently executing program.
int
um_alloc_slot(struct um_state* state, platter_t cap, int fl_index, int bit) {
  // The index that is ultimately going to be returned is the slot plus the
  // bit - the index into the state's array table.
  int index = bit + 64 * fl_index;

  // The array table is initially small. If we need to expand it, do so here.
  if (index >= state->num_arrays) {
    state->num_arrays += index;
    state->arrays =
      realloc(state->arrays, state->num_arrays * sizeof(struct um_array));
    if (!state->arrays) {
      die("state arrays malloc");
    }
  }

  // At this point we have chosen an index to allocate to this new array. It
  // must not be zero (we never allocate the zero array) and it must point to
  // a valid index in the array table.
  //
  // Allocate the requested capacity array and initialize the chosen array
  // index with its pointer and length.
  assert(index > 0);
  assert(index < state->num_arrays);
  state->arrays[index].size = cap;
  state->arrays[index].ptr = calloc(cap, sizeof(platter_t));
  if (!state->arrays[index].ptr) {
    die("state arrays cap alloc");
  }

  // Zero out the new array.
  memset(state->arrays[index].ptr, 0, cap * sizeof(platter_t));

  // Clear the bit of the index we just allocated.
  state->free_list[fl_index] &= ~(1UL << bit);
  return index;
}


int
um_alloc(struct um_state *state, platter_t cap) {
  for (int i = state->first_nonzero_index; i < FREE_LIST_LENGTH; i++) {
    uint64_t entry = state->free_list[i];
    if (entry == 0) {
      continue;
    }

    state->first_nonzero_index = i;
    return um_alloc_slot(state, cap, i, __builtin_ffsl(entry) - 1);
  }

  die("free list completely full");
  return -1;
}

// um_abandon abandons an array at the given index. It frees the memory
// allocated for the given array and sets the bit in the free list corresponding
// to this index so that this array can be allocated again later.
void
um_abandon(struct um_state *state, int index) {
  free(state->arrays[index].ptr);
  state->arrays[index].size = 0;
  int slot = index / 64;
  state->first_nonzero_index = min(state->first_nonzero_index, slot);
  int bit = index % 64;
  state->free_list[slot] |= (1UL << bit);
}

// um_state_init initializes the UM state using a program saved in the requested
// file.
void
um_state_init(struct um_state *state, const char* filename) {
  // The machine shall be initialized with a '0' array whose contents
  // shall be read from a "program" scroll.
  int fd = open(filename, O_RDONLY);
  if (!fd) {
    die("opening program");
  }

  struct stat fs;
  int status = fstat(fd, &fs);
  if (status != 0) {
    die("getting file status");
  }

  platter_t* program = mmap(NULL, fs.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (program == MAP_FAILED) {
    die("mmap file");
  }

  // Start out with one array initially. We'll expand it as necessary.
  state->num_arrays = 1;
  state->arrays = calloc(state->num_arrays, sizeof(struct um_array));
  platter_t *copied_program = malloc(fs.st_size);
  if (!copied_program) {
    die("um_state_init malloc");
  }

  for (size_t i = 0; i < fs.st_size / sizeof(platter_t); i++) {
    // When reading programs from legacy "unsigned 8-bit character"
    // scrolls, a series of four bytes A,B,C,D should be interpreted with
    // 'A' as the most magnificent byte, and 'D' as the most shoddy, with
    // 'B' and 'C' considered lovely and mediocre respectively.
    //
    // (translation: UM programs are big-endian. Since my machine is
    // little-endian, we need to swap the byte order.)
    copied_program[i] = __builtin_bswap32(program[i]);
  }
  state->arrays[0].ptr = copied_program;
  state->arrays[0].size = fs.st_size;

  // All registers shall be initialized with platters of value '0'.
  memset(state->regs, 0, sizeof(platter_t) * 8);

  // The execution finger shall point to the first platter of the '0' array,
  // which has offset zero.
  state->finger = state->arrays[0].ptr;
  state->debug = false;

  // Allocate and zero our free list. We won't resize it (for now).
  state->free_list = aligned_alloc(FREE_LIST_ALIGNMENT, FREE_LIST_LENGTH * sizeof(uint64_t));
  // state->free_list = calloc(FREE_LIST_LENGTH, sizeof(uint64_t));
  if (!state->free_list) {
    die("malloc free list");
  }

  memset(state->free_list, UINT8_MAX, FREE_LIST_LENGTH * sizeof(uint64_t));

  // Mark the zero array as permanently allocated. It always represents the
  // running program and should never be reallocated.
  state->free_list[0] &= ~1;
  state->first_nonzero_index = 0;
}

// Simple debug routine that dumps out the next instruction to be executed. Not
// to be used in normal operation.
void
um_debug_hook(struct um_state* state) {
  static const char op_table[13][5] = {
    "cmov",
    "arri",
    "arra",
    "add",
    "mul",
    "div",
    "nand",
    "halt",
    "allc",
    "aban",
    "out",
    "in",
    "ldpg"
  };

  if (!state->debug) {
    return;
  }
  
  platter_t inst = *state->finger;
  int op = (0xF0000000 & inst) >> 28;
  if (op >= 0 && op <= 12) {
    int a = (0x1C0 & inst) >> 6;
    int b = (0x38 & inst) >> 3;
    int c = 0x7 & inst;
    fprintf(stderr, "%4s r%d r%d r%d\n", op_table[op], a, b, c);
    return;
  }

  if (op == 13) {
    int a = (0x0E000000 & inst) >> 25;
    fprintf(stderr, "cons r%d %d\n", a, 0x01FFFFFF & inst);
    return;
  }

  fprintf(stderr, "illegal instruction");
}

// um_trap explicitly traps. There are a few behaviors where the spec says that
// it's legal for us to fail and we generally translate those into calls to
// um_trap when appropriate.
void
um_trap(struct um_state* state, const char* type) {
  die(type);
}

// um_dispatch is the core of the UM interpreter. It is an indirect-threaded
// interpreter that uses the current instruction opcode to index into
// dispatch_table, which is a table of code addresses that can be jump targets.
//
// The DISPATCH macro advances the instruction pointer, gets the opcode of the
// next instruction, and jumps to the code address in dispatch_table
// corresponding to the opcode. This interpreter uses two C extensions to
// accomplish this: "labels as values", which allows the use of a
// double-addressof operator "&&" to produce a void* from a label (representing
// the address of the label), and "computed goto", which allows the use of a
// "goto *expr" statement that jumps to the code address in "expr". This is
// significantly faster than the normal "for/switch" bytecode dispatch mechanism
// since it is friendlier to branch predictors.
void
um_dispatch(struct um_state *state) {
  static void *dispatch_table[] = {
    &&cmov, // 0
    &&arri, // 1
    &&arra, // 2
    &&add,  // 3
    &&mul,  // 4
    &&div,  // 5
    &&nand, // 6
    &&halt, // 7
    &&allc, // 8
    &&aban, // 9
    &&out,  // 10
    &&in,   // 11
    &&ldpg, // 12
    &&cons, // 13
    &&ill,  // 14
    &&ill,  // 15
  };

  // Each Standard Operator performs an errand using three registers,
  // called A, B, and C. Each register is described by a three bit
  // segment of the instruction platter. The register C is described by
  // the three least meaningful bits, the register B by the three next
  // more meaningful than those, and the register A by the three next
  // more meaningful than those.
  //
  // Before this operator is discharged, the execution finger shall be
  // advanced to the next platter, if any.
  #define REG(x) state->regs[(x)]
  #define ARR(x) state->arrays[(x)].ptr
  #define ARR_LEN(x) state->arrays[(x)].size
  #define DISPATCH() do {                            \
    inst = *state->finger++;                         \
    goto *dispatch_table[(0xF0000000 & inst) >> 28]; \
  } while (0)

  #define A ((0x1C0 & inst) >> 6)
  #define B ((0x38 & inst) >> 3)
  #define C (0x7 & inst)
  #define CONS_A ((0x0E000000 & inst) >> 25)
  #define CONS (0x01FFFFFF & inst)

  platter_t inst;
  // Begin executing by jumping to the label of the first instruction. All
  // labels terminate their execution by calling DISPATCH, which jumps to the
  // handler of the next instruction.
  DISPATCH();

  cmov: // Conditional Move
    // The register A receives the value in register B,
    // unless the register C contains 0.
    if (REG(C) != 0) {
      REG(A) = REG(B);
    }

    DISPATCH();
  arri: // Array Index.
    // The register A receives the value stored at offset
    // in register C in the array identified by B.
    REG(A) = ARR(REG(B))[REG(C)];
    DISPATCH();
  arra: // Array Amendment.
    // The array identified by A is amended at the offset
    // in register B to store the value in register C.
    ARR(REG(A))[REG(B)] = REG(C);
    DISPATCH();
  add: // Addition.
    // The register A receives the value in register B plus 
    // the value in register C, modulo 2^32.
    REG(A) = REG(B) + REG(C);
    DISPATCH();
  mul: // Multiplication.
    // The register A receives the value in register B times
    // the value in register C, modulo 2^32.
    REG(A) = REG(B) * REG(C);
    DISPATCH();
  div: // Division.
    // The register A receives the value in register B
    // divided by the value in register C, if any, where
    // each quantity is treated treated as an unsigned 32
    // bit number.
    if (REG(C) == 0) {
      um_trap(state, "divide by zero");
    }

    REG(A) = REG(B) / REG(C);
    DISPATCH();
  nand: // Not-And.
    // Each bit in the register A receives the 1 bit if
    // either register B or register C has a 0 bit in that
    // position.  Otherwise the bit in register A receives
    // the 0 bit.
    REG(A) = ~(REG(B) & REG(C));
    DISPATCH();
  halt: // Halt.
    // The universal machine stops computation.
    return;
  allc: { // Allocation.
    // A new array is created with a capacity of platters
    // commensurate to the value in the register C. This
    // new array is initialized entirely with platters
    // holding the value 0. A bit pattern not consisting of
    // exclusively the 0 bit, and that identifies no other
    // active allocated array, is placed in the B register.
    REG(B) = um_alloc(state, REG(C));
    DISPATCH();
  }
  aban: // Abandonment.
    um_abandon(state, REG(C));
    DISPATCH();
  out: // Output.
    // The value in the register C is displayed on the console
    // immediately. Only values between and including 0 and 255
    // are allowed.
    if (REG(C) > 255) {
      um_trap(state, "output out of range");
    }

    putchar(REG(C));
    DISPATCH();
  in: // Input.
    // The universal machine waits for input on the console.
    // When input arrives, the register C is loaded with the
    // input, which must be between and including 0 and 255.
    // If the end of input has been signaled, then the 
    // register C is endowed with a uniform value pattern
    // where every place is pregnant with the 1 bit.
    REG(C) = getchar();
    if (REG(C) == EOF) {
      if (ferror(stdin)) {
        die("getting input");
      }

      assert(feof(stdin));
      REG(C) = (platter_t)-1;
    }
    DISPATCH();
  ldpg: { // Load Program.
    // The array identified by the B register is duplicated
    // and the duplicate shall replace the '0' array,
    // regardless of size. The execution finger is placed
    // to indicate the platter of this array that is
    // described by the offset given in C, where the value
    // 0 denotes the first platter, 1 the second, et
    // cetera.
    //
    // The '0' array shall be the most sublime choice for
    // loading, and shall be handled with the utmost
    // velocity.
    if (REG(B) == 0) {
      state->finger = ARR(0) + REG(C);
      DISPATCH();
    }

    free(ARR(0));
    ARR(0) = calloc(ARR_LEN(REG(B)), sizeof(platter_t));
    if (!ARR(0)) {
      die("load program malloc");
    }

    ARR_LEN(0) = ARR_LEN(REG(B));
    memcpy(ARR(0), ARR(REG(B)), ARR_LEN(REG(B)) * sizeof(platter_t));
    state->finger = ARR(0) + REG(C);
    DISPATCH();
  }
  cons: // Orthography.
    // The value indicated is loaded into the register A
    // forthwith.
    REG(CONS_A) = CONS;
    DISPATCH();
  ill: // Illegal instruction.
    um_trap(state, "illegal instruction");
    DISPATCH();

  #undef REG
  #undef ARR
  #undef ARR_LEN
  #undef DISPATCH
  #undef A
  #undef B
  #undef C
  #undef CONS_A
  #undef CONS
}

int main(int argc, char** argv) {
  if (argc != 2) {
    puts("usage: ./um <program>");
    return 0;
  }

  struct um_state state;
  um_state_init(&state, argv[1]);
  um_dispatch(&state);
  return 0;
}
