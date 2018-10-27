# ICFP 2006 Universal Machine

This repo contains an implementation of the [ICFP 2006 Universal
Machine](http://boundvariable.org/task.shtml), as specified by the [the
specification](http://boundvariable.org/um-spec.txt). It's pretty fast, fast
enough to navigate the various parts of ICFP 2006 with near real-time
responsiveness.

You'll need gcc or clang and you'll want optimizations:

```
$ cc um.c -O2 -o um
$ ./um sandmark.umz
trying to Allocate array of size 0..
trying to Abandon size 0 allocation..
trying to Allocate size 11..
...
```

`um.c` makes use of some non-standard C and compiler-specific intrinsics, so
beware. It compiles fine for me with clang.
