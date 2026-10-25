# Stage 0 modernization

Clean compile under modern C with identical runtime behavior.

## Changes

- **C standard**: CMake now uses C11 (`CMAKE_C_STANDARD 11`)
- **ANSI prototypes**: All functions converted from K&R style to ANSI C prototypes
- **Header**: `lisp.h` now has full prototypes (was old-style empty `()`)
- **`main` / `quit`**: Proper signatures; `quit` returns `struct lnode *` and is `_Noreturn`
- **`error`**: Marked `_Noreturn` (it always `longjmp`s)
- **NULL-as-integer**: Replaced casts like `(int)NULL` / `(char)NULL` with plain `0`
- **`TRUE`**: `#define TRUE 1` instead of `!NULL`
- **Signal handler**: Single-argument modern form `void sig_handler(int sig)`
- **FILE\* encoding**: Pointers stored with `PRIxPTR` / `SCNxPTR` and `uintptr_t` (64-bit safe)
- **Buffer**: Open-file handle string buffer enlarged to 32 bytes
- **`labs` bug**: Registration used C library `labs`; now correctly uses `lisp_abs`
- **Braces / fallthrough**: Clarified a few ambiguous `else` / switch fallthroughs (no logic change)
- **Warnings**: Builds with `-Wall -Wextra -Wpedantic` and **zero warnings**

## Build

```bash
mkdir build && cd build
cmake ..
make
./lisp
```

## Note

Recursive `fact` returning `NIL` is a pre-existing interpreter behavior, present in the original sources as well. Stage 0 intentionally does not change semantics.
