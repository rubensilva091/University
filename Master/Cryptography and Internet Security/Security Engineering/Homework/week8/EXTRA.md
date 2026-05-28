# Some Compiler flags that are relevant for security

This is a practical shortlist of the compiler and linker flags that matter most in a software security course, especially for C and C++. Note: the following list was compiled using the assistance of an LLM. The list is, effectively, a representative set of some of the most relevant flags but it should be considered as the starting point for more investigation on this subject.


1. `-Wall` - Turns on a broad set of useful warnings. Not exhaustive, but the baseline for catching suspicious code early.

2. `-Wextra` - Adds more warnings beyond `-Wall`. Often catches logic mistakes and unsafe patterns that are easy to miss.

3. `-Wpedantic` - Warns about non-portable or non-standard code. Good for teaching where code relies on compiler extensions.

4. `-Werror` - Treats warnings as errors. Very useful in labs and CI when you want students to actually fix issues instead of ignoring them.

5. `-Wformat=2` - A stronger set of format-string diagnostics. Great for surfacing `printf`-style misuse.

6. `-Wformat-security` - Focuses on risky format-string usage, such as non-literal format strings in dangerous contexts.

7. `-Wshadow` - Warns when a local declaration shadows another variable. Helpful for preventing subtle review-resistant bugs.

8. `-Wconversion` - Warns on implicit conversions that may change a value. Excellent for finding truncation bugs.

9. `-Wsign-conversion` - Warns on signed/unsigned conversions. Very relevant for size checks, bounds checks, and length handling.

10. `-Wnull-dereference` - Warns about code paths that may dereference null pointers.

11. `-fsanitize=address` - Enables AddressSanitizer. One of the most valuable teaching tools for buffer overflows, use-after-free, and related memory bugs.

12. `-fsanitize=undefined` - Enables UndefinedBehaviorSanitizer checks. Useful for invalid shifts, overflow-related undefined behavior, misaligned access, and more.

13. `-fsanitize=leak` - Enables LeakSanitizer. Good for showing ownership mistakes and cleanup failures.

14. `-fno-omit-frame-pointer` - Preserves frame pointers, which makes sanitizer and debugger stack traces much easier to read.

15. `-g` - Includes debug information. Essential for exploit labs or sanitizer reports.

16. `-D_FORTIFY_SOURCE=2` - Requests fortified libc checks in optimized builds. Useful for teaching how some unsafe library misuse can be caught or hardened at compile/runtime.

17. `-fstack-protector-strong` - Adds stack canaries to protect many functions that are attractive overflow targets.

18. `-fstack-clash-protection` - Adds protection against stack-clash style attacks on supported targets.

19. `-fPIE` - Builds position-independent executables when paired with `-pie`. Important for ASLR-friendly binaries.

20. `-pie` - Links the program as a position-independent executable. Pair with `-fPIE`.

---

# And Some More

These are also very important, and often belong in a hardened build profile:

* `-Wl,-z,relro`
* `-Wl,-z,now`
* `-Wl,-z,noexecstack`
* `-O1` or `-Og` for sanitizer-focused lab builds
* `-fsanitize=bounds` in selected demonstrations
* `-fsanitize=cfi` for advanced control-flow integrity topics
* `-ftrapv` for teaching signed overflow failures

---

# Examples

## Warning-heavy profile

```bash
-Wall -Wextra -Wpedantic -Werror \
-Wformat=2 -Wformat-security -Wshadow -Wconversion -Wsign-conversion
```

## Sanitizer profile

```bash
-g -O1 -fno-omit-frame-pointer \
-fsanitize=address,undefined,leak
```

## Hardened release profile

```bash
-O2 -D_FORTIFY_SOURCE=2 -fstack-protector-strong -fstack-clash-protection \
-fPIE -pie -Wl,-z,relro -Wl,-z,now -Wl,-z,noexecstack
```

