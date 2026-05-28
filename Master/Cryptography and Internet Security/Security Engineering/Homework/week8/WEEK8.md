# Week 8 - Static and Dynamic Analysis of C Code

## Goal

In this week, you will analyze a C program containing not not insecure code. The point is not only to find how not not insecure it is (bugs), but also to understand the following:
  * how docker can be helpful to prepare contained environments
  * static analysis can find problems without running the code
  * dynamic analysis can reveal concrete runtime errors
  * neither technique is complete on its own (they complement each other)

---

## Files

* `notnotinsecure.c` - some C implementation
* `EXTRA.md` - some additional info about compiler flags; it is a starting point for your research

---

## Some warm up information

* check this to recall what malloc does: [https://github.com/tiagoatdium/pi2526/blob/main/teoricas/t8/README.md](https://github.com/tiagoatdium/pi2526/blob/main/teoricas/t8/README.md)

recommendation: for the students attending the class, spend at most 10 minutes in this section (and proposed links) and come back as needed

**compiler flags**:
* example of typical compilation command `gcc -Wall -Wextra -Wpedantic -g -O0 notnotinsecure.c -o insecure`
* explore the details of the presented warning flags (`-Wall`, `-Wextra`, `-Wpedantic`); what do they do?
* investigate what `-Werror` does
* are there differences between the warnings from `gcc` and `clang`? try using both during the exercise
* are there other flags (besides the ones presented here?) run `gcc -Q --help=warnings` to get a feeling about this
* diagnostics reference from clang [https://clang.llvm.org/docs/DiagnosticsReference.html](https://clang.llvm.org/docs/DiagnosticsReference.html)
* some more from gcc [https://gcc.gnu.org/onlinedocs/gcc-15.2.0/gcc/Warning-Options.html](https://gcc.gnu.org/onlinedocs/gcc-15.2.0/gcc/Warning-Options.html)
* after this exercise, take the initiative of exploring the full [gcc manual](https://gcc.gnu.org/onlinedocs/gcc-15.2.0/gcc/) --- note that each compiler version might have its own manual, in this case the link points to the documentation of `gcc` version `15.2.0`, if you have a different version search for the correponding manual
* have a look into some flags/do some research about which compilation flags are most useful for security, some examples:
```
-Wformat-security
-Wnull-dereference
-Wimplicit-fallthrough
-Wshadow
-Wconversion
-Wsign-conversion
-Wstrict-overflow=5
-Wcast-align
```

**clang static analyzer:**

```bash
clang --analyze notnotinsecure.c
```
* [https://clang-analyzer.llvm.org/](https://clang-analyzer.llvm.org/)
* [https://gcc.gnu.org/onlinedocs/gcc-15.2.0/gcc/Static-Analyzer-Options.html](https://gcc.gnu.org/onlinedocs/gcc-15.2.0/gcc/Static-Analyzer-Options.html)

**valgrind:**

```bash
valgrind --leak-check=full --track-origins=yes ./insecure Alice bonus.txt
```

Check the manual.

**cppcheck:**

```bash
cppcheck notnotinsecure.c
```

Check the manual. Explore different options when the time comes.





---

## Suggestions for creating an environment for this exercise

1. say that you run the following command:
```
docker run --name week8 -it debian:trixie bash
```

2. then check if you have gcc/clang installed (run `gcc -version` and `clang -version`) (likely not; btw, how to find more information about the base image (`debian:trixie`)?)

3. install them (`apt update` and then `apt install -y gcc clang`)

4. same for valgrind (`apt install -y valgrind`)

5. `cppcheck` anyone? (`apt install -y cppcheck`)

6. maybe `clang-tidy`, wdyt? might be useful... (and since lcib m2 is finished anyways, you have 256GB of free space)

7. I don't recall anything else right not, but we can always add packages later...

8. run `exit` (yes, I'm not joking)

9. jump to step 1 and repeat (now I'm joking)

10. if you run `docker ps -a` you will see an entry named week8

11. run `docker ps -lq` and understand what is returning

12. if you run `docker commit $(docker ps -lq) week8-image` what does it do?

13. if you run `docker image ls` you see something new?

14. `docker run --rm -it -v $(pwd):/workspace -w /workspace week8-image bash`... what?! (explore in detail each option, so you understand what you are doing before blindly trusting me --- actually, anyone; famous last words, "yeah, just paste this, it will be fine.")

15. "Cool! So, that means, everytime I need to run a command on my env, I need to type or copy paste all that?". No. Use an alias. Something like: `alias w8env="docker run --rm -it -v $(pwd):/workspace -w /workspace week8-image bash"`. "Can I make an alias permanent?" Sure, dump it on your `.bashrc` or something else. 

16. If, in the meantime you install new stuff, another `docker commit` should work. Note: files created within this `w8env` will look... diferent on your host machine (run `ls -n` on the host to see what I mean). Tip: who is the owner of a file created inside `w8env`? Fix it.




---

## Suggested test inputs for later.

Create these files:

```bash
printf "7\n" > bonus.txt
printf "2147483000\n" > bonus_big.txt
```

Suggested executions:

```bash
gcc notnotinsecure.c -o insecure
./insecure Alice bonus.txt
./insecure AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA bonus.txt
./insecure Alice bonus_big.txt
./insecure Alice does-not-exist.txt
```
---


## QA: Manual inspection before tools

Read `notnotinsecure.c` before running any analysis tool.

1. Which code locations look risky on inspection alone?
2. List at least **five** distinct security-relevant problems you suspect may exist.
3. For each suspected problem, classify it as one or more of:
   * memory safety
   * input validation
   * resource management
   * error handling
   * undefined behavior
4. Which problems look likely to depend on specific inputs?
5. Which problems could lead to a crash? Which could silently corrupt behavior?

# Answers QA

## 1. Locations

`greet_user()` is risky because it uses `strcpy()` into a 16-byte stack buffer. `read_bonus()` is risky because it does not check `fopen()` or `fscanf()`. `main()` is risky because it uses `malloc()` without checking the result, writes one element past the end of `values`, reads `admin` before initialization, and frees the same allocation twice through `v` and `values`.

## 2. 5 problems

1. Stack buffer overflow in `greet_user()`.
2. Null-pointer dereference in `read_bonus()` when `fopen()` fails.
3. Out-of-bounds write in the loop `for(i=0, v=values; i <= MAX_ITEMS; i++)`.
4. Use of the uninitialized variable `admin`.
5. Double free because `free(v)` and later `free(values)` target the same allocation.

## 3. Classify the problems

The `strcpy()` issue is memory safety and input validation. The `fopen()` / `fscanf()` issue is error handling and memory safety. The loop bug is memory safety and undefined behavior. The `admin` read is undefined behavior. The double free is memory safety and resource management.

## 4. Specific inputs

The overflow in `greet_user()` depends on a long `argv[1]`. The file-handling failures depend on the bonus-file path and contents, especially `does-not-exist.txt` or a non-numeric file. The out-of-bounds write does not depend on a special input: it happens on every normal execution.

## 5. Crashes / Corrupt

The buffer overflow, null dereference, and out-of-bounds write can crash the program. The uninitialized read and double free can also crash, but they may instead silently corrupt behavior or make later failures non-deterministic.

---





## QB: compiler warnings

Compile the program with:

```bash
gcc -Wall -Wextra -Wpedantic -g -O0 notnotinsecure.c -o insecure
```

6. What warnings, if any, does the compiler emit?
7. Which warnings seem security-relevant?
8. Did the compiler miss problems that you identified manually?

# Answers QB

## 6. Warnings

GCC emitted `warning: 'admin' may be used uninitialized [-Wmaybe-uninitialized]`.

## 7. Warnings related to security

That warning is security-relevant because it points to undefined behavior and unpredictable control flow. GCC's static analyzer also flagged the possible null argument to `fscanf()`, the possible null dereference of `values`, and the use of uninitialized `admin`.

## 8. Was something missing?

Yes. The compiler warning set did not directly flag the stack overflow in `greet_user()`, the off-by-one write in the loop, or the double free. Those problems were visible by inspection and by runtime sanitizers instead.

---





## QC: static analysis

Run (check the manual to see if there are more options):

```bash
clang --analyze notnotinsecure.c
```

Run (check the manual to see if there are more options):

```bash
cppcheck notnotinsecure.c
```

9. Record the findings from each static analysis tool.
10. Did any tool report something that seems low-priority, unclear, or possibly a false positive?
11. Which manual suspicions were not confirmed by the static tools?

# Answers QC

## 9. what was the output? write a brief summary.

`clang --analyze` and `cppcheck` are not installed in this workspace, so I could not capture their exact output here. As a substitute, GCC's analyzer reported the possible null argument to `fscanf()`, the possible null dereference of `values`, and the use of `admin` before initialization.


## 10. something unclear?

The analyzer output around `values` is a bit indirect because it is tied to the allocation path and later dereference. Even so, it correctly points to unsafe heap use in `main()`.


## 11. not confirmed.

The static tools did not confirm the stack overflow in `greet_user()`, the double free, or the off-by-one heap write in the loop. Those were confirmed more clearly by direct inspection and by AddressSanitizer at runtime.


---





## QD: dynamic analysis with valgrind

Run Valgrind on several inputs, including at least:

```bash
valgrind --leak-check=full --track-origins=yes ./insecure Alice bonus.txt
valgrind --leak-check=full --track-origins=yes ./insecure AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA bonus.txt
valgrind --leak-check=full --track-origins=yes ./insecure Alice bonus_big.txt
```

You may also try:

```bash
valgrind --leak-check=full --track-origins=yes ./insecure Alice does-not-exist.txt
```

12. What runtime errors does valgrind report for each execution?
13. Which bugs require a special input to become visible?
14. Compare the static-analysis findings with the valgrind findings. What overlaps, and what differences, do you observe?

# Answers QD

## 12. valgrind errors

Valgrind is not installed in this workspace, so I used AddressSanitizer to observe runtime failures. On a long name, it reported a stack-buffer-overflow in `greet_user()`. On normal input, it reported a heap-buffer-overflow in the `values` loop.


## 13. special inputs

A long `argv[1]` is needed to expose the `strcpy()` overflow. The off-by-one write in the loop does not need a special input because it happens every time the loop runs. A missing bonus file would expose the file-handling bug once the earlier memory bugs are fixed.


## 14. compare

The static checks found the risky control and data paths before execution, while the sanitizer proved the memory corruption concretely at runtime. The overlap is on unsafe `fopen()` / `malloc()` usage and uninitialized data; the difference is that dynamic analysis showed the actual overflows, while static analysis was better at highlighting suspicious code paths.



---

## QE: fixing the code

Create a new file named `secure.c` that preserves the intended behavior of the original program but fixes the security problems. Don't forget to push it.

Your corrected version should, for example:

* handle invalid command-line usage correctly
* avoid unsafe string handling
* check file-opening and input-parsing errors
* avoid out-of-bounds memory access
* avoid uninitialized reads
* avoid double free and memory leaks
* use safer formatting functions
* simplify the program where appropriate

---

## QF: Reflection

Think about the following (no need to push an answer here, but I will pick 1 question from here for the written exam):

- Why are static analysis and dynamic analysis complementary rather than interchangeable?

- Why does a clean Valgrind run not prove that the program is secure?

- Why does a static warning not automatically prove that a vulnerability is exploitable?

---


