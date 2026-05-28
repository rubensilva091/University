# Week 11: Symbolic Dynamic Execution with KLEE

**Tool:** KLEE
**Goal:** Understand what KLEE is, run it on small C programs, and inspect the test cases it generates.

KLEE is a symbolic execution engine for programs compiled to [LLVM bitcode](https://llvm.org/docs/BitCodeFormat.html). The official [KLEE documentation](https://klee-se.org/getting-started/) suggests Docker as one of the fastest ways to get started, and the [first KLEE tutorial](https://klee-se.org/tutorials/testing-function/) introduces symbolic inputs through `klee_make_symbolic`. See [Tutorials](https://klee-se.org/docs/) for more information.

For each question, write your answer directly below the question.

---

# Part 1 - Start KLEE with Docker

In this worksheet, you will create files on your host machine and access them from inside the KLEE Docker container:
- the idea is that the host machine folder is mounted into the Docker container.

This lets you edit files normally on your machine while running KLEE inside the container.

## 1.1 Go to this week's folder

On your host machine, go to the folder for this week's class, `week11` (create it in your repository), and copy this file into there.

All files for this worksheet should be created inside this `week11` folder. Don't push binary files/large files.

## 1.2 Pull the official KLEE Docker image

Run:

```sh
docker pull klee/klee
```

The official KLEE documentation has a [dedicated Docker page](https://klee-se.org/docker/) and describes Docker as a way to run KLEE in an isolated container without modifying the host system.

## 1.3 Start a KLEE container from inside `week11`

Make sure you are inside the `week11` folder on your host machine. You can run the following command to check "where you are":

```sh
pwd
```

Then start the container:

```sh
docker run --rm -it -v "$PWD":/home/klee/week11 -w /home/klee/week11 klee/klee
```

This command mounts your current host folder, `week11`, into the container at:

```text
/home/klee/week11
```

The option:

```text
-w /home/klee/week11
```

makes `/home/klee/week11` the current working directory inside the container.

You should now be inside the container.

Check that KLEE works:

```sh
klee --version
clang --version
ktest-tool --help
```

## Questions

Question 1. What does `klee --version` print?

Answer: KLEE 3.2 (https://klee-se.org/), Build mode: RelWithDebInfo (Asserts: TRUE), LLVM version 16.0.6

---

Question 2. What does `clang --version` print?

Answer: clang version 16.0.6, Target: x86_64-unknown-linux-gnu

---

# Part 2 - From concrete execution to symbolic execution

Before using KLEE, let us first think about what normally happens when a program runs.

Consider this function:

```c
int get_sign(int x)
{
  if (x == 0)
    return 0;

  if (x < 0)
    return -1;
  else
    return 1;
}
```

If we call:

```c
get_sign(5);
```

then `x` has the concrete value `5`. The program follows one path:

```text
x == 0       false
x < 0        false
return 1
```

If we call:

```c
get_sign(-3);
```

then the program follows another path:

```text
x == 0       false
x < 0        true
return -1
```

And if we call:

```c
get_sign(0);
```

then the program follows yet another path:

```text
x == 0       true
return 0
```

Ordinary testing asks us to choose these concrete inputs ourselves. If we only test `5`, we only see the positive case. If we forget to test `0`, we may miss behavior specific to zero.

**Symbolic execution** changes the question. Instead of choosing a value for `x`, we ask the tool to treat `x` as an unknown symbolic value.

Informally, we run the program with:

```text
x = some unknown integer
```

As the program executes, the symbolic execution engine records the conditions needed to reach each branch.

For `get_sign`, the interesting paths are:

```text
Path 1: x == 0
Path 2: x != 0 and x < 0
Path 3: x != 0 and x >= 0
```

The tool can then ask a [solver](https://klee-se.org/docs/solver-chain/) for concrete examples that satisfy these path conditions. For example:

```text
Path 1: x == 0              example input:  0
Path 2: x != 0 and x < 0    example input: -1
Path 3: x != 0 and x >= 0   example input:  1
```

This is the key idea behind using KLEE for test generation:

```text
symbolic input -> path constraints -> solver -> concrete test cases
```

KLEE does not run C source code directly. Instead, we compile the C program to LLVM bitcode, and KLEE symbolically executes that bitcode.

In the next part, we will explicitly mark an integer as symbolic using:

```c
klee_make_symbolic(&a, sizeof(a), "a");
```

This tells KLEE:

```text
The bytes of variable 'a' are not fixed. Treat them as symbolic input.
```

Then KLEE will try to generate concrete tests for the feasible paths through the program.

## Questions

Question 1. In ordinary testing, who chooses the concrete input values?

Answer: The tester (programmer or QA engineer) manually chooses the concrete input values to test.

---

Question 2. In symbolic execution, what does it mean for `x` to be symbolic?

Answer: x is treated as an unknown value that can take on any integer value. Instead of running the program with a fixed input, symbolic execution explores all possible paths the program could take for different values of x.

---

Question 3. For `get_sign`, why would only testing `x = 5` be incomplete?

Answer: Testing only x = 5 covers only the positive case (x > 0). It misses the zero case (x == 0) and the negative case (x < 0), so you wouldn't validate all branches of the function.

---

Question 4. What a solver helps KLEE produce?

Answer: The solver helps KLEE produce concrete test cases by solving the constraints that represent conditions needed to reach each path through the program.

---

Question 5. Why do we compile C code to LLVM bitcode before running KLEE?

Answer: KLEE works on LLVM bitcode, not directly on C source code. The bitcode is an intermediate representation that KLEE can symbolically execute and instrument for path exploration.

---

# Part 3 - First KLEE program: `get_sign`

The first official KLEE tutorial tests a small function called `get_sign`. The program marks an integer as symbolic with `klee_make_symbolic`, then asks KLEE to explore the possible paths.

## 3.1 Create the file

Create the file `get_sign.c`:

```c
#include <klee/klee.h>

int get_sign(int x) {
  if (x == 0)
    return 0;

  if (x < 0)
    return -1;
  else
    return 1;
}

int main()
{
  int a;

  klee_make_symbolic(&a, sizeof(a), "a");

  return get_sign(a);
}
```

The important line here is:

```c
klee_make_symbolic(&a, sizeof(a), "a");
```

This tells KLEE that the bytes of variable `a` should be treated as symbolic input.

## 3.2 Compile to LLVM bitcode

Since KLEE works on LLVM bitcode, not directly on C source code. Compile `get_sign.c` as follows:

```sh
clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone get_sign.c
```

This should create:

```sh
get_sign.bc
```

Check with the following command:

```sh
ls
```

## 3.3 Run KLEE

Run:

```sh
klee get_sign.bc
```

You should see output showing that KLEE explored several paths and generated test cases.

KLEE also creates output directories such as:

```text
klee-out-0
klee-last
```

`klee-last` is a convenient symbolic link to the most recent KLEE output directory (running `ls -n klee-last` shows the link).

Check the generated files:

```sh
ls klee-last/
```

## 3.4 Inspect the test cases

Run:

```sh
ktest-tool klee-last/test000001.ktest
ktest-tool klee-last/test000002.ktest
ktest-tool klee-last/test000003.ktest
```

## Questions

Question 1. How many test cases did KLEE generate?

Answer: 3

---

Question 2. What values of `a` did KLEE generate?

Answer: a = 0, a = 16843009 (positive), a = -2147483648 (negative)

---

Question 3. Which test corresponds to `a == 0`?

Answer: test000001

---

Question 4. Which test corresponds to `a < 0`?

Answer: test000003 (a = -2147483648)

---

Question 5. Which test corresponds to `a > 0`?

Answer: test000002 (a = 16843009)

---

Question 6. Why did KLEE need only a few tests here?

Answer: The function has only three distinct paths based on simple numeric comparisons. KLEE efficiently found one test case for each path: zero, negative, and positive values.

---

# Part 4 - Your first modification

Now create a slightly more interesting program.

## 4.1 Create `classify.c`

```c
#include <klee/klee.h>

int classify(int x)
{
  if (x == 42)
    return 1;

  if (x > 100 && x < 110)
    return 2;

  if (x * x == 25)
    return 3;

  return 0;
}

int main()
{
  int a;

  klee_make_symbolic(&a, sizeof(a), "a");

  return classify(a);
}
```

## 4.2 Compile and run

```sh
clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone classify.c
klee classify.bc
```

Inspect the generated tests:

```sh
ls klee-last
```

Then inspect some `.ktest` files:

```sh
ktest-tool klee-last/test000001.ktest
ktest-tool klee-last/test000002.ktest
ktest-tool klee-last/test000003.ktest
```

There may be more than three tests.

## Questions

Question 1. How many test cases did KLEE generate?

Answer: 4

---

Question 2. Did KLEE find an input for `x > 100 && x < 110`?

Answer: Yes, test000003 with a = 101

---

Question 3. Did KLEE find an input make the condition `x * x == 25` true? In which test?

Answer: Yes, test000004 with a = 5

---

Question 4. What concrete values did it generate?

Answer: a = 42, a = 0, a = 101, a = 5

---

Question 5. Are the generated values the only possible values for each branch?

Answer: No. For example, any value between 101-109 would satisfy `x > 100 && x < 110`, and both 5 and -5 satisfy `x * x == 25`. KLEE provides one example per branch, not all possibilities.

---

Question 6. What is the difference between "a value that reaches this branch" and "all values that reach this branch"?

Answer: "A value that reaches this branch" is one concrete example that satisfies the branch condition (what KLEE generates). "All values that reach this branch" is the set of all possible inputs that satisfy the condition (which is infinite for integer ranges).

---

# Part 5 - Finding bugs with `klee_assert`

KLEE can be used not only to generate tests, but also to find assertion failures.

## 5.1 Create `buggy_abs.c`

```c
#include <assert.h>
#include <klee/klee.h>

int buggy_abs(int x)
{
  if (x < 0)
    return -x;

  return x;
}

int main()
{
  int a;

  klee_make_symbolic(&a, sizeof(a), "a");

  int r = buggy_abs(a);

  klee_assert(r >= 0);

  return 0;
}
```

## 5.2 Compile and run

```sh
clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone buggy_abs.c
klee buggy_abs.bc
```

KLEE should report an assertion failure for one input.

Inspect the error files:

```sh
ls klee-last
```

Look for files ending in `.assert.err`.

For example:

```sh
cat klee-last/test000001.assert.err
```

**Note that exact test number may be different.**

Inspect the corresponding `.ktest` file, for example:

```sh
ktest-tool klee-last/test000001.ktest
```

**Adjust the test number if needed.**

## Questions

Question 1. Does KLEE report an assertion failure?

Answer: Yes

---

Question 2. Which input causes the failure?

Answer: a = -2147483648 (the minimum 32-bit signed integer)

---

Question 3. Why does this input break `buggy_abs`?

Answer: When x = -2147483648, the negation -x overflows and results in -2147483648 again (the smallest negative int has no positive equivalent in 32-bit signed integers), violating the assertion r >= 0.

---

Question 4. Would you have tested this input manually?

Answer: Unlikely. Most programmers would test typical negative numbers like -1 or -100, not the extreme boundary value.

---

Question 5. What does this example suggest about boundary cases?

Answer: Boundary cases and extreme values are critical to test and are easy to miss in manual testing. Symbolic execution automatically explores these edge cases.

---

## Hint

Think about the smallest signed 32-bit integer.

---

# Part 6 - Symbolic command-line arguments

KLEE can also create symbolic [command-line arguments](https://klee-se.org/docs/options/). The [official symbolic environment tutorial](https://klee-se.org/tutorials/using-symbolic/) describes options such as `-sym-arg`, and notes that symbolic command-line arguments are used together with KLEE's POSIX runtime.

## 6.1 Create `password.c`

```c
#include <stdio.h>

int check_password(char *buf)
{
  //
  // This is a toy example for symbolic execution only.
  // Do not hardcode real passwords in programs.
  //
  if (buf[0] == 'h' &&
      buf[1] == 'e' &&
      buf[2] == 'l' &&
      buf[3] == 'l' &&
      buf[4] == 'o')
    return 1;

  return 0;
}

int main(int argc, char **argv)
{
  if (argc < 2)
    return 1;

  if (check_password(argv[1]))
  { printf("password found!\n");
    return 0;
  }

  return 1;
}
```

## 6.2 Compile

```sh
clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone password.c
```

## 6.3 Run with a symbolic argument

```sh
klee -posix-runtime password.bc -sym-arg 5
```

Here:

```text
-posix-runtime
```

enables KLEE's POSIX (Portable Operating System Interface) environment support.

Many C programs do not just compute over local variables. They interact with an execution environment: command-line arguments, standard input, standard output, files, file descriptors, environment variables, and so on. These interfaces are commonly described by POSIX-like operating-system behavior.

KLEE's POSIX runtime provides a symbolic model of this environment. In this example, it is necessary because we want KLEE to create and manage a symbolic `argv[1]`.

The option:

```text
-sym-arg 5
```

creates one symbolic command-line argument of length up to 5 bytes.

Inspect the tests:

```sh
ls klee-last
```

Then inspect the `.ktest` files:

```sh
ktest-tool klee-last/test000001.ktest
ktest-tool klee-last/test000002.ktest
```

Find the test case containing:

```text
hello
```

## Questions

Question 1. Did KLEE find the password?

Answer: Yes

---

Question 2. Which `.ktest` file contains the successful input?

Answer: test000005

---

Question 3. What happens with the following command?

```sh
klee -posix-runtime password.bc -sym-arg 3
```

Answer: KLEE would generate fewer test cases (for argument lengths up to 3 bytes). The password "hello" requires 5 bytes, so KLEE would not find a test case that satisfies all 5 character conditions.

---

Question 4. What happens with the following command?

```sh
klee -posix-runtime password.bc -sym-arg 10
```

Answer: KLEE would generate more test cases to cover symbolic arguments of length up to 10 bytes. It would still find the password "hello" among other test cases, since hello fits within 10 bytes.

---

Question 5. Why does the length of the symbolic argument matter?

Answer: The length determines the search space. A shorter length may miss the actual password if it's longer than the specified limit, while a longer length increases the number of paths and test cases KLEE needs to explore.

---

# Part 7 - Path explosion

In this part, we will use a program that is still small, but already gives KLEE noticeably more work than the previous examples.

Create `path_explosion.c`:

```c
#include <assert.h>
#include <klee/klee.h>

int main(void)
{
  unsigned char input[16];
  int score = 0;

  klee_make_symbolic(input, sizeof(input), "input");

  for (int i = 0; i < 16; i++)
  {
    if (input[i] == 'A')
      score += 1;
    else
      score -= 1;
  }

  if (score == 16)
  { klee_assert(0 && "all bytes were A"); }

  return 0;
}
```

Compile it:

```sh
clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone path_explosion.c
```

Run KLEE:

```sh
klee path_explosion.bc
```

This program creates a symbolic byte array with 16 bytes:

```c
unsigned char input[16];
```

The call:

```c
klee_make_symbolic(input, sizeof(input), "input");
```

tells KLEE to treat all 16 bytes of `input` as symbolic.

The loop then checks all 16 symbolic bytes:

```c
for (int i = 0; i < 16; i++)
```

Inside the loop, each checked byte creates a branch:

```c
if (input[i] == 'A')
```

For one byte, there are two cases:

```text
input[0] == 'A'
input[0] != 'A'
```

For two bytes, there are four combinations:

```text
input[0] == 'A'   and input[1] == 'A'
input[0] == 'A'   and input[1] != 'A'
input[0] != 'A'   and input[1] == 'A'
input[0] != 'A'   and input[1] != 'A'
```

For three bytes, there are eight combinations.

In general, if the loop checks `n` symbolic bytes, there can be up to:

```text
2^n
```

branch combinations.

Here the loop checks 16 bytes, so there can be up to:

```text
2^16 = 65536
```

branch combinations.

The assertion is reached only in the special case where every checked byte is `'A'`:

```text
input[0] == 'A'
input[1] == 'A'
...
input[15] == 'A'
```

In that case, `score` is incremented 16 times, so:

```text
score == 16
```

and the program reaches:

```c
klee_assert(0 && "all bytes were A");
```

This is a small example of **path explosion**: the C program is short, but the number of paths KLEE may need to consider grows exponentially as the loop checks more symbolic bytes.

Inspect the output:

```sh
ls klee-last
```

Look for an assertion error:

```sh
ls klee-last/*.assert.err
```

Inspect the corresponding test case. The exact number may differ:

```sh
ktest-tool klee-last/test000001.ktest
```

If `test000001.ktest` is not the assertion-triggering test, inspect the `.assert.err` filename and use the matching `.ktest` file.

---

## Questions

Question 1. How many symbolic bytes are allocated in `input`?

Answer: 16 bytes

---

Question 2. How many symbolic bytes are checked by the loop?

Answer: 16 bytes (the loop iterates from i=0 to i=15)

---

Question 3. At each loop iteration, what are the two branch cases KLEE has to consider?

Answer: (1) input[i] == 'A' (increment score), or (2) input[i] != 'A' (decrement score)

---

Question 4. If the loop checks 16 bytes and each byte creates two branch cases, how many branch combinations are possible?

Answer: 2^16 = 65,536

---

Question 5. Which input reaches the assertion failure?

Answer: input = "AAAAAAAAAAAAAAAA" (all 16 bytes are 'A')

---

Question 6. Why does this program take longer for KLEE to analyse than `get_sign.c`?

Answer: `get_sign.c` has only 3 paths, while this program has 2^16 = 65,536 possible paths due to the loop checking 16 symbolic bytes. This exponential growth in paths is path explosion.

---

Question 7. What would you expect to happen if the loop bound changed from `16` to `20` and the input array also changed to `input[20]`?

Answer: KLEE would need to explore 2^20 = 1,048,576 paths, taking significantly longer and requiring more memory. The analysis time would grow exponentially.

---

Question 8. What would you expect to happen if the loop bound changed from `16` to `5` and the input array also changed to `input[5]`?

Answer: KLEE would need to explore only 2^5 = 32 paths, completing much faster. The assertion would be triggered only when all 5 bytes equal 'A'.

---



# Part 8 - Wrap-up

Answer these in your own words.

## Exit questions

Question 1. What is symbolic execution?

Answer: Symbolic execution is a program analysis technique that treats input variables as symbolic (unknown) values rather than concrete values, and explores all possible execution paths by tracking constraints on these symbolic values.

---

Question 2. Why do we compile C code to `.bc` files?

Answer: KLEE operates on LLVM bitcode (.bc files), which is an intermediate representation. Compiling to bitcode allows KLEE to instrument and analyze the program at the intermediate level rather than working directly with source code.

---

Question 3. What does `klee_make_symbolic` do?

Answer: It marks a variable or memory region as symbolic, telling KLEE to treat its bytes as unknown inputs rather than fixed values during execution.

---

Question 4. What is a `.ktest` file?

Answer: A .ktest file is a test case generated by KLEE containing concrete input values that cause the program to follow a specific execution path. It includes the argument list and symbolic variable values.

---

Question 5. What is one advantage of KLEE over "normal" unit testing?

Answer: KLEE automatically generates test cases that explore different execution paths and often finds edge cases (like boundary values) that a human tester might miss. It doesn't require manual selection of test inputs.

---

Question 7. What is one limitation of KLEE?

Answer: Path explosion -> the number of paths grows exponentially with program complexity, making analysis infeasible for larger programs with many branches or loops over symbolic data.

---

Question 8. What is path explosion?

Answer: Path explosion occurs when the number of execution paths a program can take grows exponentially with the number of symbolic inputs and branch points. For example, checking n symbolic bytes creates up to 2^n paths, making analysis extremely slow for large n.

---

# Optional exercises

## Optional A - Add an impossible branch

Modify `classify.c` and add:

```c
if (x > 10 && x < 5)
  return 99;
```

Recompile and rerun KLEE.

Questions:

Question 1. Does KLEE generate a test for this branch?

Answer: No

---

Question 2. Why or why not?

Answer: The condition `x > 10 && x < 5` is impossible. No integer value can be simultaneously greater than 10 and less than 5. KLEE's constraint solver recognizes this contradiction and marks the branch as unsatisfiable, so no test case is generated for it.

---

## Optional B - SHA-3

Replicate the results of [https://mouha.be/sha-3-buffer-overflow-part-2/](https://mouha.be/sha-3-buffer-overflow-part-2/). You can find this and many more external resources here: [https://klee-se.org/docs/](https://klee-se.org/docs/).

### Solution

Created `hkdf.c` with the HKDF code pattern from the article that was vulnerable in an older version of BoringSSL's HKDF implementation. The code includes a `klee_assert(done + todo >= done)` to check for integer overflow in the addition.

Compiled and ran KLEE:

```sh
clang -emit-llvm -c -g -O0 -Xclang -disable-O0-optnone hkdf.c
klee hkdf.bc
```

### Results

KLEE executed the symbolic analysis and completed successfully:

- **Total instructions**: 12,540
- **Completed paths**: 512
- **Generated tests**: 512
- **Assertion failures**: None (No `.assert.err` files)

### Conclusion

KLEE confirmed that the integer addition `done + todo` **cannot overflow** in this code pattern. Even with all possible values of `out_len` being symbolic, KLEE found no input that causes the assertion `done + todo >= done` to fail.

This demonstrates that the overflow check `if (out_len + digest_len < out_len || n > 255)` at the beginning of the function is sufficient to prevent integer overflow in the loop. The check works by detecting if adding `digest_len` to `out_len` would wrap around (overflow), which would result in a value smaller than `out_len`.

This case study shows how KLEE can be used to formally verify that a potentially vulnerable code pattern is actually safe in practice.

