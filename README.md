# FCHK
An [LLVM FileCheck](https://llvm.org/docs/CommandGuide/FileCheck.html) clone with a terser syntax.

Like FileCheck, this is a tool aimed at testing that text output matches certain patterns. It supports
most of the features that FileCheck does, but with a terser syntax. fchk provides features that make
it easier to use regular expressions without having to wrap them in `[[]]` or `{{}}`.

Design note: While e.g. `*` and `+` may be less obvious than `CHECK` and `CHECK-NEXT` if you’re not
used to either tool, once you’ve written `CHECK-NEXT` 200 times, you’ll probably prefer `+`—at least
I did.

There a lot more differences between fchk and FileCheck than just that; for more information,
check out the [examples file](/test/basic.txt), which showcases most of the features of fchk. You
can try modifying a few lines and running `fchk test/basic.txt` to see what happens.

To run the program, simply add some tests (and a `R` directive) to a file, and run `fchk` on it.

This program runs on Linux and Windows, though it is mainly developed on Linux. MacOS should work
but is untested. Building with MSVC is supported.

## Build
This program uses CMake and requires an up-to-date C++23 compiler. Dependencies (fmtlib and PCRE2)
are downloaded automatically. To build it, just add it as a subdirectory to your CMake project or
build it separately by invoking CMake as usual:
```bash
$ cmake -B build
$ cmake --build build -- -j`nproc`
```

## CMake Integration
fchk provides a CMake function that makes it easy to run tests using CTest. If you’re building fchk
yourself as part of your project, just add the following lines to your CMakeLists.txt:
```cmake
include(CTest)
set(FCHK_FORCE_RELEASE ON) # Always build fchk in Release mode.
add_subdirectory(fchk)
FCHKAddAllTestsInDir(
    IN tests more_tests
    PATTERN "*.foo" "*.bar"
    PREFIX "#"
    WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
    ARGS --first --second --third
    RECURSIVE
)
```

If you’re not building fchk yourself, then you only need to have a copy of 
[cmake/FCHKDiscoverTests.cmake](/cmake/FCHKDiscoverTests.cmake) on your system, include it
in your CMakeLists.txt, and then use the `FCHKAddAllTestsInDir` function as above.

In this example, we’re collecting tests in the directories `tests` and `more_tests`, matching files
with the extension `.foo` and `.bar`, and passing the arguments `--first --second --third` to fchk. We’re also
setting the fchk directive prefix to `#` and the working directory to the project root. Finally, 
the `RECURSIVE` option tells fchk to recurse into subdirectories.

See also the aforementioned CMake file for more information.