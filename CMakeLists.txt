cmake_minimum_required(VERSION 3.20)
project(fchk VERSION 3.1.0 LANGUAGES CXX)

## ============================================================================
##  Global CMake Variables.
## ============================================================================
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

## ============================================================================
##  Global compiler options.
## ============================================================================
## Turn on diagnostics colours.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-fdiagnostics-color=always)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-fcolor-diagnostics)
endif()

## Use mold as the default linker, if it exists.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    find_program(MOLD_LINKER "mold")
    if (MOLD_LINKER)
        if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
            add_compile_options(-fuse-ld=mold)
        endif()
        add_link_options(-fuse-ld=mold)
    endif()
endif()

## ============================================================================
##  Compiler options.
## ============================================================================
add_library(_fchk_options INTERFACE)

## Allow the user to force building this in release mode.
option(FCHK_FORCE_RELEASE "Force building fchk in release mode." OFF)

## Flags for Clang and GCC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(_fchk_options INTERFACE
        ## Warnings.
        -Wall -Wextra     # Enable ‘all’ warnings.
        -Wundef           # Invalid #undef or undefined macro in #if.
        -Wcast-align      # Casting that changes alignment.
        -Wconversion      # Implicit conversions.
        -Wsign-conversion # Implicit sign conversions.
        -Wformat=2        # Stricter format checking.

        ## Disabled warnings.
        -Wno-unused-function
        -Wno-unused-local-typedefs

        ## NULL Errors.
        -Werror=nonnull # Passing NULL to nonnull parameter.

        ## Memory Errors.
        -Werror=address              # Suspicious use of addresses.
        -Werror=init-self            # Initialization of a variable with itself.
        -Werror=uninitialized

        ## Return type.
        -Werror=return-type

        ## C/C++.
        -Werror=implicit-fallthrough
        -Werror=pointer-arith        # Disallow void* and function pointer arithmetic.
        -Werror=string-compare       # Nonsensical string comparisons.
        -Werror=switch               # Missing switch cases.
        # -Werror=switch-enum          # Switch on enum (even if there is a default case).
        -Werror=write-strings        # Strings in C should be const char*.

        ## C++.
        -Werror=missing-field-initializers
        -Werror=non-virtual-dtor
        -Werror=pessimizing-move
    )
endif()

## Additional flags for GCC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(_fchk_options INTERFACE
        -Wlogical-op      # Duplicate or unintended logical operators.
        -Werror=invalid-memory-model # For atomics.
        -Werror=maybe-uninitialized
        -Werror=missing-requires
        -Werror=return-local-addr
    )
endif()

## Additional flags for Clang.
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(_fchk_options INTERFACE
        -Werror=dangling
        -Werror=return-stack-address
    )
endif()

## Flags for MSVC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_compile_options(_fchk_options INTERFACE
        /W4 # Enable ‘all’ warnings.

        # Allow unnamed structs/unions.
        /wd4201

        # Source code is UTF-8.
        /utf-8
    )
endif()

## On Windows, don’t suggest the _s nonsense functions.
if (WIN32)
    target_compile_definitions(_fchk_options INTERFACE
        _CRT_SECURE_NO_WARNINGS
        _CRT_SECURE_NO_WARNINGS_GLOBALS
        _CRT_NONSTDC_NO_WARNINGS
    )
endif()

if (ENABLE_ASAN)
    target_compile_options(_fchk_options INTERFACE -fsanitize=address)
    target_link_options(_fchk_options INTERFACE -fsanitize=address)
endif()

if (MSVC)
    target_compile_options(_fchk_options INTERFACE /Zc:preprocessor)
endif()
## Debug/Release flags.
if (FCHK_FORCE_RELEASE)
    if (NOT MSVC)
        target_compile_options(_fchk_options INTERFACE -O3 -march=native)
        target_link_options(_fchk_options INTERFACE -O3 -march=native)
    else()
        target_compile_options(_fchk_options INTERFACE /O2)
    endif()
else()
    if (NOT MSVC)
        target_compile_definitions(_fchk_options INTERFACE _GLIBCXX_DEBUG)
        target_compile_options(_fchk_options INTERFACE
            $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3>
            $<$<CONFIG:RELEASE>:-O3 -march=native>
        )
        target_link_options(_fchk_options INTERFACE
            $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic>
            $<$<CONFIG:RELEASE>:-O3 -march=native>
        )
    else()
        target_compile_options(_fchk_options INTERFACE
            $<$<CONFIG:DEBUG>:/Od>
            $<$<CONFIG:RELEASE>:/O2>
        )
    endif()
endif()

## ============================================================================
##  Submodules and include dirs.
## ============================================================================
include(FetchContent)

## Get libbase.
message(STATUS "Downloading libbase ...")
set(LIBBASE_CXXFLAGS -fno-exceptions -fno-rtti -fwrapv -fnew-infallible)
FetchContent_Declare(libbase
    GIT_REPOSITORY https://github.com/Sirraide/libbase
    GIT_TAG master
)
FetchContent_MakeAvailable(libbase)

## Link against libfmt.
target_link_libraries(_fchk_options INTERFACE libbase)

## ‘src’ should be an include directory.
target_include_directories(_fchk_options INTERFACE src)

## ‘include’ too, if it exists.
if (EXISTS ${PROJECT_SOURCE_DIR}/include)
    target_include_directories(_fchk_options INTERFACE include)
endif()

## As well as everything in ‘libs’.
if (EXISTS ${PROJECT_SOURCE_DIR}/libs)
    file(GLOB libs ${PROJECT_SOURCE_DIR}/libs/*)

    ## Add the include directory to the include path, or the root
    ## directory if there is no include directory.
    foreach(lib ${libs})
        if (IS_DIRECTORY ${lib}/include)
            target_include_directories(_fchk_options INTERFACE ${lib}/include)
        else()
            target_include_directories(_fchk_options INTERFACE ${lib})
        endif()
    endforeach()

    ## Also add all of them as subdirectories if they have a CMakeLists.txt.
    foreach (lib ${libs})
        if (EXISTS ${lib}/CMakeLists.txt)
            add_subdirectory(${lib})
        endif()
    endforeach()
endif()

## ============================================================================
##  Executables and libraries.
## ============================================================================
add_library(fchk-core STATIC src/core.cc src/core.hh src/errs.hh)
add_executable(fchk src/main.cc)

## Project dir so we can strip it from diags.
target_compile_definitions(fchk-core PUBLIC
    "FCHK_PROJECT_DIR_NAME=\"fchk\""
    "FCHK_PROJECT_DIR=\"${PROJECT_SOURCE_DIR}\""
    "FCHK_VERSION=\"${PROJECT_VERSION}\""
)

## Apply our options.
target_link_libraries(fchk-core PRIVATE _fchk_options)
target_link_libraries(fchk PRIVATE fchk-core _fchk_options)
target_include_directories(fchk-core PUBLIC src)

## Automatically provide access to the test discovery feature.
set(FCHK_EXE_PATH "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/fchk")
include("${PROJECT_SOURCE_DIR}/cmake/FCHKDiscoverTests.cmake")

## ============================================================================
##  Tests.
## ============================================================================
if (FCHK_TEST_FCHK)
    Include(FetchContent)

    FetchContent_Declare(
      Catch2
      GIT_REPOSITORY https://github.com/catchorg/Catch2.git
      GIT_TAG        v3.4.0
    )

    FetchContent_MakeAvailable(Catch2)

    add_executable(tests test/tests.cc)
    target_link_libraries(tests PRIVATE fchk-core Catch2::Catch2WithMain _fchk_options)

    include(CTest)
    include(Catch)
    catch_discover_tests(tests)
endif()
