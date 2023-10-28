enable_testing()

## User or the projects CMake file is expected to provide this.
set(FCHK_EXE_PATH "fchk" CACHE FILEPATH "Path to the fchk executable" )

## Collect tests in one or more directories.
##
## Arguments:
##   IN: One or more directories containing tests.
##   PATTERN: A glob pattern to match test files.
##   PREFIX: A prefix to add to the test name. Empty by default.
##   RECURSIVE: If set, recurse into subdirectories.
##
## Example:
##   FCHKAddAllTestsInDir(
##     IN tests more_tests
##     PATTERN "*.foobar"
##     RECURSIVE
##   )
function(FCHKAddAllTestsInDir)
    set(FCHKAddAllTestsInDir_PREFIX "")

    set(options RECURSIVE)
    set(oneValueArgs PREFIX)
    set(multiValueArgs IN PATTERN)
    cmake_parse_arguments(FCHKAddAllTestsInDir
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

    foreach (dir ${FCHKAddAllTestsInDir_IN})
        foreach (pat ${FCHKAddAllTestsInDir_PATTERN})
            file(GLOB_RECURSE tests "${dir}/${pat}")
            foreach (test ${tests})
                add_test(
                    NAME "${FCHKAddAllTestsInDir_PREFIX}${test}"
                    COMMAND ${FCHK_EXE_PATH} ${test}
                )
            endforeach()
        endforeach()
    endforeach()
endfunction()