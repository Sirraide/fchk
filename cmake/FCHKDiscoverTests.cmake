enable_testing()

## User or the projects CMake file is expected to provide this.
set(FCHK_EXE_PATH "fchk" CACHE FILEPATH "Path to the fchk executable" )

## Collect tests in one or more directories.
##
## Arguments:
##   IN: One or more directories containing tests.
##   PATTERN: A glob pattern to match test files.
##   PREFIX: FHCK prefix to use. If empty, tests must define a prefix themselves.
##   TEST_NAME_PREFIX: A prefix to add to the test name. Empty by default.
##   WORKING_DIRECTORY: The working directory to use for the test.
##   ARGS: Additional arguments to pass to fchk.
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
    set(FCHKAddAllTestsInDir_WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

    set(options RECURSIVE)
    set(oneValueArgs TEST_NAME_PREFIX PREFIX WORKING_DIRECTORY)
    set(multiValueArgs IN PATTERN ARGS)
    cmake_parse_arguments(FCHKAddAllTestsInDir
        "${options}"
        "${oneValueArgs}"
        "${multiValueArgs}"
        ${ARGN}
    )

    set(fchk_prefix "")
    if (FCHKAddAllTestsInDir_PREFIX)
        set(fchk_prefix --prefix ${FCHKAddAllTestsInDir_PREFIX})
    endif()

    foreach (dir ${FCHKAddAllTestsInDir_IN})
        foreach (pat ${FCHKAddAllTestsInDir_PATTERN})
            file(GLOB_RECURSE tests "${dir}/${pat}")
            foreach (test ${tests})
                add_test(
                    NAME "${FCHKAddAllTestsInDir_TEST_NAME_PREFIX}${test}"
                    COMMAND
                        ${FCHK_EXE_PATH}
                        ${fchk_prefix}
                        ${FCHKAddAllTestsInDir_ARGS}
                        ${test}
                    WORKING_DIRECTORY ${FCHKAddAllTestsInDir_WORKING_DIRECTORY}
                )
            endforeach()
        endforeach()
    endforeach()
endfunction()