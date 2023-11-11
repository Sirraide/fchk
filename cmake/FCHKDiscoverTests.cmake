enable_testing()

## User or the projects CMake file is expected to provide this.
if (NOT DEFINED FCHK_EXE_PATH)
    set(FCHK_EXE_PATH "fchk" CACHE FILEPATH "Path to the fchk executable" )
endif()

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
##   DEPENDS: Mark that the tests depend on one or more targets.
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
    set(multiValueArgs IN PATTERN ARGS DEPENDS)
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

    ## Ungodly jank to allow people to add dependencies on other targets to the tests.
    if (FCHKAddAllTestsInDir_DEPENDS)
        foreach (dependency ${FCHKAddAllTestsInDir_DEPENDS})
            if (NOT TEST "_fchk_build_deps-${dependency}")
                add_test(
                    NAME "_fchk_build_deps-${dependency}"
                    COMMAND "${CMAKE_COMMAND}"
                    --build "${CMAKE_BINARY_DIR}"
                    --config "$<CONFIG>"
                    --target "${dependency}"
                    COMMAND_EXPAND_LISTS
                )

                set_tests_properties("_fchk_build_deps-${dependency}"
                    PROPERTIES FIXTURES_SETUP _fchk_build_deps
                )
            endif()
        endforeach()
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

                if (FCHKAddAllTestsInDir_DEPENDS)
                    set_tests_properties("${FCHKAddAllTestsInDir_TEST_NAME_PREFIX}${test}"
                        PROPERTIES FIXTURES_REQUIRED _fchk_build_deps
                    )
                endif()
            endforeach()
        endforeach()
    endforeach()
endfunction()
