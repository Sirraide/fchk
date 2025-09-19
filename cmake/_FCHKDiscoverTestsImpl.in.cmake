set(dirs @FCHKAddAllTestsInDir_absolute_paths@)
foreach (dir IN LISTS dirs)
    foreach (pat "@FCHKAddAllTestsInDir_PATTERN@")
        file(GLOB_RECURSE tests "${dir}/${pat}")
        foreach (test ${tests})
            set(test_name "${dir}.${test}")
            message(STATUS "Adding test ${test_name}")
            add_test(
                "@FCHKAddAllTestsInDir_TEST_NAME_PREFIX@${test_name}"
                [==[@FCHK_EXE_PATH@]==]
                --prefix [==[@fchk_prefix@]==]
                @FCHKAddAllTestsInDir_ARGS_ESCAPED@
                "${test}"
            )

            set_tests_properties("@FCHKAddAllTestsInDir_TEST_NAME_PREFIX@${test_name}"
                PROPERTIES WORKING_DIRECTORY [==[@FCHKAddAllTestsInDir_WORKING_DIRECTORY@]==]
            )

            if (@FCHKAddAllTestsInDir_DEPENDS@)
                set_tests_properties("@FCHKAddAllTestsInDir_TEST_NAME_PREFIX@${test_name}"
                    PROPERTIES FIXTURES_REQUIRED _fchk_build_deps
                )
            endif()
        endforeach()
    endforeach()
endforeach()
