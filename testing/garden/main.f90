program garden_main
    use garden, only : test_item_t, test_that, run_tests
    use garden_mesh_generator, only : test_assert_eq_double_mat
    implicit none

    if (.not.run()) stop 1

contains
    function run() result(passed)
        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = test_assert_eq_double_mat()
        
        tests = test_that(individual_tests)

        passed = run_tests(tests)
    end function run
end program garden_main