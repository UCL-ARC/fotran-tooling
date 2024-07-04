module test_cal_pi
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use, intrinsic :: iso_fortran_env

    use calc_pi, only : calculate_pi
    
    implicit none

    private :: test_calculate_pi

    public :: collect_calc_pi_suite

    integer, parameter :: dp = selected_real_kind(15)

contains

    !> Collect all exported unit tests
    subroutine collect_calc_pi_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("calculate_pi", test_calculate_pi), &
            new_unittest("calculate_pi_parameterised", test_calculate_pi_parameterised), &
            new_unittest("test_skip_example", test_skip_example) &
        ]
    end subroutine collect_calc_pi_suite

    !------------------------
    ! Tests
    !------------------------

    subroutine test_calculate_pi(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real64)   :: actual_pi, expected_pi = 3.141592

        actual_pi = calculate_pi(10000_dp)
        call check(error, actual_pi, expected_pi, thr=1e-06_dp)
    end subroutine test_calculate_pi

    subroutine test_calculate_pi_parameterised(error)
        type(error_type), allocatable, intent(out) :: error

        integer(kind=int64) :: steps_to_test(5) = (/10, 100, 1000, 10000, 100000/)
        real(kind=real64)   :: thresholds_to_test(5) = (/1e-03, 1e-05, 1e-07, 1e-07, 1e-07/)
        real(kind=real64)   :: actual_pi, expected_pi = 3.14159265359
        integer             :: i

        do i = 1, 5
            actual_pi = calculate_pi(steps_to_test(i))
            call check(error, expected_pi, actual_pi, thr=thresholds_to_test(i))
        end do 
    end subroutine test_calculate_pi_parameterised

    subroutine test_skip_example(error)
        type(error_type), allocatable, intent(out) :: error

        real(kind=real64)   :: actual_pi, expected_pi = 3.141592

        call skip_test(error, "This feature is not implemented yet")
        return

        actual_pi = calculate_pi(10000_dp)
        call check(error, actual_pi, expected_pi, thr=1e-06_dp)
    end subroutine test_skip_example

end module test_cal_pi