module test_mesh_generator
    use, intrinsic :: iso_fortran_env
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use mesh_generator
    
    implicit none 

    real(kind=real64) :: test_threshold = 1e-06

    !> test_calculate_mesh_parameters inputs and outputs
    type :: calculate_mesh_parameters_inputs
        real(kind=real64)   :: edge_size
        integer(kind=int64) :: box_size
    end type calculate_mesh_parameters_inputs
    type :: calculate_mesh_parameters_expected_ouputs
        integer(kind=int64) :: num_edges_per_boundary, num_nodes, num_boundary_nodes, num_elements
    end type calculate_mesh_parameters_expected_ouputs

    !> test_calculate_mesh inputs and outputs
    type :: calculate_mesh_inputs
        integer(kind=int64) :: num_edges_per_boundary, num_nodes, num_elements, num_boundary_nodes
    end type calculate_mesh_inputs
    type :: calculate_mesh_expected_ouputs
        integer(kind=int64), dimension(:, :), allocatable :: elements
        integer(kind=int64), dimension(:, :), allocatable :: boundary_edges
        real(kind=real64), dimension(:, :), allocatable :: nodes
    end type calculate_mesh_expected_ouputs
contains
    !> Collect all test in this module into a single test suite
    !! 
    !! @param testsuite - An array of unittest_types in which to store this suite's tests
    subroutine collect_mesh_generator_testsuite(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_calculate_mesh_parameters_5_1", test_calculate_mesh_parameters_5_1), &
            new_unittest("test_calculate_mesh_parameters_10_05", test_calculate_mesh_parameters_10_05), &
            new_unittest("test_calculate_mesh_8_2_8_9", test_calculate_mesh_8_2_8_9) &
            ]
    end subroutine collect_mesh_generator_testsuite

    !> A unit test template for the calculate_mesh_parameters subroutine.
    !! 
    !! @param error - An allocatable error_type to track failing tests.
    !! @param inputs - A structure containing the required inputs for 
    !!                 calling calculate_mesh_parameters.
    !! @param expected_outputs - A structure containing the outputs we 
    !!                           expect for the provided inputs.
    subroutine verify_calculate_mesh_parameters(error, inputs, expected_outputs)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(calculate_mesh_parameters_inputs), intent(in) :: inputs
        type(calculate_mesh_parameters_expected_ouputs), intent(in) :: expected_outputs

        integer(kind=int64) :: actual_num_edges_per_boundary, actual_num_nodes,  &
                   actual_num_boundary_nodes, actual_num_elements

        call calculate_mesh_parameters(inputs%box_size, inputs%edge_size, &
                actual_num_edges_per_boundary, actual_num_nodes,              &
                actual_num_boundary_nodes, actual_num_elements)

        call check(error, expected_outputs%num_boundary_nodes,     actual_num_boundary_nodes)
        call check(error, expected_outputs%num_edges_per_boundary, actual_num_edges_per_boundary)
        call check(error, expected_outputs%num_elements,           actual_num_elements)
        call check(error, expected_outputs%num_nodes,              actual_num_nodes)
        
        ! Catch test failure
        if (allocated(error)) return
    end subroutine
    !> A unit test for the calculate_mesh_parameters subroutine with:
    !!     box_size = 5
    !!     edge_size = 1.0
    subroutine test_calculate_mesh_parameters_5_1(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(calculate_mesh_parameters_inputs) :: inputs
        type(calculate_mesh_parameters_expected_ouputs) :: expected_outputs
        ! Setup inputs
        inputs%box_size = 5
        inputs%edge_size = 1.0
        ! Setup expected outputs
        expected_outputs%num_boundary_nodes = 20
        expected_outputs%num_edges_per_boundary = 5
        expected_outputs%num_elements = 50
        expected_outputs%num_nodes = 36
        ! Call parent test
        call verify_calculate_mesh_parameters(error, inputs, expected_outputs)
    end subroutine test_calculate_mesh_parameters_5_1
    !> A unit test for the calculate_mesh_parameters subroutine with:
    !!     box_size = 10
    !!     edge_size = 0.5
    subroutine test_calculate_mesh_parameters_10_05(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(calculate_mesh_parameters_inputs) :: inputs
        type(calculate_mesh_parameters_expected_ouputs) :: expected_outputs
        ! Setup inputs
        inputs%box_size = 10
        inputs%edge_size = 0.5
        ! Setup expected outputs
        expected_outputs%num_boundary_nodes = 80
        expected_outputs%num_edges_per_boundary = 20
        expected_outputs%num_elements = 800
        expected_outputs%num_nodes = 441
        ! Call parent test
        call verify_calculate_mesh_parameters(error, inputs, expected_outputs)
    end subroutine test_calculate_mesh_parameters_10_05


    !> A unit test template for the calculate_mesh subroutine.
    !! 
    !! @param error - An allocatable error_type to track failing tests.
    !! @param inputs - A structure containing the required inputs for 
    !!                 calling calculate_mesh.
    !! @param expected_outputs - A structure containing the outputs we 
    !!                           expect for the provided inputs.
    subroutine verify_calculate_mesh(error, inputs, expected_outputs)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(calculate_mesh_inputs), intent(in) :: inputs
        type(calculate_mesh_expected_ouputs), intent(in) :: expected_outputs

        integer(kind=int64), dimension(3, inputs%num_elements) :: actual_elements
        integer(kind=int64), dimension(3, inputs%num_boundary_nodes) :: actual_boundary_edges
        real(kind=real64), dimension(2, inputs%num_nodes) :: actual_nodes
        real(kind=real64) :: threshold = 1e-06
        character*80 :: failure_message

        integer :: i, j

        call calculate_mesh(inputs%num_edges_per_boundary, inputs%num_nodes, inputs%num_elements, inputs%num_boundary_nodes, actual_nodes, actual_elements, actual_boundary_edges)

        do i = 1, inputs%num_elements
            do j = 1, 3
                write(failure_message,'(a,i1,a,i1,a,i2,a,i2)') "Unexpected value for elements(", j, ",", i, "), got ", actual_elements(j, i), " expected ", expected_outputs%elements(j, i)
                call check(error, expected_outputs%elements(j, i), actual_elements(j, i), failure_message)
                if (allocated(error)) return
            end do 
        end do

        do i = 1, inputs%num_boundary_nodes
            do j = 1, 3
                write(failure_message,'(a,i1,a,i1,a,i2,a,i2)') "Unexpected value for boundary_edges(", j, ",", i, "), got ", actual_boundary_edges(j, i), " expected ", expected_outputs%boundary_edges(j, i)
                call check(error, expected_outputs%boundary_edges(j, i), actual_boundary_edges(j, i), failure_message)
                if (allocated(error)) return
            end do
        end do

        do i = 1, inputs%num_nodes
            do j = 1, 2
                write(failure_message,'(a,i1,a,i1,a,f3.1,a,f3.1)') "Unexpected value for nodes(", j, ",", i, "), got ", actual_nodes(j, i), " expected ", expected_outputs%nodes(j, i)
                call check(error, expected_outputs%nodes(j, i), actual_nodes(j, i), failure_message, thr=threshold)
                if (allocated(error)) return
            end do
        end do
    end subroutine verify_calculate_mesh
    !> A unit test for the calculate_mesh subroutine with:
    !!     num_boundary_nodes = 8
    !!     num_edges_per_boundary = 2
    !!     num_elements = 8
    !!     num_nodes = 9
    subroutine test_calculate_mesh_8_2_8_9(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(calculate_mesh_inputs) :: inputs
        type(calculate_mesh_expected_ouputs) :: expected_outputs
        ! Setup inputs
        inputs%num_boundary_nodes = 8
        inputs%num_edges_per_boundary = 2
        inputs%num_elements = 8
        inputs%num_nodes = 9

        ! Setup expected outputs
        allocate(expected_outputs%boundary_edges(3, inputs%num_nodes))
        expected_outputs%boundary_edges(:,1) = (/1,2,1/)
        expected_outputs%boundary_edges(:,2) = (/2,3,2/)
        expected_outputs%boundary_edges(:,3) = (/3,6,2/)
        expected_outputs%boundary_edges(:,4) = (/6,9,6/)
        expected_outputs%boundary_edges(:,5) = (/9,8,8/)
        expected_outputs%boundary_edges(:,6) = (/8,7,7/)
        expected_outputs%boundary_edges(:,7) = (/7,4,7/)
        expected_outputs%boundary_edges(:,8) = (/4,1,3/)
        allocate(expected_outputs%elements(3, inputs%num_elements))
        expected_outputs%elements(:,1) = (/1,2,5/)
        expected_outputs%elements(:,2) = (/2,3,6/)
        expected_outputs%elements(:,3) = (/1,5,4/)
        expected_outputs%elements(:,4) = (/2,6,5/)
        expected_outputs%elements(:,5) = (/4,5,8/)
        expected_outputs%elements(:,6) = (/5,6,9/)
        expected_outputs%elements(:,7) = (/4,8,7/)
        expected_outputs%elements(:,8) = (/5,9,8/)
        allocate(expected_outputs%nodes(3, inputs%num_nodes))
        expected_outputs%nodes(:,1) = (/1.0,1.0/)
        expected_outputs%nodes(:,2) = (/1.0,2.0/)
        expected_outputs%nodes(:,3) = (/1.0,3.0/)
        expected_outputs%nodes(:,4) = (/2.0,1.0/)
        expected_outputs%nodes(:,5) = (/2.0,2.0/)
        expected_outputs%nodes(:,6) = (/2.0,3.0/)
        expected_outputs%nodes(:,7) = (/3.0,1.0/)
        expected_outputs%nodes(:,8) = (/3.0,2.0/)
        expected_outputs%nodes(:,9) = (/3.0,3.0/)

        ! Call parent test
        call verify_calculate_mesh(error, inputs, expected_outputs)
        
        ! Teardown
        deallocate(expected_outputs%boundary_edges)
        deallocate(expected_outputs%elements)
        deallocate(expected_outputs%nodes)
    end subroutine test_calculate_mesh_8_2_8_9


    !> A unit test to demonstrate test-drives ability to skip a test
    !! 
    !! @param error - An allocatable error_type to track failing tests.
    subroutine test_skip_example(error)
        type(error_type), allocatable, intent(out) :: error
        call skip_test(error, "This feature is not implemented yet")
        return
    end subroutine test_skip_example
end module test_mesh_generator
