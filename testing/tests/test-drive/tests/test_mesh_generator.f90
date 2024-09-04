module test_mesh_generator
    use, intrinsic :: iso_fortran_env
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use mesh_generator
    
    implicit none 

    real(kind=real64) :: test_threshold = 1e-06

    ! test_calculate_mesh_parameters
    type :: calculate_mesh_parameters_inputs
        real(kind=real64)   :: edge_size
        integer(kind=int64) :: box_size
    end type calculate_mesh_parameters_inputs
    type :: calculate_mesh_parameters_expected_ouputs
        integer(kind=int64) :: num_edges_per_boundary, num_nodes, num_boundary_nodes, num_elements
    end type calculate_mesh_parameters_expected_ouputs

    ! test_calculate_mesh
    type :: calculate_mesh_inputs
        integer(kind=int64) :: num_edges_per_boundary, num_nodes, num_elements, num_boundary_nodes
    end type calculate_mesh_inputs
    type :: calculate_mesh_expected_ouputs
    integer(kind=int64), dimension(:, :), allocatable :: elements
    integer(kind=int64), dimension(:, :), allocatable :: boundary_edges
    real(kind=real64), dimension(:, :), allocatable :: nodes
    end type calculate_mesh_expected_ouputs
contains
    !> Collect all exported unit tests
    subroutine collect_mesh_generator_testsuite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_calculate_mesh_parameters_5_1", test_calculate_mesh_parameters_5_1), &
            new_unittest("test_calculate_mesh_parameters_10_05", test_calculate_mesh_parameters_10_05), &
            new_unittest("test_calculate_mesh_8_2_8_9", test_calculate_mesh_8_2_8_9) &
            ]

    end subroutine collect_mesh_generator_testsuite

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

        call check(error, expected_outputs%num_edges_per_boundary, actual_num_edges_per_boundary)
        call check(error, expected_outputs%num_nodes,              actual_num_nodes)
        call check(error, expected_outputs%num_boundary_nodes,     actual_num_boundary_nodes)
        call check(error, expected_outputs%num_elements,           actual_num_elements)
        
        ! Catch test failure
        if (allocated(error)) return
    end subroutine

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
        expected_outputs%nodes(:,1) = (/1,1/)
        expected_outputs%nodes(:,2) = (/1,2/)
        expected_outputs%nodes(:,3) = (/1,3/)
        expected_outputs%nodes(:,4) = (/2,1/)
        expected_outputs%nodes(:,5) = (/2,2/)
        expected_outputs%nodes(:,6) = (/2,3/)
        expected_outputs%nodes(:,7) = (/3,1/)
        expected_outputs%nodes(:,8) = (/3,2/)
        expected_outputs%nodes(:,9) = (/3,3/)
        ! Call parent test
        call verify_calculate_mesh(error, inputs, expected_outputs)
    end subroutine test_calculate_mesh_8_2_8_9
    subroutine verify_calculate_mesh(error, inputs, expected_outputs)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(calculate_mesh_inputs), intent(in) :: inputs
        type(calculate_mesh_expected_ouputs), intent(in) :: expected_outputs

        integer(kind=int64), dimension(3, inputs%num_elements) :: actual_elements
        integer(kind=int64), dimension(3, inputs%num_boundary_nodes) :: actual_boundary_edges
        real(kind=real64), dimension(2, inputs%num_nodes) :: actual_nodes
        character*80 :: failure_message

        integer :: i, j

        call calculate_mesh(inputs%num_edges_per_boundary, inputs%num_nodes, inputs%num_elements, inputs%num_boundary_nodes, actual_nodes, actual_elements, actual_boundary_edges)

        do i = 1, inputs%num_elements
            do j = 1, 3
                write(failure_message,'(a,i1,a,i1,a,i1,a,i1)') "Unexpected value for elements(", j, ",", i, "), got ", actual_elements(j, i), " expected ", expected_outputs%elements(j, i)
                call check(error, expected_outputs%elements(j, i), actual_elements(j, i), failure_message)
                if (allocated(error)) return
            end do 
        end do

        do i = 1, inputs%num_boundary_nodes
            do j = 1, 3
                write(failure_message,'(a,i1,a,i1,a,i1,a,i1)') "Unexpected value for boundary_edges(", j, ",", i, "), got ", actual_boundary_edges(j, i), " expected ", expected_outputs%boundary_edges(j, i)
                call check(error, expected_outputs%boundary_edges(j, i), actual_boundary_edges(j, i), failure_message)
                if (allocated(error)) return
            end do
        end do

        do i = 1, inputs%num_nodes
            do j = 1, 2
                write(failure_message,'(a,i1,a,i1,a,i1,a,i1)') "Unexpected value for nodes(", j, ",", i, "), got ", actual_nodes(j, i), " expected ", expected_outputs%nodes(j, i)
                call check(error, expected_outputs%nodes(j, i), actual_nodes(j, i), failure_message)
                if (allocated(error)) return
            end do
        end do
        
    end subroutine verify_calculate_mesh
end module test_mesh_generator