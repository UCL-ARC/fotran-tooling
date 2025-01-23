module garden_mesh_generator
    use, intrinsic :: iso_fortran_env
    use mesh_generator, only : calculate_mesh_parameters
    implicit none

contains
    function test_assert_eq_double_mat() result(tests)
        use garden, only: &
            test_item_t, &
            describe, &
            it
        implicit none 
        type(test_item_t) :: tests

        tests = describe( &
                "calculate_mesh_parameters", &
                [ it( &
                        "passes with valid inputs", &
                        check_calculate_mesh_parameters_valid_inputs) &
                ])
    end function

    function check_calculate_mesh_parameters_valid_inputs() result(result_)
        use garden, only: result_t, assert_equals
        implicit none

        type(result_t) :: result_

        integer(kind=int64)  :: box_size = 10
        real(kind=real64)    :: edge_size = 1.0
        integer(kind=int64)  :: actual_num_edges_per_boundary, actual_num_nodes, &
                                actual_num_boundary_nodes, actual_num_elements

        integer  :: &
            expected_num_edges_per_boundary = 10, &
            expected_num_nodes = 121, &
            expected_num_boundary_nodes = 40, &
            expected_num_elements = 200, &
            num_edges_per_boundary, & 
            num_nodes, & 
            num_boundary_nodes, & 
            num_elements

        call calculate_mesh_parameters(box_size, edge_size,      &
                actual_num_edges_per_boundary, actual_num_nodes, &
                actual_num_boundary_nodes, actual_num_elements)

        num_edges_per_boundary = actual_num_edges_per_boundary        
        num_nodes = actual_num_nodes
        num_boundary_nodes = actual_num_boundary_nodes
        num_elements = actual_num_elements

        result_ = &
            assert_equals(num_edges_per_boundary, expected_num_edges_per_boundary).and.&
            assert_equals(num_nodes, expected_num_nodes).and.&
            assert_equals(num_boundary_nodes, expected_num_boundary_nodes).and.&
            assert_equals(num_elements, expected_num_elements)

    end function
end module garden_mesh_generator
