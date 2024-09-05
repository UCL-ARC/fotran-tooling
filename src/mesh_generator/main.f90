program main
    use, intrinsic :: iso_fortran_env
    use mesh_generator

    implicit none 

    ! Command line arguments
    integer                       :: argl
    character(len=:), allocatable :: a
    integer(kind=int64)           :: box_size
    real(kind=real64)             :: edge_size

    ! Mesh variables
    integer(kind=int64) :: num_nodes, num_elements, num_boundary_nodes, &
                           num_edges_per_boundary
    integer(kind=int64), dimension(:, :), allocatable :: elements, boundary_edges
    real(kind=real64), dimension(:, :), allocatable :: nodes

    ! Get command line args (Fortran 2003 standard)
    if (command_argument_count() > 0) then
        call get_command_argument(1, length=argl)
        allocate(character(argl) :: a)
        call get_command_argument(1, a)
        read(a,*) box_size
        call get_command_argument(2, a)
        read(a,*) edge_size
    end if

    ! Output start message
    write(*,'(A)') "Generating mesh using:"
    write(*,'(A,1I16)') "box size: ", box_size 
    write(*,*) " process: ", edge_size
    
    call calculate_mesh_parameters(box_size, edge_size, num_edges_per_boundary, num_nodes, num_boundary_nodes, num_elements)

    ! Allocate arrays
    allocate(nodes(2, num_nodes))
    allocate(elements(3, num_elements))
    allocate(boundary_edges(3, num_boundary_nodes))

    call calculate_mesh(num_edges_per_boundary, num_nodes, num_elements, num_boundary_nodes, nodes, elements, boundary_edges)
    
    call write_mesh_to_file(num_nodes, num_elements, num_boundary_nodes, nodes, elements, boundary_edges)
end program main
