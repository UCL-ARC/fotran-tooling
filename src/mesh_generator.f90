program mesh_generator
    use, intrinsic :: iso_fortran_env
    implicit none 
    ! Command line arguments
    integer                       :: argl
    character(len=:), allocatable :: a
    integer(kind=int64)           :: box_size
    real(kind=real64)             :: edge_size

    ! Mesh variables
    integer :: num_nodes, num_elements, num_boundary_nodes, &
               num_sets, num_dirichlet_boundary_conditions,  &
               num_neumann_boundary_conditions, num_nodes_per_edge, & 
               num_edges_per_boundary, num_nodes_per_boundary, i, j, counter, bottom_left_node
    integer, dimension(:, :), allocatable :: elements, boundary_edges
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
    
    ! Calculate mesh 
    num_edges_per_boundary = floor(box_size / edge_size)
    num_nodes_per_boundary = num_edges_per_boundary + 1
    num_nodes = num_nodes_per_boundary**2
    num_boundary_nodes = (num_edges_per_boundary) * 4
    num_elements = 2 * (num_edges_per_boundary)**2

    write(*,*) "num_edges_per_boundary", num_edges_per_boundary
    write(*,*) "num_nodes_per_boundary", num_nodes_per_boundary
    write(*,*) "num_nodes", num_nodes
    write(*,*) "num_boundary_nodes", num_boundary_nodes
    write(*,*) "num_elements", num_elements

    ! Allocate arrays
    allocate(nodes(2, num_nodes))
    allocate(elements(3, num_elements))
    allocate(boundary_edges(3, num_boundary_nodes))

    counter = 1
    do i = 1, num_nodes_per_boundary
        do j = 1, num_nodes_per_boundary
            nodes(1, counter) = i
            nodes(2, counter) = j
            counter = counter + 1
        end do
    end do

    counter = 1
    do i = 1, num_edges_per_boundary
        do j = 1, num_edges_per_boundary
            bottom_left_node = j + (i - 1) * num_nodes_per_boundary

            elements(1, counter) = bottom_left_node
            elements(2, counter) = bottom_left_node + 1
            elements(3, counter) = bottom_left_node + 1 + num_nodes_per_boundary

            elements(1, counter + num_edges_per_boundary) = bottom_left_node
            elements(2, counter + num_edges_per_boundary) = bottom_left_node + num_nodes_per_boundary + 1
            elements(3, counter + num_edges_per_boundary) = bottom_left_node + num_nodes_per_boundary

            counter = counter + 1
        end do 
        counter = counter + num_edges_per_boundary
    end do

    ! If we are along the bottom boundary
    do i = 1, num_edges_per_boundary
        ! bottom boundary 
        boundary_edges(1, i) = i       ! left node
        boundary_edges(2, i) = i + 1   ! right node
        boundary_edges(3, i) = i*2 - 1 ! element

        ! right boundary
        boundary_edges(1, i + num_edges_per_boundary) = i       * num_nodes_per_boundary
        boundary_edges(2, i + num_edges_per_boundary) = (i + 1) * num_nodes_per_boundary
        boundary_edges(3, i + num_edges_per_boundary) = (2*i - 1) * num_edges_per_boundary

        ! top boundary
        boundary_edges(1, i + num_edges_per_boundary * 2) = num_nodes - i + 1
        boundary_edges(2, i + num_edges_per_boundary * 2) = num_nodes - i
        boundary_edges(2, i + num_edges_per_boundary * 2) = num_elements - i + 1

        ! left boundary
        boundary_edges(1, i + num_edges_per_boundary * 3) = (num_nodes_per_boundary - i) * num_nodes_per_boundary + 1
        boundary_edges(2, i + num_edges_per_boundary * 3) = (num_nodes_per_boundary - 1 - i) * num_nodes_per_boundary + 1
        boundary_edges(3, i + num_edges_per_boundary * 3) = num_elements - (num_edges_per_boundary - 1) - (2 * (i - 1) * num_edges_per_boundary)
    end do

    call write_mesh_to_file()

contains
    subroutine write_mesh_to_file()
        implicit none
        character*11 :: file_name
        integer :: file_io
        integer :: iostat
        integer :: i

        file_name = "square_mesh"
        file_io = 100

        ! Write outpout 
        open (unit=file_io,          &
        file=file_name,        &
        status="new",          &
        IOSTAT=iostat)
        
        if( iostat .ne. 0) then
            write(*,'(a)') ' *** Error when opening '//trim(file_name)
        else
            write(*,'(/,a)') ' *** '//trim(file_name)//' opened'
        end if
        
        write(file_io,*) "! num_nodes, num_elements, num_boundary_points, num_sets, num_dirichlet_boundary_conditions, num_neumann_boundary_conditions"
        write(file_io,*) num_nodes, num_elements, num_boundary_nodes, 1, 1, 0
        
        write(file_io,*) "! jb,vb(1,jb),vb(2,jb),vb(3,jb) - as many lines as num_sets"
        write(file_io,*) 1, 1, 1, 1
        
        write(file_io,*) "! jb,vb1(jb) - as many lines as num_dirichlet_boundary_conditions"
        write(file_io,*) 1, 0
        
        write(file_io,*) "! jb,vb2(jb) - as many lines as num_neumann_boundary_conditions"
        
        write(file_io,*) "! jp,coordinates(1,jp),coordinates(2,jp) - as many lines as num_nodes"
        do i = 1, num_nodes
            write(file_io,*) i, nodes(1, i), nodes(2, i)
        end do 
        
        write(file_io,*) "! je,element_to_node(1,je),element_to_node(2,je),element_to_node(3,je),vb_index(je) - as many lines as num_elements"
        do i = 1, num_elements
            write(file_io,*) i, elements(1, i), elements(2, i), elements(3, i), 1
        end do 
        
        write(file_io,*) "! boundary_node_num(1,ib),boundary_node_num(2,ib) - as many lines as num_boundary_points"
        do i = 1, num_boundary_nodes
            write(file_io,*) i, 1
        end do 
        
        write(file_io,*) "! num_side_nodes(1,ib),num_side_nodes(2,ib),num_side_nodes(3,ib),num_side_nodes(4,ib) - as many lines as num_boundary_points"
        do i = 1, num_boundary_nodes
            write(file_io,*) boundary_edges(1, i), boundary_edges(2, i), boundary_edges(3, i), 0
        end do 
    end subroutine write_mesh_to_file
        
end program mesh_generator