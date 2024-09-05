program main
    use poisson

    implicit none

    integer       :: boundary_index(mxp), element_to_node(3,mxe),    &
                     vb_index(mxe), boundary_node_num(2,mxb), num_side_nodes(4,mxb)
    real          :: coordinates(2, mxp), nodal_value_of_f(mxp), rhs_vector(mxp), beta(mxp), f_increment(mxp), &
                     vb1(mxc), vb2(mxc), vb(3,mxc), element_stiffness(6,mxe),             &
                     pre_conditioning_matrix(mxp)
    integer       :: fname_io = 100, fname_out_io = 101
    character*120 :: input_fname, output_fname

    !!
    !! *** Asks for I/O file names
    !!
    write(*,'(10(/),7(a,/),//)')                    &
        '        *********************************', &
        '        ***                           ***', &
        '        ***    *  P O I S S O N  *    ***', &
        '        ***                           ***', &
        '        ***  PCG-FEM HEAT CONDUCTION  ***', &
        '        ***                           ***', &
        '        *********************************' 
    input_fname = textread(' Enter input file name : ')
    output_fname = trim(input_fname)//'.out'
    call open_file(input_fname, 'old', fname_io)
    call open_file(output_fname, 'new', fname_out_io)

    !!
    !! *** Reads the triangular mesh and problem constants: Kx,Ky,Q,fp,q
    !!
    call inp(element_to_node,vb_index,coordinates,boundary_node_num,num_side_nodes,vb,vb1,vb2,fname_io)

    !!
    !! *** Assembles and solves the system of equations
    !!
    call pcg(element_to_node,vb_index,coordinates,nodal_value_of_f,boundary_node_num,num_side_nodes,vb,vb1,vb2,element_stiffness,rhs_vector,beta,f_increment,boundary_index,pre_conditioning_matrix)

    !!
    !! *** Writes the computed solution
    !!
    call out(element_to_node,coordinates,nodal_value_of_f,fname_out_io)
end program main
