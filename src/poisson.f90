!!----------------------------------------------------------------------------------------------------*
!!                                                                                                    *
!!                                   *** Program POISSON ***                                          *
!!                                                                                                    *
!!                This is a program for the solution of the steady-state heat conduction              *
!!                equation represented by the Poisson equation:                                       *
!!                                                                                                    *
!!                             (Kx f,x),x + (Ky f,y),y + Q = 0     in D                               *
!!                                                                                                    *
!!                 + b.c.       f = fp               (1)                                           *
!!                                 Kx f,x + Ky f,y = q  (2)                                           *
!!                                                                                                    *
!!                 It uses a finite element Galerkin discretization on a triangular mesh.             *
!!                 The solution is assumed to vary linearly within an element. The values             *
!!                 of the unknown (f) are stored at the nodes. The resulting system of                *
!!                 equations is solved by means of a Preconditioned Conjugate Gradient                *
!!                 -PCG- with diagonal preconditioning.                                               *
!!                                                                                                    *
!!                 Version:   February 95                                                             *
!!                 Author:    Joaquim Peiro                                                           *
!!                            Department of Aeronautics                                               *
!!                            Imperial College of Science, Technology and Medicine                    *
!!                            Prince Consort Road                                                     *
!!                            LONDON SW7 2BY   (U.K.)                                                 *
!!                            e-mail: j.peiro@ic.ac.uk                                                *
!!                                                                                                    *
!!----------------------------------------------------------------------------------------------------*
!!                                                                                                    *
!!    INPUT DATA:                                                                                     *
!!                                                                                                    *
!!    num_elements ................................. No of triangular elements in the mesh.           *
!!    num_nodes .................................... No of nodes (or points) in the mesh.             *
!!    num_boundary_points .......................... No of boundary points (same as boundary sides)   *
!!    num_sets ..................................... No of sets (Kx,Ky,Q) used. The parameters        *
!!                                                   (Kx,Ky,Q) are assumed to be constant for an      *
!!                                                   element, but can change from element to element. *
!!                                                   The values (Kx,Ky,Q) are stored in "vb".         *
!!    num_dirichlet_boundary_conditions ............ No of Dirichlet type boundary conditions:        *
!!                                                          f = fp               (1)                  *
!!                                                   The prescribed values (fp) are stored in "vb1".  *
!!    num_neumann_boundary_conditions .............. No of Neumann type boundary conditions:          *
!!                                                          Kx f,x + Ky f,y = q  (2)                  *
!!                                                   (Kx,Ky) are obtained from the element adjacent   *
!!                                                   to a boundary side. The values of the prescribed *
!!                                                   heat flux (q) are stored in "vb2".               *
!!    coordinates(1:2,1:num_nodes) ................. (x,y) coordinates of the nodes.                  *
!!    element_to_node(1:3,1:num_elements) .......... (I1,I2,I3) Element to node connectivity array.   *
!!                                                   I1,I2,I3 are the numbers of the nodes forming    *
!!                                                   the triangular element. Using the right-hand     *
!!                                                   rule, the circulation I1->I2->I3 is in the       *
!!                                                   z-direction:                                     *
!!                                                                                                    *
!!                                                                  I3             y                  *
!!                                                                 *              ^                   *
!!                                                                / \             |                   *
!!                                                               /   \            +--> x              *
!!                                                           I1 *-----* I2                            *
!!                                                                -->                                 *
!!                                                                                                    *
!!    vb2_index(1:num_elements) .................... Points to the index in "vb2" where the element   *
!!                                                   values of thez parameters Kx,Ky,Q are stored     *
!!    boundary_node_num(1:2,1:num_boundary_points) . (I1,I2) I1 is the boundary node number. I2 is    *
!!                                                   either 0 (no Dirichlet BC) or a pointer (I2>0)   *
!!                                                   to the position in "vb1" containing prescribed   *
!!                                                   value of (fp).                                   *
!!    num_side_nodes(1:4,1:num_boundary_points) .... (I1,I2,IE,IK) I1,I2 are the numbers of the nodes *
!!                                                   on the side. IE is the element containing the    *
!!                                                   side. IK is either 0 (no Neumann BC) or a        *
!!                                                   pointer (IK > 0) at the position in "vb2" that   *
!!                                                   contains the prescribed value of (q). The values *
!!                                                   (Kx,Ky) are those corresponding to element IE.   *
!!                                                                                                    *
!!    FEM VARIABLES:                                                                                  *
!!                                                                                                    *
!!    nodal_value_of_f(1:num_nodes) ................ Nodal values of f.                               *
!!    element_stiffness(1:6,1:num_elements) ........ Stores the entries of the element stiffness      *
!!                                                   matrix (k11,k12,k13,k22,k23,k33).                *
!!                                                                                                    *
!!    PCG VARIABLES:                                                                                  *
!!                                                                                                    *
!!    pre_conditioning_matrix(1:num_nodes) ......... Diagonal preconditioning matrix.                 *
!!    rhs_vector(1:num_nodes) ...................... Right-hand side (RHS) vector.                    *
!!    f_increment(1:num_nodes) ..................... Computed increment of f.                         *
!!    boundary_index(1:num_nodes) .................. Index to set f_increment(ip) = 0 for Dirichlet   *
!!                                                   boundary conditions.                             *
!!    b(1:num_nodes) ............................... Stores beta as in Hestenes-Stiefel relation.     *
!!                                                                                                    *
!!----------------------------------------------------------------------------------------------------*
module poisson
      implicit none

      integer, parameter :: mxp  = 10000
      integer, parameter :: mxe  = 30000 
      integer, parameter :: mxb  =  5000 
      integer, parameter :: mxc  =   100

      integer :: num_nodes, num_elements, num_boundary_points, num_sets, num_dirichlet_boundary_conditions, &
                 num_neumann_boundary_conditions

contains

      subroutine open_file(file_name, status, file_io)
            implicit none

            integer, intent(out) :: file_io
            character*120, intent(in) :: file_name
            character*3, intent(in) :: status

            integer :: iostat

            print *, file_name
            
            open (file_io,                  &
                  file=trim(file_name),     &
                  status=status,            &
                  IOSTAT=iostat)

            if( iostat .ne. 0) then
                  write(*,'(a)') ' *** Error when opening '//trim(file_name)
            else
                  write(*,'(/,a)') ' *** '//trim(file_name)//' opened'
            end if
      end subroutine open_file

      !!-----------------------------------------------------------------------------*
      !!                                                                             *
      !!    "inp" reads the input data: triangular mesh and problem parameters.      *
      !!                                                                             *
      !!-----------------------------------------------------------------------------*
      subroutine inp(element_to_node,vb2_index,coordinates,boundary_node_num,num_side_nodes,vb,vb1,vb2,file_io)
            implicit none

            integer, intent(out) :: element_to_node(3,mxp), vb2_index(mxe), coordinates(2, mxp), boundary_node_num(2,mxb), &
                                    num_side_nodes(4,mxb)
            real, intent(out)     :: vb(3,mxc), vb1(mxc), vb2(mxc)

            integer      :: file_io, mx, ib, ip, ie, jb, jp, je, icheck
            character*80 :: text

            read(file_io,'(a)') text
            read(file_io,*) num_nodes,num_elements,num_boundary_points,num_sets,num_dirichlet_boundary_conditions,num_neumann_boundary_conditions

            !!
            !! *** Check dimensions
            !!
            icheck = 0
            if( num_nodes .gt. mxp ) then
                  write(*,'(a,i6)') ' *** Increase mxp to: ',num_nodes
                  icheck = 1
            endif
            if( num_elements .gt. mxe ) then
                  write(*,'(a,i6)') ' *** Increase mxe to: ',num_elements
                  icheck = 1
            endif
            if( num_boundary_points .gt. mxb ) then
                  write(*,'(a,i6)') ' *** Increase mxb to: ',num_boundary_points
                  icheck = 1
            endif
            mx = max(num_sets,num_dirichlet_boundary_conditions,num_neumann_boundary_conditions)
            if( mx .gt. mxc ) then  
                  write(*,'(a,i6)') ' *** Increase mxc to: ',mx
                  icheck = 1
            endif
            if( icheck .eq. 1 ) STOP

            !!
            !! *** Reads (Kx,Ky,Q) sets 
            !!
            read(file_io,'(a)') text
            do ib=1,num_sets
                  read(file_io,*) jb,vb(1,jb),vb(2,jb),vb(3,jb)
            end do
            
            !!
            !! *** Reads (fp) sets 
            !!
            read(file_io,'(a)') text
            do ib=1,num_dirichlet_boundary_conditions
                  read(file_io,*) jb,vb1(jb)
            end do

            !!
            !! *** Reads (q) sets 
            !!
            read(file_io,'(a)') text
            do ib=1,num_neumann_boundary_conditions
                  read(file_io,*) jb,vb2(jb)
            end do
            
            !!
            !! *** Reads coordinates 
            !!
            read(file_io,'(a)') text
            do ip=1,num_nodes
                  read(file_io,*) jp,coordinates(1,jp),coordinates(2,jp)
            end do

            !!
            !! *** Reads element-to-node array + index to (Kx,Ky,Q) set
            !! 
            read(file_io,'(a)') text
            do ie=1,num_elements
                  read(file_io,*) je,element_to_node(1,je),element_to_node(2,je),element_to_node(3,je),vb2_index(je)
            end do 
            
            !!
            !! *** Boundary points: I1,I2 = boundary_node_num(1:2,1:num_boundary_points)
            !!
            !!     - I1 ......... Number of the boundary point
            !!     - I2 ......... I2 = 0 No Dirichlet BC is applied
            !!                    I2 > 0 I2 points at the position in "vb1" containing 
            !!                    the prescribed value of (fp).
            !! 
            read(file_io,'(a)') text
            do ib=1,num_boundary_points
                  read(file_io,*) boundary_node_num(1,ib),boundary_node_num(2,ib)
            end do 
            
            !!
            !! *** Boundary sides: I1,I2,IE,IK = num_side_nodes(1:4,1:num_boundary_points)
            !!
            !!     - (I1,I2) .... Numbers of the nodes on the side
            !!     - IE ......... Element containing the side
            !!     - IK ......... IK = 0 No Neumann BC is applied. 
            !!                    IK > 0 IK Points at the position in "vb2" containing 
            !!                    the prescribed value of (q). The values (Kx,Ky) are 
            !!                    those corresponding to element IE.
            !!
            read(file_io,'(a)') text
            do ib=1,num_boundary_points
                  read(file_io,*) num_side_nodes(1,ib),num_side_nodes(2,ib),num_side_nodes(3,ib),num_side_nodes(4,ib)
            end do 

      end subroutine inp

      !!-----------------------------------------------------------------------------*
      !!                                                                             *
      !!    "pcg" solves the system "K x = y" by a preconditioned conjugate gradient *
      !!    method.                                                                  *
      !!                                                                             *
      !!-----------------------------------------------------------------------------*
      subroutine pcg(element_to_node,vb2_index,coordinates,nodal_value_of_f,boundary_node_num,num_side_nodes,vb,vb1,vb2,element_stiffness,rhs_vector,b,f_increment,boundary_index,pre_conditioning_matrix)
            implicit none

            real, parameter :: eps = 1.e-04

            integer, intent(inout) :: element_to_node(3,mxp), vb2_index(mxe), coordinates(2, mxp), boundary_node_num(2,mxb),    &
                                    num_side_nodes(4,mxb), boundary_index(mxp)
            real, intent(inout)    :: nodal_value_of_f(mxp), rhs_vector(mxp), b(mxp), f_increment(mxp), vb(3,mxc), vb1(mxc), &
                                    vb2(mxc), element_stiffness(6,mxe), pre_conditioning_matrix(mxp)
            
            integer      :: file_io, mx, ib, ip, ie, jb, jp, je, icheck, nit, in, ip1, ip2, ip3, ix, it
            real         :: tol, va, akx, aky, qq, ar, a1, a2, qa, qb, x21, y21, x31, y31, s1x, s1y, s2x, &
                            s2y, s3x, s3y, al, d1, d2, d3, f1, f2, f3, rh0, beta, energy_old, energy,     &
                            eta, res, ad
            character*80 :: text
            logical :: is_converged
            
            tol = eps*eps
            nit = 10*num_nodes

            !!
            !! *** Initial guess for the solution vector nodal_value_of_f
            !!
            do ip=1,num_nodes
                  boundary_index(ip) = 1
                  nodal_value_of_f(ip) = 0.0
                  f_increment(ip) = 0.0
                  pre_conditioning_matrix(ip) = 0.0
                  rhs_vector(ip) = 0.0
            end do 
            
            !!
            !! *** Dirichlet type b.c.
            !!
            do ib=1,num_boundary_points
                  in = boundary_node_num(2,ib)
                  if(in .gt. 0) then
                        ip     = boundary_node_num(1,ib)
                        va     = vb1(in)
                        nodal_value_of_f(ip) = va
                        boundary_index(ip) = 0
                  endif
            end do 

            do ie=1,num_elements
                  ip1      = element_to_node(1,ie)
                  ip2      = element_to_node(2,ie)
                  ip3      = element_to_node(3,ie)
                  ix       = vb2_index(ie)
                  akx      = vb(1,ix)
                  aky      = vb(2,ix)
                  qq       = vb(3,ix)
                  x21      = coordinates(1,ip2)-coordinates(1,ip1)
                  y21      = coordinates(2,ip2)-coordinates(2,ip1)
                  x31      = coordinates(1,ip3)-coordinates(1,ip1)
                  y31      = coordinates(2,ip3)-coordinates(2,ip1)
                  ar       = x21*y31-x31*y21
                  a1       = 0.5/ar
                  ar       = 0.5*ar
                  s1x      = -y31+y21
                  s1y      =  x31-x21
                  s2x      =  y31
                  s2y      = -x31
                  s3x      = -y21
                  s3y      =  x21

                  !!
                  !! *** Stiffness matrix
                  !!
                  element_stiffness(1,ie) = a1*( akx*s1x*s1x + aky*s1y*s1y )     
                  element_stiffness(2,ie) = a1*( akx*s2x*s2x + aky*s2y*s2y )
                  element_stiffness(3,ie) = a1*( akx*s3x*s3x + aky*s3y*s3y )
                  element_stiffness(4,ie) = a1*( akx*s1x*s2x + aky*s1y*s2y )
                  element_stiffness(5,ie) = a1*( akx*s2x*s3x + aky*s2y*s3y )
                  element_stiffness(6,ie) = a1*( akx*s1x*s3x + aky*s1y*s3y )
                  qa       = 0.3333333*qq*ar

                  !!
                  !! *** RHS
                  !!
                  rhs_vector(ip1)  = rhs_vector(ip1)+qa
                  rhs_vector(ip2)  = rhs_vector(ip2)+qa
                  rhs_vector(ip3)  = rhs_vector(ip3)+qa

                  !!
                  !! *** Diagonal preconditioner (Mass lumping)
                  !!
                  pre_conditioning_matrix(ip1)  = pre_conditioning_matrix(ip1)+element_stiffness(1,ie)
                  pre_conditioning_matrix(ip2)  = pre_conditioning_matrix(ip2)+element_stiffness(2,ie)
                  pre_conditioning_matrix(ip3)  = pre_conditioning_matrix(ip3)+element_stiffness(3,ie)
            end do 

            !!
            !! *** Boundary contribution to the RHS.
            !!
            do ib=1,num_boundary_points
                  in = num_side_nodes(4,ib)
                  if(in .gt. 0) then
                        ip1     = num_side_nodes(1,ib)
                        ip2     = num_side_nodes(2,ib)
                        qb      = vb2(in)
                        x21     = coordinates(1,ip2)-coordinates(1,ip1)
                        y21     = coordinates(2,ip2)-coordinates(2,ip1)
                        al      = sqrt(x21*x21+y21*y21)
                        qb      = qb*al*0.5
                        rhs_vector(ip1) = rhs_vector(ip1)-qb
                        rhs_vector(ip2) = rhs_vector(ip2)-qb
                  endif
            end do 

            !!
            !! *** Calculate first residual rhs_vector = rhs_vector-element_stiffness*nodal_value_of_f
            !!
            do ie=1,num_elements
                  ip1     = element_to_node(1,ie)
                  ip2     = element_to_node(2,ie)
                  ip3     = element_to_node(3,ie)
                  d1      = nodal_value_of_f(ip1)
                  d2      = nodal_value_of_f(ip2)
                  d3      = nodal_value_of_f(ip3)
                  f1      = element_stiffness(1,ie)*d1+element_stiffness(4,ie)*d2+element_stiffness(6,ie)*d3
                  f2      = element_stiffness(4,ie)*d1+element_stiffness(2,ie)*d2+element_stiffness(5,ie)*d3
                  f3      = element_stiffness(6,ie)*d1+element_stiffness(5,ie)*d2+element_stiffness(3,ie)*d3
                  rhs_vector(ip1) = rhs_vector(ip1)-f1
                  rhs_vector(ip2) = rhs_vector(ip2)-f2
                  rhs_vector(ip3) = rhs_vector(ip3)-f3
            end do

            rh0 = 0.0
            do ip=1,num_nodes
                  rh0 = rh0 + rhs_vector(ip)*rhs_vector(ip)
            end do 

            !!
            !! *** Solution of element_stiffness*nodal_value_of_f = rhs_vector  by PCG.
            !!
            beta = 0.0
            energy_old = 1.0
            do ip=1,num_nodes
                  pre_conditioning_matrix(ip) = 1./pre_conditioning_matrix(ip)
            end do

            !! 
            !! *** Iteration procedure 
            !!
            nit_loop: do it=1,nit
                  !!
                  !! *** Solves p*b=rhs_vector and obtains beta using Hestenes-Stiefel relation.
                  !!     Note: boundary_index(ip)=0 takes care of the Dirichlet boundary conditions.
                  !!
                  energy = 0.0
                  do ip=1,num_nodes
                        b(ip)  = rhs_vector(ip)*pre_conditioning_matrix(ip)*boundary_index(ip)
                        energy = energy+b(ip)*rhs_vector(ip)
                  end do
                  beta       = energy/energy_old
                  energy_old = energy

                  !!
                  !! *** Updates d (i.e. f_increment=f_increment*beta+b) and evaluates a1=f_increment*rhs_vector.
                  !!     As it will be needed to compute eta, it saves rhs_vector in b. 
                  !!
                  a1 = 0.0
                  do ip=1,num_nodes
                        f_increment(ip) = beta*f_increment(ip)+b(ip)
                        b(ip)  = rhs_vector(ip)
                        a1     = a1+f_increment(ip)*rhs_vector(ip)
                  end do 

                  !!
                  !! *** Evaluates the new residuals using the formula rhs_vector=rhs_vector-element_stiffness*f_increment, in this 
                  !!     way we don't need to keep the RHS term of the linear equation.
                  !!
                  do ie=1,num_elements
                        ip1     = element_to_node(1,ie)
                        ip2     = element_to_node(2,ie)
                        ip3     = element_to_node(3,ie)
                        d1      = f_increment(ip1)
                        d2      = f_increment(ip2)
                        d3      = f_increment(ip3)
                        f1      = element_stiffness(1,ie)*f_increment(ip1)+element_stiffness(4,ie)*f_increment(ip2)+element_stiffness(6,ie)*f_increment(ip3)
                        f2      = element_stiffness(4,ie)*f_increment(ip1)+element_stiffness(2,ie)*f_increment(ip2)+element_stiffness(5,ie)*f_increment(ip3)
                        f3      = element_stiffness(6,ie)*f_increment(ip1)+element_stiffness(5,ie)*f_increment(ip2)+element_stiffness(3,ie)*f_increment(ip3)
                        rhs_vector(ip1) = rhs_vector(ip1)-f1
                        rhs_vector(ip2) = rhs_vector(ip2)-f2
                        rhs_vector(ip3) = rhs_vector(ip3)-f3
                  end do

                  !!
                  !! *** Finally, evaluates the eta parameter of the line search
                  !!     method and updates f_increment and rhs_vector accordingly.
                  !!
                  a2 = 0.0
                  do ip=1,num_nodes
                        a2 = a2+f_increment(ip)*(rhs_vector(ip)-b(ip))
                  end do
                  eta = -a1/a2 
                  res = 0.0
                  do ip=1,num_nodes
                        ad     = eta*f_increment(ip)
                        nodal_value_of_f(ip) = nodal_value_of_f(ip)+ad
                        res    = res+ad*ad
                        rhs_vector(ip) = eta*rhs_vector(ip)+(1.-eta)*b(ip)
                  end do

                  is_converged = res.le.rh0*tol
                  if(is_converged) then
                        write(*,'(a,i4)') ' *** PCG converged: iterations = ',it
                        exit
                  endif
            end do nit_loop

            !!
            !! *** Number of iterations exceeded -> This Should NOT Happen !!
            !!
            if(.not. is_converged) then
                  write(*,'(a)') ' *** Warning: PCG iterations exceeded'
            endif

      end subroutine pcg

      !!-----------------------------------------------------------------------------*
      !!    "out" writes output results.                                             *
      !!-----------------------------------------------------------------------------*
      subroutine out(element_to_node,coordinates,nodal_value_of_f,file_io)
            implicit none

            integer, intent(in) :: element_to_node(3,mxp), coordinates(2, mxp), file_io
            real, intent(in)    :: nodal_value_of_f(mxp)

            integer :: ip, ie
            real    :: z

            z = 0.0
            write(file_io,'(3i5)') num_nodes,num_elements,3
            do ip=1,num_nodes
                  write(file_io,'(i5,8f10.5)') ip,coordinates(1,ip),coordinates(2,ip),z,z,nodal_value_of_f(ip),z,z,z
            end do

            do ie=1,num_elements
                  write(file_io,'(4i6)') ie,element_to_node(1,ie),element_to_node(2,ie),element_to_node(3,ie)
            end do

            return
      end subroutine out

      !!------------------------------------------------------------*
      !!                                                            *
      !!             ***    UTILITY  ROUTINES    ***                *
      !!                                                            *
      !!------------------------------------------------------------*
      character*120 function textread(prompt) result(text_read_in)
            implicit none
            
            character*(*), intent(in) :: prompt
            

            integer :: l = 120

            text_read_in = ' '
            do 
                  write(*,'(/,a,$)') prompt
                  read(*,'(a)') text_read_in
                  l = len(trim(text_read_in))
                  if(l.gt.0) exit
            end do
      end function textread
end module poisson
