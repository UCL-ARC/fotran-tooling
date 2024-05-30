c*-----------------------------------------------------------------------------*
c*                                                                             *
c*                      *** Program POISSON ***                                *
c*                                                                             *
c*   This is a program for the solution of the steady-state heat conduction    *
c*   equation represented by the Poisson equation:                             *
c*                                                                             *
c*                (Kx f,x),x + (Ky f,y),y + Q = 0     in D                     *
c*                                                                             *
c*    + b.c.          f = fp               (1)                                 *
c*                    Kx f,x + Ky f,y = q  (2)                                 *
c*                                                                             *
c*    It uses a finite element Galerkin discretization on a triangular mesh.   *
c*    The solution is assumed to vary linearly within an element. The values   *
c*    of the unknown (f) are stored at the nodes. The resulting system of      *
c*    equations is solved by means of a Preconditioned Conjugate Gradient      *
c*    -PCG- with diagonal preconditioning.                                     *
c*                                                                             *
c*    Version:   February 95                                                   *
c*    Author:    Joaquim Peiro                                                 *
c*               Department of Aeronautics                                     *
c*               Imperial College of Science, Technology and Medicine          *
c*               Prince Consort Road                                           *
c*               LONDON SW7 2BY   (U.K.)                                       *
c*               e-mail: j.peiro@ic.ac.uk                                      *
c*                                                                             *
c*-----------------------------------------------------------------------------*
c*                                                                             *
c*    INPUT DATA:                                                              *
c*                                                                             *
c*    ne .................... No of triangular elements in the mesh.           *
c*    np .................... No of nodes (or points) in the mesh.             *
c*    nb .................... No of boundary points (same as boundary sides)   *
c*    mb .................... No of sets (Kx,Ky,Q) used. The parameters        *
c*                            (Kx,Ky,Q) are assumed to be constant for an      *
c*                            element, but can change from element to element. *
c*                            The values (Kx,Ky,Q) are stored in "vb".         *
c*    mb1 ................... No of Dirichlet type boundary conditions:        *
c*                                   f = fp               (1)                  *
c*                            The prescribed values (fp) are stored in "vb1".  *
c*    mb2 ................... No of Neumann type boundary conditions:          *
c*                                   Kx f,x + Ky f,y = q  (2)                  *
c*                            (Kx,Ky) are obtained from the element adjacent   *
c*                            to a boundary side. The values of the prescribed *
c*                            heat flux (q) are stored in "vb2".               *
c*    co(1:2,1:np) .......... (x,y) coordinates of the nodes.                  *
c*    lm(1:3,1:ne) .......... (I1,I2,I3) Element to node connectivity array.   *
c*                            I1,I2,I3 are the numbers of the nodes forming    *
c*                            the triangular element. Using the right-hand     *
c*                            rule, the circulation I1->I2->I3 is in the       *
c*                            z-direction:                                     *
c*                                                                             *
c*                                           I3             y                  *
c*                                          *              ^                   *
c*                                         / \             |                   *
c*                                        /   \            +--> x              *
c*                                    I1 *-----* I2                            *
c*                                         -->                                 *
c*                                                                             *
c*    ki(1:ne) .............. Points to the index in "vb2" where the element   *
c*                            values of the parameters Kx,Ky,Q are stored      *
c*    lb(1:2,1:nb) .......... (I1,I2) I1 is the boundary node number. I2 is    *
c*                            either 0 (no Dirichlet BC) or a pointer (I2>0)   *
c*                            to the position in "vb1" containing prescribed   *
c*                            value of (fp).                                   *
c*    ls(1:4,1:nb) .......... (I1,I2,IE,IK) I1,I2 are the numbers of the nodes *
c*                            on the side. IE is the element containing the    *
c*                            side. IK is either 0 (no Neumann BC) or a        *
c*                            pointer (IK > 0) at the position in "vb2" that   *
c*                            contains the prescribed value of (q). The values *
c*                            (Kx,Ky) are those corresponding to element IE.   *
c*                                                                             *
c*    FEM VARIABLES:                                                           *
c*                                                                             *
c*    un(1:np) .............. Nodal values of f.                               *
c*    sm(1:6,1:ne) .......... Stores the entries of the element stiffness      *
c*                            matrix (k11,k12,k13,k22,k23,k33).                *
c*                                                                             *
c*    PCG VARIABLES:                                                           *
c*                                                                             *
c*    pc(1:np) .............. Diagonal preconditioning matrix.                 *
c*    rh(1:np) .............. Right-hand side (RHS) vector.                    *
c*    du(1:np) .............. Computed increment of f.                         *
c*    ik(1:np) .............. Index to set du(ip) = 0 for Dirichlet boundary   *
c*                            conditions.                                      *
c*    b(1:np) ............... Stores beta as in Hestenes-Stiefel relation.     *
c*                                                                             *
c*-----------------------------------------------------------------------------*
c
      program poisson
c
      parameter ( mxp  = 10000 )
      parameter ( mxe  = 30000 )
      parameter ( mxb  =  5000 )
      parameter ( mxc  =   100 )
c
      dimension  co(2,mxp),    un(mxp),    rh(mxp),     b(mxp)
      dimension    du(mxp),    ik(mxp),    pc(mxp)
      dimension  lm(3,mxe),    ki(mxe),  sm(6,mxe)
      dimension  lb(2,mxb),  ls(4,mxb)
      dimension   vb1(mxc),   vb2(mxc),  vb(3,mxc)
c
      common /data/ np,ne,nb,mb,mb1,mb2
      common /dime/ MXPOI,MXELE,MXBOU,MXBOC
      character*120 textread, fname
c
c *** Stores variables for dimension checking
c
      MXPOI = mxp
      MXELE = mxe
      MXBOU = mxb
      MXBOC = mxc
c
c *** Asks for I/O file names
c
      ierr = 0
      write(*,'(10(/),7(a,/),//)')
     &         '        *********************************',
     &         '        ***                           ***',
     &         '        ***    *  P O I S S O N  *    ***',
     &         '        ***                           ***',
     &         '        ***  PCG-FEM HEAT CONDUCTION  ***',
     &         '        ***                           ***',
     &         '        *********************************'
  100 continue
      if( ierr .eq. 1) then
        write(*,'(a)') ' *** Error when opening '//fname(1:l)
      else if( ierr .eq. 2) then
        write(*,'(a)') ' *** Error when opening '//fname(1:l)//'.out'
      endif
      ierr = 0
      fname = textread(' Enter input file name : ')
      l     = namlen(fname)
      ierr  = 1
      open(unit   = 11, 
     &     file   = fname(1:l),
     &     status = 'old',
     &     err    = 100)
      write(*,'(/,a)') ' *** '//fname(1:l)//' opened'
      ierr = 2
      open(unit   = 22,
     &     file   = fname(1:l)//'.out',
     &     status = 'new',
     &     err    = 100)
c
c *** Reads the triangular mesh and problem constants: Kx,Ky,Q,fp,q
c
      call inp(lm,ki,co,lb,ls,vb,vb1,vb2)
c
c *** Assembles and solves the system of equations
c
      call pcg(lm,ki,co,un,lb,ls,vb,vb1,vb2,sm,rh,b,du,ik,pc)
c
c *** Writes the computed solution
c
      call out(lm,co,un)
c
      stop
      end     
c*-----------------------------------------------------------------------------*
c*                                                                             *
c*    "inp" reads the input data: triangular mesh and problem parameters.      *
c*                                                                             *
c*-----------------------------------------------------------------------------*
      subroutine inp(lm,ki,co,lb,ls,vb,vb1,vb2)
c
      dimension lm(3,*),co(2,*),lb(2,*),ls(4,*)
      dimension ki(*),vb1(*),vb2(*),vb(3,*)
      common /data/ np,ne,nb,mb,mb1,mb2
      common /dime/ MXPOI,MXELE,MXBOU,MXBOC
      character*80 text
c
      read(11,'(a)') text
      read(11,*) np,ne,nb,mb,mb1,mb2
c
c *** Check dimensions
c
      icheck = 0
      if( np .gt. MXPOI ) then
        write(*,'(a,i6)') ' *** Increase mxp to: ',np
        icheck = 1
      endif
      if( ne .gt. MXELE ) then
        write(*,'(a,i6)') ' *** Increase mxe to: ',ne
        icheck = 1
      endif
      if( nb .gt. MXBOU ) then
        write(*,'(a,i6)') ' *** Increase mxb to: ',nb
        icheck = 1
      endif
      mx = max(mb,mb1,mb2)
      if( mx .gt. MXBOC ) then  
        write(*,'(a,i6)') ' *** Increase mxc to: ',mx
        icheck = 1
      endif
      if( icheck .eq. 1 ) STOP 
c
c *** Reads (Kx,Ky,Q) sets 
c
      read(11,'(a)') text
      do 50 ib=1,mb
      read(11,*) jb,vb(1,jb),vb(2,jb),vb(3,jb)
   50 continue
c
c *** Reads (fp) sets 
c
      read(11,'(a)') text
      do 100 ib=1,mb1
      read(11,*) jb,vb1(jb)
  100 continue
c
c *** Reads (q) sets 
c
      read(11,'(a)') text
      do 200 ib=1,mb2
      read(11,*) jb,vb2(jb)
  200 continue
c
c *** Reads coordinates 
c
      read(11,'(a)') text
      do 300 ip=1,np
      read(11,*) jp,co(1,jp),co(2,jp)
  300 continue
c
c *** Reads element-to-node array + index to (Kx,Ky,Q) set
c 
      read(11,'(a)') text
      do 400 ie=1,ne
      read(11,*) je,lm(1,je),lm(2,je),lm(3,je),ki(je)
  400 continue
c
c *** Boundary points: I1,I2 = lb(1:2,1:nb)
c
c     - I1 ......... Number of the boundary point
c     - I2 ......... I2 = 0 No Dirichlet BC is applied
c                    I2 > 0 I2 points at the position in "vb1" containing 
c                    the prescribed value of (fp).
c 
      read(11,'(a)') text
      do 500 ib=1,nb
      read(11,*) lb(1,ib),lb(2,ib)
  500 continue
c
c *** Boundary sides: I1,I2,IE,IK = ls(1:4,1:nb)
c
c     - (I1,I2) .... Numbers of the nodes on the side
c     - IE ......... Element containing the side
c     - IK ......... IK = 0 No Neumann BC is applied. 
c                    IK > 0 IK Points at the position in "vb2" containing 
c                    the prescribed value of (q). The values (Kx,Ky) are 
c                    those corresponding to element IE.
c
      read(11,'(a)') text
      do 600 ib=1,nb
      read(11,*) ls(1,ib),ls(2,ib),ls(3,ib),ls(4,ib)
  600 continue
c
      return
      end    
c*-----------------------------------------------------------------------------*
c*                                                                             *
c*    "pcg" solves the system "K x = y" by a preconditioned conjugate gradient *
c*    method.                                                                  *
c*                                                                             *
c*-----------------------------------------------------------------------------*
      subroutine pcg(lm,ki,co,un,lb,ls,vb,vb1,vb2,sm,rh,b,du,ik,pc)
c
      parameter (eps = 1.e-04)
c
      dimension rh(*),b(*),du(*),ik(*),pc(*)
      dimension sm(6,*),lm(3,*),co(2,*),un(*),ki(*)
      dimension lb(2,*),ls(4,*),vb(3,*),vb1(*),vb2(*)
      common /data/ np,ne,nb,mb,mb1,mb2
c
      tol = eps*eps
      nit = 10*np
c
c *** Initial guess for the solution vector un
c
      do 100 ip=1,np
      ik(ip) = 1
      un(ip) = 0.0
      du(ip) = 0.0
      pc(ip) = 0.0
      rh(ip) = 0.0
  100 continue
c
c *** Dirichlet type b.c.
c
      do 200 ib=1,nb
      in = lb(2,ib)
      if(in .gt. 0) then
        ip     = lb(1,ib)
        va     = vb1(in)
        un(ip) = va
        ik(ip) = 0
      endif
  200 continue
c
      do 300 ie=1,ne
      ip1      = lm(1,ie)
      ip2      = lm(2,ie)
      ip3      = lm(3,ie)
      ix       = ki(ie)
      akx      = vb(1,ix)
      aky      = vb(2,ix)
      qq       = vb(3,ix)
      x21      = co(1,ip2)-co(1,ip1)
      y21      = co(2,ip2)-co(2,ip1)
      x31      = co(1,ip3)-co(1,ip1)
      y31      = co(2,ip3)-co(2,ip1)
      ar       = x21*y31-x31*y21
      a1       = 0.5/ar
      ar       = 0.5*ar
      s1x      = -y31+y21
      s1y      =  x31-x21
      s2x      =  y31
      s2y      = -x31
      s3x      = -y21
      s3y      =  x21
c
c *** Stiffness matrix
c
      sm(1,ie) = a1*( akx*s1x*s1x + aky*s1y*s1y )     
      sm(2,ie) = a1*( akx*s2x*s2x + aky*s2y*s2y )
      sm(3,ie) = a1*( akx*s3x*s3x + aky*s3y*s3y )
      sm(4,ie) = a1*( akx*s1x*s2x + aky*s1y*s2y )
      sm(5,ie) = a1*( akx*s2x*s3x + aky*s2y*s3y )
      sm(6,ie) = a1*( akx*s1x*s3x + aky*s1y*s3y )
      qa       = 0.3333333*qq*ar
c
c *** RHS
c
      rh(ip1)  = rh(ip1)+qa
      rh(ip2)  = rh(ip2)+qa
      rh(ip3)  = rh(ip3)+qa
c
c *** Diagonal preconditioner (Mass lumping)
c
      pc(ip1)  = pc(ip1)+sm(1,ie)
      pc(ip2)  = pc(ip2)+sm(2,ie)
      pc(ip3)  = pc(ip3)+sm(3,ie)
  300 continue
c
c *** Boundary contribution to the RHS.
c
      do 400 ib=1,nb
      in = ls(4,ib)
      if(in .gt. 0) then
        ip1     = ls(1,ib)
        ip2     = ls(2,ib)
        qb      = vb2(in)
        x21     = co(1,ip2)-co(1,ip1)
        y21     = co(2,ip2)-co(2,ip1)
        al      = sqrt(x21*x21+y21*y21)
        qb      = qb*al*0.5
        rh(ip1) = rh(ip1)-qb
        rh(ip2) = rh(ip2)-qb
      endif
  400 continue
c
c *** Calculate first residual rh = rh-sm*un
c
      do 500 ie=1,ne
      ip1     = lm(1,ie)
      ip2     = lm(2,ie)
      ip3     = lm(3,ie)
      d1      = un(ip1)
      d2      = un(ip2)
      d3      = un(ip3)
      f1      = sm(1,ie)*d1+sm(4,ie)*d2+sm(6,ie)*d3
      f2      = sm(4,ie)*d1+sm(2,ie)*d2+sm(5,ie)*d3
      f3      = sm(6,ie)*d1+sm(5,ie)*d2+sm(3,ie)*d3
      rh(ip1) = rh(ip1)-f1
      rh(ip2) = rh(ip2)-f2
      rh(ip3) = rh(ip3)-f3
  500 continue
      rh0 = 0.0
      do 550 ip=1,np
      rh0 = rh0 + rh(ip)*rh(ip)
  550 continue
c
c *** Solution of sm*un = rh  by PCG.
c
      beta = 0.0
      energy_old = 1.0
      do 600 ip=1,np
      pc(ip) = 1./pc(ip)
  600 continue
c 
c *** Iteration procedure 
c
      do 2000 it=1,nit
c
c *** Solves p*b=rh and obtains beta using Hestenes-Stiefel relation.
c     Note: ik(ip)=0 takes care of the Dirichlet boundary conditions.
c
      energy = 0.0
      do 700 ip=1,np
      b(ip)  = rh(ip)*pc(ip)*ik(ip)
      energy = energy+b(ip)*rh(ip)
  700 continue
      beta       = energy/energy_old
      energy_old = energy
c
c *** Updates d (i.e. du=du*beta+b) and evaluates a1=du*rh.
c     As it will be needed to compute eta, it saves rh in b. 
c
      a1 = 0.0
      do 800 ip=1,np
      du(ip) = beta*du(ip)+b(ip)
      b(ip)  = rh(ip)
      a1     = a1+du(ip)*rh(ip)
  800 continue
c
c *** Evaluates the new residuals using the formula rh=rh-sm*du, in this 
c     way we don't need to keep the RHS term of the linear equation.
c
      do 900 ie=1,ne
      ip1     = lm(1,ie)
      ip2     = lm(2,ie)
      ip3     = lm(3,ie)
      d1      = du(ip1)
      d2      = du(ip2)
      d3      = du(ip3)
      f1      = sm(1,ie)*du(ip1)+sm(4,ie)*du(ip2)+sm(6,ie)*du(ip3)
      f2      = sm(4,ie)*du(ip1)+sm(2,ie)*du(ip2)+sm(5,ie)*du(ip3)
      f3      = sm(6,ie)*du(ip1)+sm(5,ie)*du(ip2)+sm(3,ie)*du(ip3)
      rh(ip1) = rh(ip1)-f1
      rh(ip2) = rh(ip2)-f2
      rh(ip3) = rh(ip3)-f3
  900 continue
c
c *** Finally, evaluates the eta parameter of the line search
c     method and updates du and rh accordingly.
c
      a2 = 0.0
      do 1000 ip=1,np
      a2 = a2+du(ip)*(rh(ip)-b(ip))
 1000 continue
      eta = -a1/a2 
      res = 0.0
      do 1100 ip=1,np
      ad     = eta*du(ip)
      un(ip) = un(ip)+ad
      res    = res+ad*ad
      rh(ip) = eta*rh(ip)+(1.-eta)*b(ip)
 1100 continue
c
      if(res.le.rh0*tol) then
        write(*,'(a,i4)') ' *** PCG converged: iterations = ',it
        goto 3000
      endif
 2000 continue
c
c *** Number of iterations exceeded -> This Should NOT Happen !!
c
      write(*,'(a)') ' *** Warning: PCG iterations exceeded'
      return
 3000 continue
c
      return
      end
c*-----------------------------------------------------------------------------*
c*    "out" writes output results.                                             *
c*-----------------------------------------------------------------------------*
      subroutine out(lm,co,un)
c
      dimension co(2,*),lm(3,*),un(*)
      common /data/ np,ne,nb,mb,mb1,mb2
c
      z = 0.0
      write(22,'(3i5)') np,ne,3
      do 100 ip=1,np
      write(22,'(i5,8f10.5)') ip,co(1,ip),co(2,ip),z,z,un(ip),z,z,z
  100 continue
      do 200 ie=1,ne
      write(22,'(4i6)') ie,lm(1,ie),lm(2,ie),lm(3,ie)
  200 continue
c
      return
      end
c*------------------------------------------------------------*
c*                                                            *
c*             ***    UTILITY  ROUTINES    ***                *
c*                                                            *
c*------------------------------------------------------------*
      character*(*) function textread( prompt)
      character*(*) prompt
c
      l = len( textread)
      call cfill( textread, l, ' ')
  120 write(*,'(/,a,$)') prompt
      read(*,'(a)') textread
      l = namlen( textread) 
      if(l.gt.0) then
  201    if(textread(1:1).eq.' ') then
            do 200 i = 1,l-1
             textread(i:i) = textread((i+1):(i+1))
  200       continue
            goto 201
         endif
      else
         goto 120
      endif
      return
      end
c*------------------------------------------------------------*
      integer function namlen( name)
      character*(*) name
c
      l = len( name)
      namlen = l
      do 100 i = l,1,-1
       if( name(i:i).eq.' ') goto 100
       namlen = i
       goto 101
  100 continue
      namlen = 0
  101 return
      end
c*------------------------------------------------------------*
      subroutine cfill( a, n, c)
      character*(*) a, c
c
      la = len(a)
      if(n.gt.0) then
        m = min(n,la)
      else
        m = la
      endif
      do i = 1,m
        a(i:i) = c 
      enddo
      return
      end
