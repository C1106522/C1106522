! Purpose: Create the hub geometry from an input file.

! Dates:                         Modifications:
! ========                       =======================
! 28/08/18                       Implementation of the bulb geometry
! 29/08/18			 Implementation of the pilar geometry
! 30/08/18                       Implementation of the tunnel geometry
! 01/09/18			 Implementation of the guide vane
! 05/09/18                       Implementation of the rotational guide vanes
! 30/10/18			 Fullness of tunnel fixed
! 11/11/18			 bulb_v2.1 implementation of the closing_face for the bulb
! 17/11/18			 bulb_v2.2 implementation of the donut on the bulb
! 29/11/18			 bulb_v2.3 implementation of the transitional part of the bulb
! 03/01/19			 bulb_v2.5 improvement of the donut shape and fullnes of bulb
! 04/01/19			 bulb_v2.6 improvement of the pilar geometry
! ========                       =======================
      
!==========================================================================================
      module hub_var
!==========================================================================================
      implicit none 
      
      ! Main ==============================================================================
      double precision,parameter :: pi=4.d0*atan(1.d0)
      integer :: i,ipart,totpts,nxn
      integer :: t,nthet,axe,inc
      double precision :: xval,yval,zval,rval
      double precision :: dx,dy,dz
      double precision :: alpha
      character(80) :: fn
      double precision :: dxi,dxe,dyi,dye,dzi,dze
      double precision,dimension(10,10) :: ec
      double precision,dimension(100) :: cr,angle,mpts
      !====================================================================================
      
      ! LMR ===============================================================================
      logical :: LRM
      integer :: nzone,temp_nparts,temp_nsect
      double precision,dimension(50) :: zone
      double precision,dimension(50,2) :: xlmr
      integer, dimension(50) :: lmr,temp_lmr,temp_geomtype
      double precision,dimension(50) :: temp_cxsect,temp_cysect,temp_czsect,temp_radsect
      double precision,dimension(50) :: temp_djc,temp_dkc,temp_dr
      double precision,dimension(50) :: temp_height,temp_base,temp_dh,temp_db
      double precision,dimension(10,2,200) :: temp_tran_sect,temp_tran_edge
      !====================================================================================
      
      ! TUNNEL GEOMETRY ===================================================================
      integer :: t_nsect,tti,ttj,ttk,tlay,t_nparts
      integer,dimension(50) :: itun,t_lmr
      double precision :: t_jc,t_kc,t_h,t_b
      double precision,dimension(10) :: cor_dis
      
      ! Tunnel case:
      double precision :: txi,txe,tyi,tye,tzi,tze
      double precision :: xitc,xetc,yitc,yetc,zitc,zetc
      double precision,dimension(10000000,3) :: casepts
      double precision,dimension(1000,2) :: t_vertice,vertice
      
      ! Inside tube:
      integer,dimension(50) :: t_sect,t_geomtype,t_scale
      double precision,dimension(50) :: t_cxsect,t_cysect,t_czsect,t_radsect,t_height,t_base
      double precision,dimension(50) :: t_dr,t_djc,t_dkc,t_dh,t_db
      double precision,dimension(10000000,3) :: tubpts
      
      ! Transistion part:
      double precision,dimension(50,2) :: edge
      double precision,dimension(50) :: t_ocxsect,t_ocysect,t_oczsect,t_oheight,t_obase
      double precision,dimension(10,2,200) :: tran_sect,tran_edge
      double precision,dimension(20,20) :: loc
      logical :: fullness
      !====================================================================================
      
      ! BULB PART =========================================================================
      ! Bulb Case:
      integer :: sec,b_nsect,nlin,b_nparts,blay
      integer,dimension(50) :: b_sect,b_geomtype,ip,b_lmr
      double precision,dimension(50) :: b_cxsect,b_cysect,b_czsect,b_radsect
      double precision,dimension(50,4) :: bezpts
      double precision,dimension(50) :: b_dr,b_djc,b_dkc
      double precision,dimension(1000000,3,20) :: bulpts
      logical :: b_fullness
      
      !Bulb donut:
      integer :: ndonut,itime,nbez
      integer, dimension(10) :: donut
      double precision,dimension(10) :: bsize
      double precision,dimension(10,2) :: d_cxsect,d_radsect
      !====================================================================================

      ! Pilar Case ========================================================================
      integer :: ipi,npil,play
      integer,dimension(50) :: p_type,orientation
      double precision,dimension(50) :: p_nsect,p_nparts,p_ax,p_by!,axpil,bypil,cxpil,cypil,czpil
      double precision,dimension(50,50) :: p_sect,p_geomtype,p_cxsect,p_cysect,p_radsect
      double precision,dimension(50,50) :: p_djc,p_dkc,p_dr
      integer,dimension(50,50) :: p_lmr,ipil
      double precision,dimension(100000,10,2) :: pilpts,xform,yform
      double precision,dimension(100000,10,2) :: nznpil,zipil,zepil,xpil,ypil,zpil
      logical :: p_fullness     
      !====================================================================================

      ! Guide Vanes =======================================================================
      integer :: nguide
      integer,dimension(10) :: gnsect,gnblade,ipg
      integer,dimension(10,10) :: gsect
      double precision,dimension(10) :: gcxloc,gcyloc,gczloc
      double precision,dimension(10,10) :: gchord,gmc,gmcl,gthick,gtwist,grloc
      double precision,dimension(10,10) :: d_c,d_mc,d_mcl,d_thick,d_twist
      double precision,dimension(1000000,10) :: xnaca,ynaca,znaca 
      double precision,dimension(1000000,10) :: xntop,yntop,zntop
      double precision,dimension(1000000,10) :: xnbot,ynbot,znbot
      double precision,dimension(1000000) :: clth
      double precision :: dxx,dyy,dzz     
      !===================================================================================
      
      end module
!=========================================================================================



!#########################################################################################
!#########################################################################################
      program hub_generation
      use hub_var
      implicit none 
      
      integer :: n,np,ng,ipts,iz,maxpts,maxsec
      double precision :: rec_h,rec_l
      

      call read_infile
       
!      call screen_display
      call tunnel_main

      call bulb_main
      
      ! Creating the geometry points for the pilars
      ! Calulate the range of the matrix required
!      do ipi=1,npil
!        rec_h=((p_cysect(p_nsect(ipi),ipi)+2*p_radsect(p_nsect(ipi),ipi))-(p_cysect(1,ipi)+2*p_radsect(1,ipi)))/
!        rec_l=(p_cxsect(p_nsect(ipi),ipi)-p_cxsect(1,ipi))
!	WRITE(6,'(a,2F10.5)') 'rec_h,rec_l:',rec_h,rec_l
!        mpts(ipi)=(tze-tzi)/2*rec_h*rec_l
!      end do
!      maxpts=nint(maxval(mpts)) ; maxsec=nint(maxval(p_nsect))
!      WRITE(6,'(a,2I5)') 'maxpts,maxsec:',maxpts,maxsec 
!      allocate(xform(maxpts,maxsec,npil),yform(maxpts,maxsec,npil))
!      allocate(nznpil(maxpts,maxsec,npil),zipil(maxpts,maxsec,npil),zepil(maxpts,maxsec,npil))
!     allocate(xpil(maxpts,maxsec,npil),ypil(maxpts,maxsec,npil),zpil(maxpts,maxsec,npil))

      do ipi=1,npil
	call pilar_main
      end do

      ! Create the guide vanes geometry points
      do ng=1,nguide
        call naca_blades(ng)
      end do
           
      ! Printing on the screen the total number of points
      totpts=0
      do ipart=1,b_nparts
	totpts=totpts+ip(ipart)
      end do
      write(6,*) '-----------------------------------------------------------------------'
      write(6,'(X,a,I9)') 'Total Geometry Points:', totpts
      write(6,*) '-----------------------------------------------------------------------'


      !-----------------------------------------------------------------------------------
      ! OUPUT FILES
      ! ----------------------------------------------------------------------------------
      if(t_nsect.ne.0) then
!      print *, 'Enter name of the tunnel file:'
!      read *,fn
      fn='hope1.dat'
      open(unit=19,file=fn)
	if(fullness.eq..FALSE.) write(19,*) 'ZONE T=TCASE'
	do i=1,itun(t_nsect)
          write(19,112) casepts(i,1),casepts(i,2),casepts(i,3)
        end do
!	if(fullness.eq..FALSE.) write(19,*) 'ZONE T=TTUN'
        ipts=0
	do n=1,t_nsect-1
!	  write(19,'(a,I1)') 'ZONE T=T_part',n
	  do i=1,itun(n)
	    ipts=ipts+1
            write(19,112) tubpts(ipts,1),tubpts(ipts,2),tubpts(ipts,3)
	  end do
	end do
      close(19)
      end if
      
      if(b_nsect.ne.0) then
      open(unit=20,file='bulb.dat')
      write(20,*) 'ZONE T=BULB'
	ipts=0
	Do ipart=1,b_nparts
	  WRITE(6,'(a,I5)') 'ip:',ip(ipart) 
          do i=1,ip(ipart)
	  ipts=ipts+1
          write(20,112) bulpts(ipts,1,ipart),bulpts(ipts,2,ipart),bulpts(ipts,3,ipart)
	  end do
	End do
      close(20)
      end if

      if(npil.ne.0) then
      open(unit=21,file='pilar.dat')
        write(21,*) 'ZONE T=PILART'
        Do ipi=1,npil ; do n=1,p_nsect(ipi)
	  ipts=0
	  do i=1,ipil(n,ipi) ; do iz=1,nznpil(i,n,ipi)
	    ipts=ipts+1
	    WRITE(21,112) xpil(ipts,n,ipi),ypil(ipts,n,ipi),zpil(ipts,n,ipi)
	  end do ; end do
	  WRITE(6,'(a,I5)') 'ipts pilat.dat:',ipts
        End do ; end do
      close(21)
      end if

      if(nguide.ne.0) then
      open(unit=22,file='nacablade.dat')
      write(22,*) 'ZONE T=NACA'
      do ipart=1,nguide
	print *, 'ipg(ipart):', ipg(ipart)
        do i=1,ipg(ipart)
          write(22,112) xnaca(i,ipart),ynaca(i,ipart),znaca(i,ipart)
	end do
      end do 
      close(22)
      end if

      112 format(3F25.7)

      end program
!#########################################################################################
!#########################################################################################





!LMR RESOLUTION
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine lmr_resolution(cas)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      integer, intent(in) :: cas
      integer :: n
      
      
      temp_cxsect    = 0 ; temp_radsect   = 0 ; temp_nparts = 0
      temp_cysect    = 0 ; temp_djc	  = 0 ; temp_nsect  = 0
      temp_czsect    = 0 ; temp_dkc	  = 0 ; temp_lmr    = 0
      temp_dr	     = 0 ; temp_dh	  = 0 ; temp_db     = 0
      temp_height    = 0 ; temp_base      = 0 ;
      temp_geomtype  = 0 ; temp_tran_edge = 0 ;
      temp_tran_sect = 0 

      Select case(cas)
      Case (1)
      temp_cxsect    = t_cxsect   ; temp_radsect   = t_radsect	; temp_nparts = t_nparts
      temp_cysect    = t_cysect   ; temp_djc	   = t_djc	; temp_nsect  = t_nsect
      temp_czsect    = t_czsect   ; temp_dkc	   = t_dkc	; temp_lmr    = t_lmr
      temp_dr	     = t_dr       ; temp_dh	   = t_dh	; temp_db     = t_db
      temp_height    = t_height   ; temp_base      = t_base	  
      temp_geomtype  = t_geomtype ; temp_tran_edge = tran_edge
      temp_tran_sect = tran_sect
	
      call lmr_solver(cas)

      t_cxsect = temp_cxsect ; t_radsect = temp_radsect ; t_nparts = temp_nparts
      t_cysect = temp_cysect ; t_djc     = temp_djc	; t_nsect  = temp_nsect
      t_czsect = temp_czsect ; t_dkc	 = temp_dkc	; t_lmr	   = temp_lmr
      t_dr     = temp_dr     ; t_dh	 = temp_dh	; t_db	   = temp_db
      t_height = temp_height ; t_base	 = temp_base
      t_geomtype = temp_geomtype ; tran_edge=temp_tran_edge
      tran_sect=temp_tran_sect

      Case (2)
      temp_cxsect = b_cxsect ; temp_radsect = b_radsect	; temp_nparts = b_nparts
      temp_cysect = b_cysect ; temp_djc	    = b_djc	; temp_nsect  =	b_nsect
      temp_czsect = b_czsect ; temp_dkc	    = b_dkc	; temp_lmr    = b_lmr
      temp_dr	  = b_dr

      call lmr_solver(cas)

      b_cxsect = temp_cxsect ; b_radsect = temp_radsect ; b_nparts = temp_nparts
      b_cysect = temp_cysect ; b_djc     = temp_djc	; b_nsect  = temp_nsect
      b_czsect = temp_czsect ; b_dkc	 = temp_dkc     ; b_lmr	   = temp_lmr
      b_dr     = temp_dr

      Case (3)
      temp_nparts = p_nparts(ipi) ; temp_nsect = p_nsect(ipi)      
      do n=1,p_nsect(ipi)
      temp_cxsect(n)  = p_cxsect(n,ipi)  ; temp_dr(n)  = p_dr(n,ipi)  
      temp_cysect(n)  = p_cysect(n,ipi)  ; temp_djc(n) = p_djc(n,ipi) 
!      temp_czsect(n)  = p_czsect(n,ipi)  ; temp_dkc(n) = p_dkc(n,ipi) 
      temp_radsect(n) = p_radsect(n,ipi) ; temp_lmr(n) = p_lmr(n,ipi)
      end do

      call lmr_solver(cas)
      
      p_nparts(ipi) = temp_nparts ; p_nsect(ipi) = temp_nsect
      do n=1,p_nsect(ipi)
      p_cxsect(n,ipi)  = temp_cxsect(n)  ; p_dr(n,ipi)  = temp_dr(n) 
      p_cysect(n,ipi)  = temp_cysect(n)  ; p_djc(n,ipi) = temp_djc(n)
!      p_czsect(n,ipi)  = temp_czsect(n) ; p_dkc(n,ipi) = temp_dkc(n)
      p_radsect(n,ipi) = temp_radsect(n) ; p_lmr(n,ipi) = temp_lmr(n)
      end do

      end select 

      end subroutine
!-----------------------------------------------------------------------------------------
      subroutine lmr_solver(cas)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      
      integer,intent(in) :: cas
      integer :: nl,nlim,m,is,ib,b,n,j,i2,j2
      double precision :: dist

      WRITE(6,*) 'Enterered lmr part, section:'
      
      if(cas.eq.1) then
      do nl=1,temp_nsect-1
	if(temp_geomtype(nl).eq.1 .and. temp_geomtype(nl+1).eq.2) then
	  WRITE(6,'(I3,F10.5)') nl, temp_geomtype(nl)
	  do i2=1,8
	    write(6,'(2F10.5)') temp_tran_sect(i2,1,nl) , temp_tran_sect(i2,2,nl)
	  end do
	end if
      end do
      end if

      nlim=temp_nsect
      do i=1,nzone
	do nl=1,nlim
	  if(xlmr(i,1).gt.temp_cxsect(nl) .and. xlmr(i,1).lt.temp_cxsect(nl+1)) then
	    is=nl+1
	     do m=nlim+1,is,-1
	       temp_geomtype(m)=temp_geomtype(m-1)
	       temp_cxsect(m)=temp_cxsect(m-1)
	       temp_cysect(m)=temp_cysect(m-1)
	       temp_czsect(m)=temp_czsect(m-1)
	       temp_radsect(m)=temp_radsect(m-1)
	       temp_djc(m)=temp_djc(m-1)
	       temp_dkc(m)=temp_dkc(m-1)
	       temp_dr(m)=temp_dr(m-1)
	       IF(cas.eq.1) THEN
	       temp_height(m)=temp_height(m-1)
	       temp_base(m)=temp_base(m-1)
	       temp_dh(m)=temp_dh(m-1)
	       temp_db(m)=temp_db(m-1)
	       do i2=1,8 ; do j2=1,2
	       temp_tran_sect(i2,j2,m)=temp_tran_sect(i2,j2,m-1)
	       temp_tran_edge(i2,j2,m)=temp_tran_edge(i2,j2,m-1)
	       end do ; end do 
	       END IF
	     end do
	     nlim=nlim+1
	     dist=xlmr(i,1)-temp_cxsect(nl)
	     temp_geomtype(is)=temp_geomtype(nl)
	     temp_cxsect(is)=xlmr(i,1)
	     temp_cysect(is)=temp_cysect(nl)+temp_djc(is)*dist
	     temp_czsect(is)=temp_czsect(nl)+temp_dkc(is)*dist
	     temp_radsect(is)=temp_radsect(nl)+temp_dr(is)*dist
	     IF(cas.eq.1) THEN
	     temp_height(is)=temp_height(nl)+temp_dh(is)*dist
	     temp_base(is)  =temp_base(nl)+temp_db(is)*dist
	     do i2=1,8 ; do j2=1,2
	       temp_tran_edge(i2,j2,is)=tran_edge(i2,j2,nl)
	       temp_tran_sect(i2,j2,is)=tran_sect(i2,j2,nl)+temp_tran_edge(i2,j2,is)*dist
	     end do ; end do
	     END IF
	  end if
	end do
      end do
      temp_nsect=nlim
      temp_nparts=nlim-1
      
      ipart=0
      do i=1,nzone ; do nl=1,nlim-1
	if(temp_cxsect(nl).ge.xlmr(i,1) .and. temp_cxsect(nl+1).le.xlmr(i,2)) then
	  ipart=ipart+1 ; temp_lmr(ipart)=lmr(i)
	end if
      end do ; end do
      
      IF(cas.eq.1) THEN
      do n=1,t_nsect-1 ; do nl=1,nlim
	if(t_geomtype(n).ne.t_geomtype(n+1)) then
	  if (temp_cxsect(nl).gt.t_cxsect(n) .and. temp_cxsect(nl).lt.t_cxsect(n+1)) then
	    temp_geomtype(nl)=3
	  end if
	end if
      end do ; end do 

      do nl=1,temp_nsect-1
	if(temp_geomtype(nl).eq.3 .and. temp_geomtype(nl+1).eq.2) then
	  WRITE(6,'(I3,F10.5)') nl, temp_geomtype(nl)
	  do i2=1,8
	    write(6,'(2F10.5)') temp_tran_sect(i2,1,nl) , temp_tran_sect(i2,2,nl)
	  end do
	end if
      end do
      END IF

      ! TEST -----------------------------------------------------------------------------
!       do nl=1,temp_nsect
!       write(6,758) nl,temp_geomtype(nl),temp_cxsect(nl),temp_dh(nl), &
!      &             temp_db(nl),temp_height(nl),temp_base(nl),temp_lmr(nl)
!       end do
!       do nl=1,temp_nsect
!       write(6,758) nl,temp_geomtype(nl),temp_cxsect(nl),temp_dr(nl),temp_djc(nl),temp_dkc(nl)
!       end do        
!       write(6,*) '=================================================================='
      !----------------------------------------------------------------------------------- 
      
758   format(2I3,5F10.5,I3)
      end subroutine
!=========================================================================================		    


! THREE-POINT CIRCLE SOLVER
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine three_pt_circle_sol(cas,n,nd,ic)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer,intent(in) :: cas,n,nd,ic
      integer :: m,m_inc,j
      double precision :: A,B,C,D,y1,y2,y3,z1,z2,z3
      double precision, dimension(3,2) :: pt

      SELECT CASE(cas)
	Case(1)
	m_inc=2
	pt(ic,1)=edge(ic,1)     ; pt(ic,2)=edge(ic,2)
	pt(ic+1,1)=edge(ic+1,1) ; pt(ic+1,2)=edge(ic+1,2)
	pt(ic+2,1)=edge(ic+2,1) ; pt(ic+2,2)=edge(ic+2,2)


	Case(2)
	m_inc=1
	do j=1,2
	  if(d_cxsect(nd,j).ge.b_cxsect(n) .and. d_cxsect(nd,j).le.b_cxsect(n+1)) then
	    d_radsect(nd,j)=b_radsect(n)+b_dr(n)*(d_cxsect(nd,j)-b_cxsect(n))
	  end if 
	end do

	pt(1,1)=d_cxsect(nd,1) ; pt(1,2)=d_radsect(nd,1)
	pt(3,1)=d_cxsect(nd,2) ; pt(3,2)=d_radsect(nd,2)
	pt(2,1)=pt(1,1) + (pt(3,1)-pt(1,1))/2 
	pt(2,2)=pt(1,2) + (pt(3,2)-pt(1,2))/2 + bsize(nd)

	Case(3)
	m_inc=1
	pt(1,1)=b_cxsect(n+1) ; pt(1,2)=b_czsect(n+1)+b_radsect(n+1)  
	pt(2,1)=b_cxsect(n)   ; pt(2,2)=b_czsect(n) 
	pt(3,1)=b_cxsect(n+1) ; pt(3,2)=b_czsect(n+1)-b_radsect(n+1)
	do m=1,3
	WRITE(6,'(a,I3,3F10.5)') 'Case 3 pt:', m, pt(m,1), pt(m,2) 
	end do

	Case(4)
	m_inc=1
	pt(ic,1)  =p_cxsect(n+1,ipi) ; pt(ic,2)  =p_cysect(n+1,ipi)+p_radsect(n+1,ipi)
	pt(ic+1,1)=p_cxsect(n,ipi)   ; pt(ic+1,2)=p_cysect(n,ipi)
	pt(ic+2,1)=p_cxsect(n+1,ipi) ; pt(ic+2,2)=p_cysect(n+1,ipi)-p_radsect(n+1,ipi)

      END SELECT

      y1=pt(ic,1)   ; z1=pt(ic,2) 
      y2=pt(ic+1,1) ; z2=pt(ic+1,2) 
      y3=pt(ic+2,1) ; z3=pt(ic+2,2)      
      A=y1*(z2-z3)-z1*(y2-y3)+y2*z3-y3*z2
      B=(y1**2+z1**2)*(z3-z2)+(y2**2+z2**2)*(z1-z3)+(y3**2+z3**2)*(z2-z1)
      C=(y1**2+z1**2)*(y2-y3)+(y2**2+z2**2)*(y3-y1)+(y3**2+z3**2)*(y1-y2)
      D=(y1**2+z1**2)*(y3*z2-y2*z3)+(y2**2+z2**2)*(y1*z3-y3*z1)+(y3**2+z3**2)*(y2*z1-y1*z2)
      B=B/A ; C=C/A ; D=D/A ; A=1
      
      if(cas.eq.1) then
	axe=2 ; inc=1 ; ec(axe,ic)=-(B/2*A) ; ec(axe+inc,ic)=-(C/2*A)
	cr(ic)=sqrt((ec(axe,ic)-y1)**2+(ec(axe+inc,ic)-z1)**2)
      else if(cas.eq.2 .or. cas.eq.3) then
	axe=1 ; inc=2 ; ec(axe,ic)=-(B/2*A) ; ec(axe+inc,ic)=-(C/2*A)
	cr(ic)=sqrt((ec(axe,ic)-y1)**2+(ec(axe+inc,ic)-z1)**2)
      else if(cas.eq.4) then
	axe=1 ; inc=1 ; ec(axe,ic)=-(B/2*A) ; ec(axe+inc,ic)=-(C/2*A)
	cr(ic)=sqrt((ec(axe,ic)-y1)**2+(ec(axe+inc,ic)-z1)**2)
      end if

      do m=ic,ic+2,m_inc
	if(pt(m,1).ge.ec(axe,ic) .and. pt(m,2).ge.ec(axe+inc,ic)) then
	  angle(m)=atan((pt(m,2)-ec(axe+inc,ic))/(pt(m,1)-ec(axe,ic)))
	    
	else if(pt(m,1).le.ec(axe,ic) .and. pt(m,2).ge.ec(axe+inc,ic)) then
	  angle(m)=pi+atan((pt(m,2)-ec(axe+inc,ic))/(pt(m,1)-ec(axe,ic)))
	    
	else if(pt(m,1).le.ec(axe,ic) .and. pt(m,2).le.ec(axe+inc,ic)) then
	  angle(m)=pi+atan((pt(m,2)-ec(axe+inc,ic))/(pt(m,1)-ec(axe,ic)))
	    
	else if(pt(m,1).ge.ec(axe,ic) .and. pt(m,2).le.ec(axe+inc,ic)) then
	  angle(m)=2*pi+atan((pt(m,2)-ec(axe+inc,ic))/(pt(m,1)-ec(axe,ic)))
	end if
      end do
      
      if(cas.eq.1) then
	alpha=angle(ic+2)-angle(ic)
	if(ic.eq.7) alpha=angle(ic+2)+2*pi-angle(ic)
!        WRITE(6,'(a,f10.5)') 'alpha:', alpha 
      end if
      if(cas.eq.2) alpha=angle(ic)-angle(ic+2)
      if(cas.eq.3 .or. cas.eq.4) alpha=angle(ic+1)-angle(ic)


      end subroutine
!=========================================================================================


! TUNNEL GEOMETRY
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine tunnel_main
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      
      integer :: ia,ib,ir,l,n,m,ipts,ipsect,nn,ang
     
      t_vertice(1,1)=tye ; t_vertice(1,2)=tze
      t_vertice(2,1)=tyi ; t_vertice(2,2)=tze
      t_vertice(3,1)=tyi ; t_vertice(3,2)=tzi
      t_vertice(4,1)=tye ; t_vertice(4,2)=tze

      do n=1,t_nsect-1
	IF(t_geomtype(n).eq.1 .and. t_geomtype(n+1).eq.t_geomtype(n)) THEN
          t_djc(n)=(t_cysect(n+1)-t_cysect(n))/(t_cxsect(n+1)-t_cxsect(n))
          t_dkc(n)=(t_czsect(n+1)-t_czsect(n))/(t_cxsect(n+1)-t_cxsect(n))
          t_dr(n) =(t_radsect(n+1)-t_radsect(n))/(t_cxsect(n+1)-t_cxsect(n))
	ELSE IF(t_geomtype(n).eq.2 .and. t_geomtype(n+1).eq.t_geomtype(n)) THEN
	  t_djc(n)=(t_cysect(n+1)-t_cysect(n))/(t_cxsect(n+1)-t_cxsect(n))
          t_dkc(n)=(t_czsect(n+1)-t_czsect(n))/(t_cxsect(n+1)-t_cxsect(n))
 	  t_dh(n) =(t_height(n+1)-t_height(n))/(t_cxsect(n+1)-t_cxsect(n))
	  t_db(n) =(t_base(n+1)-t_base(n))/(t_cxsect(n+1)-t_cxsect(n))
	ELSE IF(t_geomtype(n).ne.t_geomtype(n+1) .or. (t_geomtype(n).eq.t_geomtype(n+1) .and.    &
     &        t_geomtype(n).eq.3)) THEN
	  DO nn=n,n+1
	  if(t_geomtype(nn).eq.1) then	  
	    do i=1,8
	      ang=45*i
	      loc(t_sect(nn),i)=ang*pi/180
	    end do
	    do i=1,8
	      tran_sect(i,1,nn)=t_cysect(nn) + t_radsect(nn)*dcos(loc(t_sect(nn),i))
	      tran_sect(i,2,nn)=t_czsect(nn) + t_radsect(nn)*dsin(loc(t_sect(nn),i))
	    end do
	  else if(t_geomtype(nn).eq.2) then
	    tran_sect(1,1,nn)=t_cysect(nn)+t_base(nn)/2
	    tran_sect(1,2,nn)=t_czsect(nn)+t_height(nn)/2
	    tran_sect(2,1,nn)=t_cysect(nn)
	    tran_sect(2,2,nn)=t_czsect(nn)+t_height(nn)/2
	    tran_sect(3,1,nn)=t_cysect(nn)-t_base(nn)/2
	    tran_sect(3,2,nn)=t_czsect(nn)+t_height(nn)/2
	    tran_sect(4,1,nn)=t_cysect(nn)-t_base(nn)/2
	    tran_sect(4,2,nn)=t_czsect(nn)
	    tran_sect(5,1,nn)=t_cysect(nn)-t_base(nn)/2
	    tran_sect(5,2,nn)=t_czsect(nn)-t_height(nn)/2
	    tran_sect(6,1,nn)=t_cysect(nn)
	    tran_sect(6,2,nn)=t_czsect(nn)-t_height(nn)/2
	    tran_sect(7,1,nn)=t_cysect(nn)+t_base(nn)/2
	    tran_sect(7,2,nn)=t_czsect(nn)-t_height(nn)/2
	    tran_sect(8,1,nn)=t_cysect(nn)+t_base(nn)/2
	    tran_sect(8,2,nn)=t_czsect(nn)
	  else if(t_geomtype(nn).eq.3) then
	    !small rec:
	    tran_sect(1,1,nn)=t_cysect(nn)+t_base(nn)/2
	    tran_sect(1,2,nn)=t_czsect(nn)+t_height(nn)/2
	    tran_sect(3,1,nn)=t_cysect(nn)-t_base(nn)/2
	    tran_sect(3,2,nn)=t_czsect(nn)+t_height(nn)/2
	    tran_sect(5,1,nn)=t_cysect(nn)-t_base(nn)/2
	    tran_sect(5,2,nn)=t_czsect(nn)-t_height(nn)/2
	    tran_sect(7,1,nn)=t_cysect(nn)+t_base(nn)/2
	    tran_sect(7,2,nn)=t_czsect(nn)-t_height(nn)/2
	    !big rec:
	    tran_sect(2,1,nn)=t_cysect(nn)
	    tran_sect(2,2,nn)=t_oczsect(nn)+t_oheight(nn)/2
	    tran_sect(4,1,nn)=t_ocysect(nn)-t_obase(nn)/2
	    tran_sect(4,2,nn)=t_czsect(nn)
	    tran_sect(6,1,nn)=t_cysect(nn)
	    tran_sect(6,2,nn)=t_oczsect(nn)-t_oheight(nn)/2
	    tran_sect(8,1,nn)=t_ocysect(nn)+t_obase(nn)/2
	    tran_sect(8,2,nn)=t_czsect(nn)
	    do i=1,8
	      WRITE(6,'(a,I3,2F10.5)') 'Edge:',i,tran_sect(i,1,nn),tran_sect(i,2,nn)
	    end do
	  end if
	  END DO
	END IF
	
	! Create the edge function from connecting the points from circular to rectangular section:
	do i=1,8
	  tran_edge(i,1,n)=(tran_sect(i,1,n+1)-tran_sect(i,1,n))/(t_cxsect(n+1)-t_cxsect(n))
	  tran_edge(i,2,n)=(tran_sect(i,2,n+1)-tran_sect(i,2,n))/(t_cxsect(n+1)-t_cxsect(n))
	end do
      end do

      do i=1,t_nsect-1
	WRITE(6,'(I3,4F20.5)') t_sect(i), t_dh(i), t_db(i), t_height(i), t_base(i)
      end do
      
      call lmr_resolution(1) 

      xitc=txi+tlay*(dx/t_lmr(1))         
      xetc=txe-tlay*(dx/t_lmr(t_nsect-1)) 
      
      call tunnel_case

      WRITE(6,*) 't_nsect after lmr', t_nsect
      ipts=0
      m=0
      DO n=1,t_nsect-1
        ipsect=0
	dxx=dx/t_lmr(n) ; dyy=dy/t_lmr(n) ; dzz=dz/t_lmr(n)
	IF (t_geomtype(n).eq.1 .and. t_geomtype(n).eq.t_geomtype(n+1)) THEN
	  call circular_part(n,ipts,ipsect)
	ELSE IF (t_geomtype(n).eq.2 .and. t_geomtype(n).eq.t_geomtype(n+1)) THEN
	  call rectangular_part(n,ipts,ipsect)
	ELSE IF (t_geomtype(n).ne.t_geomtype(n+1)) THEN
          call transition_part(n,ipts,ipsect)
	ELSE IF (t_geomtype(n).eq.3 .and. t_geomtype(n).eq.t_geomtype(n+1)) THEN
	  call transition_part(n,ipts,ipsect)
	END IF
	WRITE(6,*)  itun(n)
      END DO
      end subroutine

!-----------------------------------------------------------------------------------------
      subroutine tunnel_case
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer :: itc,jtc,ktc,it,l,n,ix,lmax
      
      if(fullness.eq..TRUE.) then
	lmax=1
      else
	lmax=tlay
      end if

      it=0
      DO l=1,lmax ; Do n=1,t_nsect-1
        dxx=dx/t_lmr(n) ; dyy=dy/t_lmr(n) ; dzz=dz/t_lmr(n) 
        nxn=nint((t_cxsect(n+1)-t_cxsect(n))/dxx)
	do ix=1,nxn
        xval=t_cxsect(n) + (ix-1)*dxx
	yitc=tyi+(l-1)*dyy ; zitc=tzi+(l-1)*dzz
        yetc=tye-(l-1)*dyy ; zetc=tze-(l-1)*dzz
        tti=nint((txe-txi)/dxx) ; ttj=nint((tye-tyi)/dyy) ; ttk=nint((tze-tzi)/dzz)
        do jtc=1,ttj+1
  	  yval=tyi+(jtc-1)*dyy
	  IF(yval.le.yitc .or. yval.ge.yetc) THEN
	    do ktc=1,ttk+1
	      zval=tzi+(ktc-1)*dzz
	      it=it+1 ; casepts(it,1)=xval ; casepts(it,2)=yval ; casepts(it,3)=zval
	    end do
	  ELSE
	    do ktc=1,ttk+1
	      zval=tzi+(ktc-1)*dzz
	      if(zval.le.zitc .or. zval.ge.zetc) then
		it=it+1 ; casepts(it,1)=xval ; casepts(it,2)=yval ; casepts(it,3)=zval
	      end if 
	    end do 
	  END IF
	  end do
	End do
      End Do ; END DO
      itun(t_nsect)=it
      
      if(fullness.eq..TRUE.) then
	yitc=tyi ; zitc=tzi
        yetc=tye ; zetc=tze
      end if

      end subroutine
!-----------------------------------------------------------------------------------------
      subroutine circular_part(n,ipts,ipsect)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer,intent(inout) ::ipts,ipsect
      integer,intent(in) :: n
      integer :: ix,lmax,l
      double precision :: distmax,nalpha,r
 
      WRITE(6,'(a,I4)') 'Enterered circular part, section:', n
      nxn=nint((t_cxsect(n+1)-t_cxsect(n))/dxx)
      if(nxn.eq.0) nxn=1
      do ix=1,nxn
        xval=t_cxsect(n) + (ix-1)*dxx
        t_jc=t_cysect(n) + t_djc(n)*(ix-1)*dxx
	t_kc=t_czsect(n) + t_dkc(n)*(ix-1)*dxx
	rval=t_radsect(n) + t_dr(n)*(ix-1)*dxx
	
	! Calculater the number of layer required 
	IF((fullness.eq..FALSE. .and. (xval.lt.xitc .or. xval.ge.xetc)) .or. &
     &	    fullness.eq..TRUE.)THEN
	  vertice(1,1)=t_jc+rval*dcos(pi/4)   ; vertice(1,2)=t_kc+rval*dsin(pi/4)
	  vertice(2,1)=t_jc+rval*dcos(3*pi/4) ; vertice(2,2)=t_kc+rval*dsin(3*pi/4)
	  vertice(3,1)=t_jc+rval*dcos(5*pi/4) ; vertice(3,2)=t_kc+rval*dsin(5*pi/4)
	  vertice(4,1)=t_jc+rval*dcos(7*pi/4) ; vertice(4,2)=t_kc+rval*dsin(7*pi/4)
	  do i=1,4
	  cor_dis(i)=sqrt((t_vertice(i,1)-vertice(i,1))**2+(t_vertice(i,2)-vertice(i,2))**2)
	  end do
	  distmax=maxval(cor_dis)
	  lmax=nint(distmax/dyy)
	ELSE
	  lmax=tlay
	END IF

	! Calculate and store the points of the circles within the tunnel boundary
	DO l=1,lmax
	r=rval+(l-1)*dyy
	nalpha=nint(2*pi*r/dyy)
	do t=1,nalpha
	  yval=t_jc+r*dcos(2.d0*pi/nalpha*(t-1))
	  zval=t_kc+r*dsin(2.d0*pi/nalpha*(t-1))
	  IF(yval.gt.yitc .and. yval.lt.yetc) THEN ; if(zval.gt.zitc .and. zval.lt.zetc) then
            ipts=ipts+1 ; tubpts(ipts,1)=xval ; tubpts(ipts,2)=yval ; tubpts(ipts,3)=zval
	    ipsect=ipsect+1
	  end if ; END IF
	end do
	END DO

      end do
      itun(n)=ipsect
      
      end subroutine

!-----------------------------------------------------------------------------------------
      subroutine rectangular_part(n,ipts,ipsect)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer,intent(in) :: n
      integer,intent(inout) :: ipts,ipsect
      integer :: it,ix,jtc,ktc,j,k
      double precision :: yi,ye,zi,ze,distmax
      double precision, dimension(100000) :: ytemp,ztemp
      integer :: l,lmax,itemp,i2
      
      WRITE(6,'(a,I4)') 'Enterered rectangular part, section:', n
      nxn=nint((t_cxsect(n+1)-t_cxsect(n))/dxx)
      if(nxn.eq.0) nxn=1
      DO ix=1,nxn
	itemp=0
        xval=t_cxsect(n) + (ix-1)*dxx
	t_jc=t_cysect(n) + t_djc(n)*(ix-1)*dxx ; t_kc=t_czsect(n) + t_dkc(n)*(ix-1)*dxx
	t_h =t_height(n) + t_dh(n)*(ix-1)*dxx  ; t_b =t_base(n) + t_db(n)*(ix-1)*dxx
	! Calculates the number of layer required
	IF((fullness.eq..FALSE. .and. (xval.lt.xitc .or. xval.ge.xetc)) .or. &
     &	    (fullness.eq..TRUE.))THEN
	  ! FULLSECTION.FACE
	  yi =t_jc-t_b/2  ; ye =t_jc+t_b/2
	  zi =t_kc-t_h/2  ; ze =t_kc+t_h/2
	  vertice(1,1)=ye ; vertice(1,2)=ze 
	  vertice(2,1)=yi ; vertice(2,2)=ze 
	  vertice(3,1)=yi ; vertice(3,2)=zi 
	  vertice(4,1)=ye ; vertice(4,2)=ze 
	  do i=1,4
	  cor_dis(i)=sqrt((t_vertice(i,1)-vertice(i,1))**2+(t_vertice(i,2)-vertice(i,2))**2)
	  end do
	  distmax=maxval(cor_dis)
	  lmax=nint(distmax/dyy)
	ELSE
	  lmax=tlay
	END IF
	
	! Calculate the position of each point and store them in the point scatter matrix
	DO l=1,lmax
	yi =t_jc-(t_b/2+(l-1)*dyy)  ; ye =t_jc+(t_b/2+(l-1)*dyy)
	zi =t_kc-(t_h/2+(l-1)*dzz)  ; ze =t_kc+(t_h/2+(l-1)*dzz)
	ttj=nint((ye-yi)/dyy)	      ; ttk=nint((ze-zi)/dzz)
	do j=1,ttj+1
	  yval=yi+(j-1)*dyy
	  itemp=itemp+1 ;  ytemp(itemp)=yval ; ztemp(itemp)=zi
	  itemp=itemp+1 ;  ytemp(itemp)=yval ; ztemp(itemp)=ze
	  do i2=itemp-1,itemp
	    IF(ytemp(i2).gt.yitc .and. ytemp(i2).lt.yetc) THEN  
	      if(ztemp(i2).gt.zitc .and. ztemp(i2).lt.zetc) then
       ipts=ipts+1 ; tubpts(ipts,1)=xval ; tubpts(ipts,2)=ytemp(i2) ; tubpts(ipts,3)=ztemp(i2)
	      ipsect=ipsect+1
	      end if 
	    END IF
	  end do
	end do
	do k=2,ttk
	  zval=zi+(k-1)*dzz
	  itemp=itemp+1 ;  ytemp(itemp)=yi ; ztemp(itemp)=zval
	  itemp=itemp+1 ;  ytemp(itemp)=ye ; ztemp(itemp)=zval
	  do i2=itemp-1,itemp
	    IF(ytemp(i2).gt.yitc .and. ytemp(i2).lt.yetc) THEN  
	      if(ztemp(i2).gt.zitc .and. ztemp(i2).lt.zetc) then
        ipts=ipts+1 ; tubpts(ipts,1)=xval ; tubpts(ipts,2)=ytemp(i2); tubpts(ipts,3)=ztemp(i2)
	      ipsect=ipsect+1
	      end if 
	    END IF
	  end do
	end do
	END DO

      END DO
      itun(n)=ipsect

      WRITE(6,*) n,itun(n)
      end subroutine
	
!-----------------------------------------------------------------------------------------
      subroutine transition_part(n,ipts,ipsect)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer, intent(in) :: n
      integer, intent(inout) :: ipts,ipsect
      integer :: nn,j,ix,iy,iz,nyn,nzn,lmax,l,m,ic
      double precision :: theta,yinit,ylast,ytemp,zinit,zlast,ztemp,ti,te,dt!,alpha
      double precision :: ri,re,dr,y1,y2,y3,z1,z2,z3
      double precision :: A,B,C,D
      logical :: circle

	! Calculate and store all the point of the different ellipse: 
	nxn=(t_cxsect(n+1)-t_cxsect(n))/dxx
	do ix=1,nxn
	xval=t_cxsect(n)+(ix-1)*dxx

	! Set up the number of layer required based on the fullness of the section:
	IF((fullness.eq..FALSE. .and. (xval.lt.xitc .or. xval.ge.xetc)) .or. &
     &	    fullness.eq..TRUE.)THEN
	  do nn=n,n+1
	    if(t_geomtype(nn).eq.1) then
	      ttj=nint((tye-tyi)/dyy) ; ttk=nint((tze-tzi)/dzz)
	      if(ttj.ge.ttk) then
		ri=0 ; re=2*tye ; dr=dyy
	      else
	        ri=0 ; re=2*tze ; dr=dzz
	      end if
	      lmax=nint((re-ri)/dr)
	    end if
	  end do
	ELSE
	  lmax=tlay
        END IF 
       
	do l=1,lmax
	  edge(1,1)=tran_sect(1,1,n) + tran_edge(1,1,n)*(ix-1)*dxx + (l-1)*dyy
	  edge(1,2)=tran_sect(1,2,n) + tran_edge(1,2,n)*(ix-1)*dxx + (l-1)*dzz
	
	  edge(2,1)=tran_sect(2,1,n) + tran_edge(2,1,n)*(ix-1)*dxx
	  edge(2,2)=tran_sect(2,2,n) + tran_edge(2,2,n)*(ix-1)*dxx + (l-1)*dzz
	
	  edge(3,1)=tran_sect(3,1,n) + tran_edge(3,1,n)*(ix-1)*dxx - (l-1)*dyy
	  edge(3,2)=tran_sect(3,2,n) + tran_edge(3,2,n)*(ix-1)*dxx + (l-1)*dzz
	
	  edge(4,1)=tran_sect(4,1,n) + tran_edge(4,1,n)*(ix-1)*dxx - (l-1)*dyy
	  edge(4,2)=tran_sect(4,2,n) + tran_edge(4,2,n)*(ix-1)*dxx
	
	  edge(5,1)=tran_sect(5,1,n) + tran_edge(5,1,n)*(ix-1)*dxx - (l-1)*dyy
	  edge(5,2)=tran_sect(5,2,n) + tran_edge(5,2,n)*(ix-1)*dxx - (l-1)*dzz
	
	  edge(6,1)=tran_sect(6,1,n) + tran_edge(6,1,n)*(ix-1)*dyy
	  edge(6,2)=tran_sect(6,2,n) + tran_edge(6,2,n)*(ix-1)*dxx - (l-1)*dzz
	
	  edge(7,1)=tran_sect(7,1,n) + tran_edge(7,1,n)*(ix-1)*dxx + (l-1)*dyy
	  edge(7,2)=tran_sect(7,2,n) + tran_edge(7,2,n)*(ix-1)*dxx - (l-1)*dzz
	
	  edge(8,1)=tran_sect(8,1,n) + tran_edge(8,1,n)*(ix-1)*dxx + (l-1)*dyy
	  edge(8,2)=tran_sect(8,2,n) + tran_edge(8,2,n)*(ix-1)*dxx
        
	  edge(9,1)=edge(1,1) ; edge(9,2)=edge(1,2)

	  do ic=1,7,2
	  
	  call three_pt_circle_sol(1,n,0,ic)
	  
	  nthet=nint(alpha*cr(ic)/dyy)
	  dt=alpha/nthet
	  do t=1,nthet
	    ipts=ipts+1
	    ipsect=ipsect+1
	    theta=angle(ic)+(t-1)*dt
	    if(theta.gt.2*pi) theta=theta-2*pi
	    yval=ec(axe,ic)+cr(ic)*dcos(theta)
	    zval=ec(axe+inc,ic)+cr(ic)*dsin(theta)
	    IF(yval.gt.yitc .and. yval.lt.yetc) THEN
	      if(zval.gt.zitc .and. zval.lt.zetc) then
              ipts=ipts+1 ; tubpts(ipts,1)=xval ; tubpts(ipts,2)=yval ; tubpts(ipts,3)=zval
	      ipsect=ipsect+1
	      end if
	    END IF
	  end do
	  end do
	  end do
	end do
	itun(n)=ipsect

      end subroutine


! BULB GEOMETRY:
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine bulb_main
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      
      integer :: nlim,n,ix,nd,ipts,isect,don_tissue,l,inlay,nlay,nr,nb
      double precision :: b_jc,b_kc,rt
      double precision,dimension(1000) :: rad
   
      ! Calculates and store radius slope------------------------------------------------
      do n=1,b_nsect-1
	b_djc(n)=(b_cysect(n+1)-b_cysect(n))/(b_cxsect(n+1)-b_cxsect(n))
	b_dkc(n)=(b_czsect(n+1)-b_czsect(n))/(b_cxsect(n+1)-b_cxsect(n))
	b_dr(n)=(b_radsect(n+1)-b_radsect(n))/(b_cxsect(n+1)-b_cxsect(n))
      end do
      b_nparts=b_nsect-1

      ! Initialise the lmr resolution of each bulb section--------------------------------
      if(LRM.eq..TRUE.) then
	call lmr_resolution(2)
      else 
	b_lmr=1
      end if

      ! Screen display the radius slope---------------------------------------------------
      ipart=0
      write(6,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      do n=1,b_nparts
	write(6,'(X,a,I3,X,a,F5.3)') 'Part:',n, 'Radius Slope:', b_dr(n)
      end do 
      write(6,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

      ! Calculates and store the points of the bulb case----------------------------------
      ipts=0
      DO n=1,b_nsect-1
      isect=0 ; nr=0
      dxx=dx/b_lmr(n) ; dyy=dy/b_lmr(n) ; dzz=dz/b_lmr(n)
      nxn=nint((b_cxsect(n+1)-b_cxsect(n))/dxx)
      Do ix=1,nxn
	xval=b_cxsect(n) + (ix-1)*dxx
	b_jc=b_cysect(n) + b_djc(n)*(ix-1)*dxx
	b_kc=b_czsect(n) + b_dkc(n)*(ix-1)*dxx
	
	! Calculates the radius of each slices depending on their specifcation------------
	if(ndonut.ne.0) then
	  don_tissue=0
	  do nd=1,ndonut
	    if(xval.ge.d_cxsect(nd,1) .and. xval.le.d_cxsect(nd,2)) then
	      don_tissue=1
	      call ellipse_sol(2,n,nd,b_kc)
	    else if(don_tissue.eq.0) then
              rval=b_radsect(n)+ b_dr(n)*(ix-1)*dxx 
	    end if
	  end do
	else if (ndonut.eq.0) then
	  rval=b_radsect(n)+ b_dr(n)*(ix-1)*dxx 
	end if
	
	! Calculate the radius of the ellipse front radius--------------------------------
	if(b_geomtype(n).eq.2) call ellipse_sol(3,n,0,b_kc)
        
	! Calculate the number of layer required to an acceptable solidity----------------
	nr=nr+1 ; rad(nr)=rval
	if(nr.gt.1 .and. abs(rad(nr)-rad(nr-1)).ge.blay*dzz) then
	  nlay=nint(abs(rad(nr)-rad(nr-1))/dzz) ; inlay=nlay
	  do nb=0,blay
	    rt=rad(nr)-(inlay+blay-nb)*dzz
	    if(rt.gt.0) then
	      nlay=inlay+blay-nb ; exit
	    end if 
	  end do
	  if(rt.lt.0) nlay=inlay
	else if(nr.gt.1) then
	  nlay=blay
	end if

	! Calculate and store the points of the bulb case --------------------------------
	do l=1,nlay
	rval=rad(nr)-(l-1)*dzz
	nthet=nint(2.d0*pi*rval/dyy)
!	WRITE(6,'(a,2F10.3,2I5)') 'xval,rval,nthet:',xval,rval,nthet,nmax
	IF(b_fullness.eq..FALSE. .and. b_cxsect(n+1).eq.b_cxsect(b_nsect)  &
     &	    .and. ix.ge.nxn-nlay .and. ix.le.nxn .and. b_radsect(n+1).gt.0 .and. l.eq.1) THEN
	  call closing_face(n,ipts,isect,b_jc,b_kc)
	ELSE IF(b_fullness.eq..TRUE. .and. l.eq.1) THEN
	  call closing_face(n,ipts,isect,b_jc,b_kc)
	ELSE IF (b_fullness.eq..FALSE.) THEN
	  if(nthet.eq.0) then
	    ipts=ipts+1 ; isect=isect+1
	    bulpts(ipts,1,n)=xval
	    bulpts(ipts,2,n)=b_jc
	    bulpts(ipts,3,n)=b_kc
	  else
	    do t=1,nthet
	      ipts=ipts+1 ; isect=isect+1
	      bulpts(ipts,1,n)=xval
	      bulpts(ipts,2,n)=b_jc + rval*dcos(2.d0*pi/nthet*(t-1))
	      bulpts(ipts,3,n)=b_kc + rval*dsin(2.d0*pi/nthet*(t-1))
	    end do
	  end if
	END IF
	end do
        End Do
      ip(n)=isect ; write(6,111) 'Part:',n,'/',b_nparts,'written || Points:', ip(n) 
      END DO
      111 format(X,a,X,I2,X,a,I2,X,a,I6)

      end subroutine

!-----------------------------------------------------------------------------------------
      subroutine ellipse_sol(cas,n,nd,b_kc)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer, intent(in) :: cas,n,nd
      double precision,intent(in) :: b_kc
      integer :: m,j,ic
      double precision :: axl_val,azl_val,dt,theta,xlast,rtemp!,alpha

      ic=1 ; xlast=0
      call three_pt_circle_sol(cas,n,nd,ic)
      
      nthet=nint(alpha*cr(ic)/(dx/(b_lmr(n)*100)))
      dt=alpha/nthet
      do t=1,nthet
	if(cas.eq.2) theta=angle(ic)-(t-1)*dt
	if(cas.eq.3) theta=angle(ic+1)-(t-1)*dt
	if(theta.gt.2*pi) theta=theta-2*pi
	axl_val=ec(axe,ic)+cr(ic)*dcos(theta)
	azl_val=ec(axe+inc,ic)+cr(ic)*dsin(theta)
	if(t.eq.1) then
	  rtemp=azl_val ; xlast=abs(xval-axl_val)
	else if(t.gt.1 .and. abs(xval-axl_val).le.xlast) then
	  rtemp=azl_val ; xlast=abs(xval-axl_val)
!	  if(nd.eq.1)  WRITE(6,'(2F10.5)') rtemp,xlast
	end if
      end do
      if(b_geomtype(n).eq.1) rval=rtemp
      if(b_geomtype(n).eq.2) rval=rtemp-b_kc

      end subroutine
!-----------------------------------------------------------------------------------------
      subroutine closing_face(n,ipts,isect,b_jc,b_kc)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer, intent(in) :: n
      integer, intent(inout) :: ipts,isect
      integer :: nrn,ir,ic
      double precision, intent(in) :: b_jc,b_kc
      double precision :: r
      
      WRITE(6,*) b_cxsect(n+1), xval
      nrn=nint(rval/(dy/b_lmr(n)))
      WRITE(6,*) nrn
      do ir=2,nrn
	r=rval-(ir-1)*(dy/b_lmr(n))
	nthet=nint(2.d0*pi*r/(dy/b_lmr(n)))
	if(nthet.eq.0) then
	  ipts=ipts+1 ; isect=isect+1
	  bulpts(ipts,1,n)=xval
	  bulpts(ipts,2,n)=b_jc
          bulpts(ipts,3,n)=b_kc
	ELSE
	  do t=1,nthet
	    ipts=ipts+1 ; isect=isect+1
	    bulpts(ipts,1,n)=xval
            bulpts(ipts,2,n)=b_jc + r*dcos(2.d0*pi/nthet*(t-1))
	    bulpts(ipts,3,n)=b_kc + r*dsin(2.d0*pi/nthet*(t-1))
	  end do
	END IF
      end do

      end subroutine 


! PILAR GEOMETRY
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine pilar_main
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      
      integer :: n,nb,ix,l,ipts,isect,nr,inlay,nlay,m,mi,me,ib,it,iz,ic,nzn
      double precision :: p_jc,rt,xcor,ycor,dist,z_inc,zi,ze
      double precision :: b_jc,b_kc,r,opp,abscis,theta
      double precision,dimension(1000) :: rad
      
      
            
      

      ! Calculates and store radius slope------------------------------------------------
      WRITE(6,*) 'Before LMR solver'
      if(p_type(ipi).eq.2) then
      do n=1,p_nsect(ipi)-1
	p_djc(n,ipi)=(p_cysect(n+1,ipi)-p_cysect(n,ipi))/(p_cxsect(n+1,ipi)-p_cxsect(n,ipi))
!	p_dkc(n,ipi)=(p_czsect(n+1,ipi)-p_czsect(n,ipi))/(p_cxsect(n+1,ipi)-p_cxsect(n,ipi))
	p_dr(n,ipi)=(p_radsect(n+1,ipi)-p_radsect(n,ipi))/(p_cxsect(n+1,ipi)-p_cxsect(n,ipi))
	WRITE(6,998) p_cxsect(n,ipi),p_cysect(n,ipi),p_radsect(n,ipi),p_djc(n,ipi),p_dr(n,ipi)
      end do
      p_nparts(ipi)=p_nsect(ipi)-1
      end if

      ! Initialise the lmr resolution of each bulb section--------------------------------
      if(LRM.eq..TRUE.) then
	call lmr_resolution(3)
      else
	do n=1,p_nsect(ipi)-1
	p_lmr(n,ipi)=1
	end do
      end if
     
      DO n=1,p_nsect(ipi)-1
      ipts=0 ; isect=0 ; nr=0
      dxx=dx/p_lmr(n,ipi) ; dyy=dy/p_lmr(n,ipi) ; dzz=dz/p_lmr(n,ipi)
      nxn=nint((p_cxsect(n+1,ipi)-p_cxsect(n,ipi))/dxx)
      do ix=1,nxn
	xval=p_cxsect(n,ipi) + (ix-1)*dxx
	p_jc=p_cysect(n,ipi) + p_djc(n,ipi)*(ix-1)*dxx
!	WRITE(6,'(a,F10.5)') 'p_jc:',p_jc 
	
	! Calculates the radius of the section
	if(p_geomtype(n,ipi).eq.1) rval=p_radsect(n,ipi) + p_dr(n,ipi)*(ix-1)*dxx
	if(p_geomtype(n,ipi).eq.2) call pil_ellipse(4,n,p_jc)

	! Calculates the number of layers required
	nr=nr+1 ; rad(nr)=rval
	if(nr.gt.1 .and. abs(rad(nr)-rad(nr-1).ge.play*dyy)) then
	  nlay=nint(abs(rad(nr)-rad(nr-1))/dyy) ; inlay=nlay
	  do nb=0,play
	    rt=rad(nr)-(inlay+play-nb)*dyy
	    if(rt.gt.0) then
	      nlay=inlay+play-nb ; exit
	    end if
	  end do
	  if(rt.lt.0) nlay=inlay
	else if(nr.gt.1) then
	  nlay=play
	end if

	do l=1,nlay
	rval=rad(nr)-(l-1)*dyy
	nthet=nint(2*rval/dyy)
	if(nthet.gt.1) nthet=2
	IF(p_fullness.eq..FALSE. .and. p_cxsect(n+1,ipi).eq.p_cxsect(p_nsect(ipi),ipi) &
     &	.and. ix.ge.nxn-nlay .and. ix.le.nxn .and. p_radsect(n+1,ipi).gt.0 .and. l.eq.1) THEN
	  call pil_full_sect(n,ipts,isect,p_jc)
	ELSE IF(p_fullness.eq..TRUE. .and. l.eq.1) THEN
	  call pil_full_sect(n,ipts,isect,p_jc)
	ELSE IF(p_fullness.eq..FALSE.) THEN
	  if((nthet.le.1 .and. l.eq.1) .or. (l.eq.play .and. nint(2*rval/dyy).eq.1)) then 
	    ipts=ipts+1 ; isect=isect+1
	    xform(ipts,n,ipi)=xval
	    yform(ipts,n,ipi)=p_jc
	  else
	    do t=1,nthet
	      ipts=ipts+1 ; isect=isect+1
	      xform(ipts,n,ipi)=xval
	      yform(ipts,n,ipi)=p_jc + rval*dcos(pi-pi*(nthet-t))
	    end do
	  end if
	END IF
	end do
      end do
      WRITE(6,'(a,2I5)') 'ipts,isect',ipts,isect 
      ipil(n,ipi)=isect
      WRITE(6,222) 'Pilar:',ipi,'  Section:',n,'  Npoints:',ipil(n,ipi)
      WRITE(6,'(a,I5)') 'Ppoints', ipts
      END do

222   format(a6,I3,a11,I3,a11,I5)

      open(unit=81,file='pilar_form.dat')
      ipts=0
      do n=1,p_nsect(ipi)-1
        do i=1,ipil(n,ipi)
	ipts=ipts+1
	write(81,'(2F10.5)') xform(i,n,ipi),yform(i,n,ipi)
      end do ; end do
      WRITE(6,'(a,I5)') 'Pilar_form pts:',ipts
      close(81)


      ipts=0
      DO n=1,p_nsect(ipi)-1 ; DO i=1,ipil(n,ipi)
	ipts=ipts+1 ; xcor=xform(i,n,ipi) ; ycor=yform(i,n,ipi)
!	WRITE(6,'(I4,2F10.5)') ipts,xcor,ycor 
	
	! Calculate the starting point from the bulb turbine for each point of the pilar 2D form:
	do ib=1,b_nsect-1
	  if(xcor.ge.b_cxsect(ib) .and. xcor.le.b_cxsect(ib+1)) then
	    dist=xcor-b_cxsect(ib)
	    b_jc=b_cysect(ib) + b_djc(ib)*dist 
	    b_kc=b_czsect(ib) + b_dkc(ib)*dist
	    r=b_radsect(ib) + b_dr(ib)*dist ; abscis=abs(ycor-b_jc)
	    opp=sqrt(r**2-abscis**2)
	    zipil(i,n,ipi)=b_kc + (opp + dzz)*orientation(ipi)
	  end if
	end do
	zi=zipil(i,n,ipi)

	! Caculates the ending limit at the tunnel limit for each point of the pilar 2D form:
	do it=1,t_nsect-1
	  if(xcor.ge.t_cxsect(it) .and. xcor.le.t_cxsect(it+1)) then
	    
	    ! Circular tunnel:
	    if(t_geomtype(it).eq.1 .and. t_geomtype(it).eq.t_geomtype(it+1)) then
	      dist=xcor-t_cxsect(it)
	      t_jc=t_cysect(it) + t_djc(it)*dist 
	      t_kc=t_czsect(it) + t_dkc(it)*dist
	      r=t_radsect(it) + t_dr(it)*dist ; abscis=abs(ycor-t_jc)
	      opp=sqrt(r**2-abscis**2)
	      zepil(i,n,ipi)=t_kc + opp*orientation(ipi)
	    
	    ! Rectangular tunnel part:
	    else if(t_geomtype(it).eq.2 .and. t_geomtype(it).eq.t_geomtype(it+1)) then
	      dist=xcor-t_cxsect(it)
	      t_kc=t_czsect(it)+t_dkc(it)*dist ; t_h=(t_height(it)+t_dh(it)*dist)/2
	      zepil(i,n,ipi)=t_kc+t_h*orientation(ipi)
!	      WRITE(6,'(5F10.5)') t_czsect(it),t_kc,t_height(it)/2,t_dh(it),zepil(i,n,ipi)
    
	    ! Transitional tunnel part:
	    else if(t_geomtype(it).ne.t_geomtype(it+1) .or. (t_geomtype(it).eq.t_geomtype(it+1) &
     &		    .and. t_geomtype(it).eq.3)) then
	      dist=xcor-t_cxsect(it)
!	      WRITE(6,'(a,3F10.5)') 'xcor,t_cxsect(it),dist:',xcor,t_cxsect(it),dist
	      if(orientation(ipi).eq.1) then
		 mi=1 ; me=3 ; ic=mi
		 
	      else
		 mi=5 ; me=7 ; ic=mi
	      end if
	      
	      do m=mi,me
		edge(m,1)=tran_sect(m,1,it) + tran_edge(m,1,it)*dist
		edge(m,2)=tran_sect(m,2,it) + tran_edge(m,2,it)*dist
!		WRITE(6,'(2I4,5F15.8)') n,m,tran_sect(m,1,it),tran_edge(m,1,it),dist,edge(m,1)
	      end do

	      call three_pt_circle_sol(1,it,0,ic)
	      
	
!	      theta=acos((ycor-ec(axe,ic))/cr(ic))
!	      zepil(i,n,ipi)=ec(axe+inc,ic)+cr(ic)*dsin(theta)
	      WRITE(6,'(2I4,3F10.5)') axe,inc,ec(axe,ic),ec(axe+inc,ic),cr(ic)
	      r=cr(ic) ; abscis=abs(ycor-ec(axe,ic)) ; opp=sqrt(r**2-abscis**2)
	      zepil(i,n,ipi)=ec(axe+inc,ic) + opp *orientation(ipi)
!	      WRITE(6,'(4F10.5)') r,abscis,opp,zepil(i,n,ipi)
	    end if
	  end if
	end do
	ze=zepil(i,n,ipi)
	nzn=nint((ze-zi)/dzz)
!	WRITE(6,'(I4,2F10.5,I4,2F10.5)') ipts,zi,ze,nzn,xcor,ycor
      END DO ; END DO


      do n=1,p_nsect(ipi)-1 
	ipts=0
	do i=1,ipil(n,ipi)
	nznpil(i,n,ipi)=nint(abs(zepil(i,n,ipi)-zipil(i,n,ipi))/dzz)
	z_inc=(zepil(i,n,ipi)-zipil(i,n,ipi))/nznpil(i,n,ipi)
	do iz=1,nznpil(i,n,ipi)
	  ipts=ipts+1
	  xpil(ipts,n,ipi)=xform(i,n,ipi)
	  ypil(ipts,n,ipi)=yform(i,n,ipi)
	  zpil(ipts,n,ipi)=zipil(i,n,ipi)+(iz-1)*z_inc
!	  WRITE(6,'(2I6,3F10.5)') n,ipts,xpil(ipts,n,ipi),ypil(ipts,n,ipi),zpil(ipts,n,ipi)
	end do
      end do ; end do

      ! LMR test:
!      WRITE(6,*) 'After LMR solver'
!      if(p_type(ipi).eq.2) then
!      do n=1,p_nsect(ipi)
!      WRITE(6,999) p_cxsect(n,ipi),p_cysect(n,ipi),p_radsect(n,ipi),p_djc(n,ipi),p_dr(n,ipi),p_lmr(n,ipi)
!      end do
!      end if
      

998   format(5F10.5)
999   format(5F10.5,I5)

      ! Screen display the radius slope---------------------------------------------------
!      ipart=0
!      write(6,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
!      do n=1,p_nparts(i)
!	write(6,'(X,a,I3,X,a,F5.3)') 'Part:',n, 'Radius Slope:', p_dr(n)
!      end do 
!      write(6,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'




      end subroutine
!-----------------------------------------------------------------------------------------
      subroutine pil_ellipse(cas,n,p_jc)
!----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer, intent(in) :: cas,n
      double precision, intent(in) :: p_jc
      integer :: m,j,ic
      double precision :: axl_val,ayl_val,dt,theta,rtemp,xlast

      ic=1 ; xlast=0
      call three_pt_circle_sol(4,n,0,ic)

      nthet=nint(alpha*cr(ic)/(dxx/100))
!      WRITE(6,'(a,F10.5,I5)') 'pilar alpha,nthet',alpha,nthet
      dt=alpha/nthet
      do t=1,nthet
	theta=angle(ic+1)-(t-1)*dt
	if(theta.gt.2*pi) theta=theta-2*pi
	axl_val=ec(axe,ic)+cr(ic)*dcos(theta)
	ayl_val=ec(axe+inc,ic)+cr(ic)*dsin(theta)
	if(t.eq.1) then
	  rtemp=ayl_val ; xlast=abs(xval-axl_val)
	else if(t.gt.1 .and. abs(xval-axl_val).lt.xlast) then
	  rtemp=ayl_val ; xlast=abs(xval-axl_val)
	end if
      end do
      rval=rtemp-p_jc
!      WRITE(6,'(a,3F10.5)') 'rtemp,p_jc,rval:',rtemp,p_jc,rval

      end subroutine
!-----------------------------------------------------------------------------------------
      subroutine pil_full_sect(n,ipts,isect,p_jc)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer,intent(in) :: n
      integer,intent(inout) :: ipts,isect
      double precision,intent(in) :: p_jc
      integer :: ir,nrn
      double precision :: r,dr
      
      nrn=nint(2*rval/dyy)
      dr=2*rval/nrn
      if(nrn.eq.0) then
	ipts=ipts+1 ; isect=isect+1
	xform(ipts,n,ipi)=xval
	yform(ipts,n,ipi)=p_jc
      else
      do ir=1,nrn
      r=rval-(ir-1)*dr
      ipts=ipts+1 ; isect=isect+1
      xform(ipts,n,ipi)=xval
      yform(ipts,n,ipi)=p_jc+r
      end do
      end if

      end subroutine
!=========================================================================================

! GUIDE VANES GEOMETRY
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine naca_blades(ng)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      
      integer,intent(in) :: ng
      double precision :: dist,ri,re,rot!,alpha
      integer :: ns,nlth,ipts,l,bs,isum,maxlmr
      double precision,dimension(100000) :: xloc
      
      
      maxlmr=maxval(lmr)
      dxx=dx/maxlmr
      dyy=dy/maxlmr
      dzz=dz/maxlmr

      ! Calculate the radius borne of the naca blades based on the distance between
      ! the bulb casing and the tunnel
      !Calculate the raduis 
      do i=1,b_nsect-1
        if(gcxloc(ng).ge.b_cxsect(i) .and. gcxloc(ng).le.b_cxsect(i+1)) then
	  ri=b_radsect(i)+b_dr(i)*(gcxloc(ng)-b_cxsect(i))+dyy
	end if
      end do 
      
      do i=1,t_nsect-1
	if(gcxloc(ng).ge.t_cxsect(i) .and. gcxloc(ng).le.t_cxsect(i+1)) then
	  re=t_radsect(i)+t_dr(i)*(gcxloc(ng)-t_cxsect(i))-dyy
	end if 
      end do
      
      dist=re-ri
      write(6,'(3(a,F6.3,X))') 'ri:',ri,'re:',re,'dist:',dist
      
      
! Normalised the percentages values of the different parameters of the naca airfoil-
      do ns=1,gnsect(ng)
	gchord(ns,ng)=gchord(ns,ng)/100*dist
	gmc(ns,ng)=gmc(ns,ng)/100!*gchord(ns,ng)
	gmcl(ns,ng)=gmcl(ns,ng)/100!*gchord(ns,ng)
	gthick(ns,ng)=gthick(ns,ng)/100!*gchord(ns,ng)
	gtwist(ns,ng)=-gtwist(ns,ng)/180*pi!+pi/2
        grloc(ns,ng)=ri+grloc(ns,ng)/100*dist
      end do
      
      do ns=1,gnsect(ng)
        write(6,*) '  '
	write(6,'(X,a,I3)') 'Section:', ns
	write(6,'(X,a,F10.7)') 'Chord:',gchord(ns,ng)
	write(6,'(2(X,a,F10.7))') 'Max Chamber:',gmc(ns,ng),'Location:',gmcl(ns,ng)
        write(6,'(2(X,a,F10.7))') 'Thickness:',gthick(ns,ng),'Twist:',gtwist(ns,ng)
	write(6,'(X,a,F10.7)') 'Section Location:',grloc(ns,ng)
      end do
      
! Create the differentials of all the parameters between each section of the naca 
! airfoil
      do i=1,gnsect(ng)-1
	d_c(i,ng)=(gchord(i+1,ng)-gchord(i,ng))/(grloc(i+1,ng)-grloc(i,ng))
	d_mc(i,ng)=(gmc(i+1,ng)-gmc(i,ng))/(grloc(i+1,ng)-grloc(i,ng))
        d_mcl(i,ng)=(gmcl(i+1,ng)-gmcl(i,ng))/(grloc(i+1,ng)-grloc(i,ng))
	d_thick(i,ng)=(gthick(i+1,ng)-gthick(i,ng))/(grloc(i+1,ng)-grloc(i,ng))
	d_twist(i,ng)=(gtwist(i+1,ng)-gtwist(i,ng))/(grloc(i+1,ng)-grloc(i,ng))
      end do
      
      do ns=1,gnsect(ng)-1
        write(6,*) '  '
	write(6,'(X,a,I3)') 'Part:', ns
	write(6,'(X,a,F10.7)') 'd_c:',d_c(ns,ng)
	write(6,'(2(X,a,F10.7))') 'd_mc:',d_mc(ns,ng),'d_mcl:',d_mcl(ns,ng)
        write(6,'(2(X,a,F10.7))') 'd_thick:',d_thick(ns,ng),'d_twist:',d_twist(ns,ng)
      end do


! Create the points for a straight naca blade---------------------------------------
      isum=1
      do ns=1,gnsect(ng)-1
         nlth=nint((grloc(ns+1,ng)-grloc(ns,ng))/dyy)
         do l=1,nlth
	   call naca_profile(ng,ns,l,isum,xloc)
         end do
      end do 
      ipg(ng)=isum-1
      
!      if(nguide.ne.0) then
!      open(unit=22,file='nacablade.dat')
!      do ipart=1,nguide
!	print *, 'ipg(ipart):', ipg(ipart)
!        do i=1,ipg(ipart)
!          write(22,112) xnaca(i,ipart),ynaca(i,ipart)+0.6,znaca(i,ipart)+0.190
!	end do
!      end do 
!      close(22)
!      end if
!112 format(3F25.7)
 
! Change adequately the radius position of the naca based on the points location on the
! bulb casing
      do bs=1,b_nsect
	do i=1,ipg(ng)
          if(xnaca(i,ng).ge.b_sect(bs) .and. xnaca(i,ng).le.b_sect(bs+1)) then
            ynaca(i,ng)=ynaca(i,ng)-(ri-(b_radsect(bs)+b_dr(i)*(xnaca(i,ng)-b_cxsect(bs))))
	  end if
        end do
      end do
      
      
!      do i=1,ipg(ng)
!	ynaca(i,ng)=b_cysect(1)+ynaca(i,ng)
!	znaca(i,ng)=b_czsect(1)+znaca(i,ng)
!      end do 

! Multiple and store the points position of all the guide vane blade
      ipts=0
      alpha=2.d0*pi/gnblade(ng)
      do l=1,gnblade(ng)+1
        do i=1,ipg(ng)
	  xval=xnaca(i,ng)
          yval=ynaca(i,ng)*cos(l*alpha)+znaca(i,ng)*sin(l*alpha)
	  zval=-ynaca(i,ng)*sin(l*alpha)+znaca(i,ng)*cos(l*alpha)
	  ipts=ipts+1
	  xnaca(ipts,ng)=xval ; ynaca(ipts,ng)=yval ; znaca(ipts,ng)=zval
        end do
      end do
      ipg(ng)=ipts

! Reposition the blade correctly relative to the position of the bulb turbine
      do i=1,ipg(ng)
	xnaca(i,ng)=xnaca(i,ng)+gcxloc(ng)
	do bs=1,b_nsect-1
	  if (gcxloc(ng).ge.b_cxsect(bs) .and. gcxloc(ng).le.b_cxsect(bs+1)) then
	    gcyloc(ng)=b_cysect(bs) ; gczloc(ng)=b_czsect(bs)
	  end if
	end do
	ynaca(i,ng)=ynaca(i,ng)+gcyloc(ng)
	znaca(i,ng)=znaca(i,ng)+gczloc(ng)
      end do

!      open(unit=20,file='nacablade.dat')
!      do i=1,ipg(ng)
!	write(20,'(3F25.10)') xnaca(i,ng),ynaca(i,ng),znaca(i,ng)
!      end do 
!      close(20)
      
      end subroutine
!-----------------------------------------------------------------------------------------
      subroutine naca_profile(ng,ns,l,isum)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      integer,intent(inout) :: ng,ns,l,isum
      integer :: nx,ipts,is
      double precision :: m,p,p1,c,tc,dist,rot,xold,zold,xc,zc,dzc,theta,zt
      
      
      yval=grloc(ns,ng)+(l-1)*dyy
      clth(l)=gchord(ns,ng)+d_c(ns,ng)*(yval-grloc(ns,ng))
      m=gmc(ns,ng)+d_mc(ns,ng)*(yval-grloc(ns,ng))
      p=gmcl(ns,ng)+d_mcl(ns,ng)*(yval-grloc(ns,ng))
      tc=gthick(ns,ng)+d_thick(ns,ng)*(yval-grloc(ns,ng))
      rot=gtwist(ns,ng)+d_twist(ns,ng)*(yval-grloc(ns,ng))
      
      if(l.eq.1) p1=p

      is=isum

! Screen printing of the different components
      write(6,*) '  '
      write(6,'(x,a,F10.5)') 'yval:', yval
      write(6,'(x,a,F10.5)') 'chord:', clth(l)
      write(6,'(x,a,F10.5)') 'max chamber:', m
      write(6,'(x,a,F10.5)') 'location:', p
      write(6,'(x,a,F10.5)') 'thickness:',tc
      write(6,'(x,a,F10.5)') 'rot',rot
      
! Creation of the naca perimeter of the section layer l
      ipts=0
      nxn=clth(l)/dxx
      write(6,'(x,a,I3)') 'nxn:',nxn
      do nx=1,nxn+2
        xc=(nx-1)*dxx/clth(l)
        if(xc.lt.p) then
          zc=m*(2*p*xc-xc**2)/p**2
          dzc=2*m*(p-xc)/p**2
        else
          zc=m*(1-2*p+2*p*xc-xc**2)/(1-p)**2
	  dzc=2*m*(p-xc)/(1-p)**2
        end if 
	theta=atan(dzc)
	zt=tc*(0.2969d0*xc**0.5-0.126d0*xc-0.3516d0*xc**2 &
     &  +0.2843d0*xc**3-0.1036d0*xc**4)/0.2
        ipts=ipts+1

! Top perimeter
        yntop(ipts,ng)=yval
        xntop(ipts,ng)=xc-zt*dsin(theta)
        zntop(ipts,ng)=zc+zt*dcos(theta)

! Bottom perimeter
        ynbot(ipts,ng)=yval
	xnbot(ipts,ng)=xc+zt*dsin(theta)
        znbot(ipts,ng)=zc-zt*dcos(theta)
      end do
      
! Transfering the top and bottom twisted perimeter to the global blade array      
      do i=1,ipts
        xnaca(isum,ng)=xntop(i,ng) 
	ynaca(isum,ng)=yntop(i,ng)
	znaca(isum,ng)=zntop(i,ng)
        xnaca(isum+1,ng)=xnbot(i,ng)
        ynaca(isum+1,ng)=ynbot(i,ng)
        znaca(isum+1,ng)=znbot(i,ng)
	isum=isum+2
      end do 

! Twisting equation of the blades
      do i=is,isum+1
	xold=xnaca(i,ng) ; zold=znaca(i,ng)
	xnaca(i,ng)=(p1+(xnaca(i,ng)-p1)*cos(rot)-zold*sin(rot))
	znaca(i,ng)=((xnaca(i,ng)-p1)*sin(rot)+zold*cos(rot))
      end do 

! Adding the chord length of each section to the NACA perimeter ratio
      do i=is,isum+1
        xnaca(i,ng)=xnaca(i,ng)*clth(l)
        znaca(i,ng)=znaca(i,ng)*clth(l)
      end do

      end subroutine


! INPUT FILE:
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine read_infile
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none 
      
      character(80) :: dummyline
      integer :: id,ierr,no,io,psimple,pcomplex,ii

      open(unit=10,file='inputV2.6.cin',status='old',iostat=ierr)
      if(ierr.ne.0) then
	write(6,*) 'Input file not found'
	STOP
      end if
      ! computional domain----------------------------------------------------------------
      do i=1,3
        read(10,*) dummyline
      end do
      read(10,*) dx,dy,dz
      read(10,*) dummyline
      read(10,*) dxi,dxe
      read(10,*) dyi,dye
      read(10,*) dzi,dze
      do i=1,3
	read(10,*) dummyline
      end do
      read(10,*) LRM
      if(LRM.eq..FALSE.) lmr(:)=1
      read(10,*) nzone
      read(10,*) dummyline
      read(10,*) dummyline
      if(nzone.ne.0) then
      do i=1,nzone
	read(10,*) zone(i), xlmr(i,1),xlmr(i,2),lmr(i)
      end do
      end if
      
      ! tunnel geometry-------------------------------------------------------------------
      do i=1,5
	read(10,*) dummyline
      end do 
      read(10,*) txi,txe
      read(10,*) tyi,tye
      read(10,*) tzi,tze
      if(nzone.eq.0) then
	nzone=1 ; zone(nzone)=1 ; xlmr(nzone,1)=txi ; xlmr(nzone,2)=txe ; lmr(nzone)=1
      end if
      do i=1,3
	read(10,*) dummyline
      end do 
      read(10,*) t_nsect
      do i=1,2
	read(10,*) dummyline
      end do
      no=0
      if(t_nsect.ne.0) then
      do i=1,t_nsect
      read(10,*) t_sect(i),t_geomtype(i),t_cxsect(i),t_cysect(i),t_czsect(i),t_radsect(i), &
     &		 t_height(i),t_base(i)!,t_bezier(i)
      if(t_geomtype(i).eq.3) no=no+1
      end do
      end if
      do i=1,4
	read(10,*) dummyline
      end do
      if(no.ne.0) then
      do i=1,no
      read(10,*) io,t_ocxsect(io),t_ocysect(io),t_oczsect(io),t_oheight(io),t_obase(io)
      end do
      end if

!      read(10,*) nbez
!      read(10,*) dummyline
!      read(10,*) dummyline
!      if(nbez.ne.0) then
!      do i=1,nbez
!	t_bsec, bezpts(t_bsec,1), bezpts(t_bsec,2), bezpts(t_bsec,3), bezpts(t_bsec,4)
!      end do
!      end if
      do i=1,3
	read(10,*) dummyline
      end do
      read(10,*) fullness
      read(10,*) tlay
      
     ! bulb geometry---------------------------------------------------------------------
      do i=1,3
        read(10,*) dummyline
      end do 
      read(10,*) b_nsect
      read(10,*) dummyline
      read(10,*) dummyline
      if(b_nsect.ne.0) then
      do i=1,b_nsect
      read(10,*) b_sect(i), b_geomtype(i), b_cxsect(i), b_cysect(i), b_czsect(i), b_radsect(i)
      end do 
      end if
            
      ! donut part:
      read(10,*) dummyline
      read(10,*) ndonut
      do i=1,2
	read(10,*) dummyline
      end do 
      if(ndonut.ne.0) then
	do id=1,ndonut
	  read(10,*) donut(id),d_cxsect(id,1),d_cxsect(id,2),bsize(id)
	end do
      end if 

      ! bezier curve:
      read(10,*) dummyline
      read(10,*) nbez
      do i=1,2
        read(10,*) dummyline
      end do 
      if(nbez.ne.0) then
      do i=1,nbez
        read(10,*) sec, bezpts(sec,1), bezpts(sec,2), bezpts(sec,3), bezpts(sec,4)
      end do 
      end if 
      do i=1,3
	read(10,*) dummyline
      end do
      read(10,*) b_fullness
      read(10,*) blay


      ! pilar geometry--------------------------------------------------------------------
      psimple=0 ; pcomplex=0
      do i=1,3
        read(10,*) dummyline
      end do
      read(10,*) npil
      read(10,*) dummyline
      read(10,*) dummyline
      if(npil.ne.0) then
        do i=1,npil
          read(10,*) io, p_type(io), orientation(io)
	  if(p_type(i).eq.1) psimple=psimple+1
	  if(p_type(i).eq.2) pcomplex=pcomplex+1
	end do 
      end  if
      
      do i=1,4
	read(10,*) dummyline
      end do
      
      if(psimple.ne.0) then
	do i=1,psimple
	  read(10,*) io,p_cxsect(1,io),p_cysect(1,io),p_ax(io),p_by(io)
	end do
      end if

      do i=1,3
	read(10,*) dummyline
      end do

      if(pcomplex.ne.0) then
	do i=1,pcomplex
	  read(10,*) io
	  read(10,*) p_nsect(io)
	  read(10,*) dummyline
	  read(10,*) dummyline
	  do ii=1,p_nsect(io)
      read(10,*) p_sect(ii,io),p_geomtype(ii,io),p_cxsect(ii,io),p_cysect(ii,io),p_radsect(ii,io)
	  end do
	end do
      else
	do i=1,4
	  read(10,*) dummyline
	end do
      end if
      
      do i=1,3
        read(10,*) dummyline
      end do
      read(10,*) p_fullness
      read(10,*) play



     
      ! guide vanes-----------------------------------------------------------------------
      do i=1,3
	read(10,*) dummyline
      end do
      read(10,*) nguide
      if(nguide.eq.0) then
        do i=1,8
	  read(10,*) dummyline
        end do
      else
        do i=1,nguide
	  read(10,*) dummyline
	  read(10,*) gnblade(i)
	  read(10,*) gnsect(i)
	  read(10,*) gcxloc(i)
	  do id=1,5
	    read(10,*) dummyline
	  end do 
	  do id=1,gnsect(i)
            read(10,*) gsect(id,i),gchord(id,i),gmc(id,i),gmcl(id,i), &
     &                gthick(id,i),gtwist(id,i),grloc(id,i)
	  end do 
	end do
      end if 
      close(10)
       
!      if(LRM.eq..TRUE. .and. fullness.eq..TRUE.) then
!	write(6,*) 'LRM and Fullness cannot be activated together'
!	STOP
!      end if

      end subroutine

!-----------------------------------------------------------------------------------------
      subroutine screen_display
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none

      character(6) :: str_orien
      character(9) :: b_type
      character(9) :: t_type
      integer :: id

      ! PRINTING THE FILE ONTO SCREEN-----------------------------------------------------
      write(6,*) '========================= Hub Generation Input========================='
      write(6,*) 'COMPUTATIONAL DOMAIN'
      write(6,*) '#######################################################################'
      write(6,'(3F6.3,X,a)') dx,dy,dz,'dx,dy,dz'
      write(6,*) '-----------------------------------------------------------------------'
      write(6,'(2F6.3,2X,a)') dxi,dxe, 'x-boundaries'
      write(6,'(2F6.3,2X,a)') dyi,dye, 'y-boundaries'
      write(6,'(2F6.3,2X,a)') dzi,dze, 'z-boundaries'
      write(6,*) '-----------------------------------------------------------------------'
      write(6,'(2X,L1,X,a)') LRM, 'LRM'

      IF(t_nsect.ne.0) THEN
      write(6,*) '#######################################################################'
      write(6,*) 'TUNNEL GEOMETRY'
      write(6,*) '#######################################################################'
      write(6,*) 'Tunnel Case Dimensions:'
      write(6,*) '======================================================================='
      write(6,'(2F6.3,2X,a)') txi,txe, 'x-boundaries'
      write(6,'(2F6.3,2X,a)') tyi,tye, 'y-boundaries'
      write(6,'(2F6.3,2X,a)') tzi,tze, 'z-boundaries'
      write(6,*) '======================================================================='
      write(6,*) 'Tunnel Circular Sections:'
      write(6,*) '======================================================================='
      write(6,'(I3,X,a)') t_nsect, 'Sections'      
      write(6,*) '-----------------------------------------------------------------------'
      write(6,*) 'Number:    Type:    Cx-loc:    Cy-loc:    Cz-loc:    Raduis:'
      do i=1,t_nsect
      if(t_geomtype(i).eq.1) t_type='Cir'
      if(t_geomtype(i).eq.2) t_type='Sqr'
      write(6,100) t_sect(i), t_type, t_cxsect(i), t_cysect(i), t_czsect(i), t_radsect(i)
      end do
      write(6,*) '======================================================================='
      write(6,*) 'Tunnel Solidity:'
      write(6,*) '======================================================================='
      write(6,'(2X,L1,X,a)') fullness, 'Full Section'
      write(6,'(I3,X,a)') tlay, 'Number of layers' 
      END IF
100   format(I7,6X,a3,4F11.3)     


!      IF(npil.ne.0) THEN
!      write(6,*) '#######################################################################'
!      write(6,*) 'Pilar Case'
!      write(6,*) '#######################################################################'
!      write(6,'(I3,X,a)') npil, 'Pilars'
!      write(6,*) '-----------------------------------------------------------------------'
!      write(6,*) 'Number:   ax-length:   by-length:   Cx-loc:   Cy-loc:   Orientation:'
!      do i=1,npil
!	if(orientation(i).eq.1) str_orien='Top'
!	if(orientation(i).eq.-1) str_orien='Bot'
!	write(6,110) ipil(i),axpil(i),bypil(i),cxpil(i),cypil(i),str_orien
!      end do 
!      END IF
110 format(I7,2F13.3,2F10.3,12X,a3)


      IF(b_nsect.ne.0) THEN
      write(6,*) '#######################################################################'
      write(6,*) 'BULB GEOMETRY'
      write(6,*) '#######################################################################'
      write(6,'(I3,X,a)') b_nsect, 'Sections' 
      write(6,*) '-----------------------------------------------------------------------'
      write(6,*) 'Number:    Type:    Cx-loc:    Cy-loc:    Cz-loc:    Raduis:'
      do i=1,b_nsect
      if(b_geomtype(i).eq.1 .or. b_geomtype(i).eq.2) b_type='Cone'
      if(b_geomtype(i).eq.3) b_type='Cyl'	      
      write(6,105) b_sect(i), b_type, b_cxsect(i), b_cysect(i), b_czsect(i), b_radsect(i)
      end do 
      if(nlin.ne.0) then
      write(6,*) '======================================================================='
      write(6,*) 'Cone Parts'
      write(6,*) '======================================================================='
      write(6,'(I3,X,a)') nlin, 'Linear Sections'
      write(6,*) '-----------------------------------------------------------------------'
      write(6,*) 'Number:    Tip-cxloc:'
      end if
      if(nbez.ne.0) then
      write(6,*) '-----------------------------------------------------------------------'
      write(6,'(I3,X,a)') nbez, 'Cubic Bezier Curve:'
      write(6,*) '-----------------------------------------------------------------------'
      write(6,*) 'Number:   a point:   b point:   c point:   d point:'
      do i=1,b_nsect
        if(b_geomtype(i).eq.2) then
          write(6,'(I7,3F11.3)') b_sect(i),bezpts(i,1),bezpts(i,2),bezpts(i,3),bezpts(i,4)
   	end if
      end do
      end if
      END IF
105   format(I7,6X,a3,4F11.3) 
      
      IF(nguide.ne.0) THEN
      write(6,*) '#######################################################################'
      write(6,*) 'GUIDE VANES'
      write(6,*) '#######################################################################'
      write(6,'(X,I3,X,a)') nguide, 'Guide Vane'
      do i=1,nguide
      write(6,*) '======================================================================='
      write(6,'(X,I6,X,a)') gnblade(i), 'Number of Blades'
      write(6,'(X,I6,X,a)') gnsect(i), 'Section Blades'
      write(6,'(X,F6.2,a)') gcxloc(i), 'Cx-location'
      write(6,*) '-----------------------------------------------------------------------'
      write(6,*) 'Section spec'
      write(6,*) '-----------------------------------------------------------------------'
      write(6,*) 'Number:   Chord:   MC:   MC-Loc:   Thick:   Twist:    Blade length:'
      do id=1,gnsect(i)
        write(6,115) gsect(id,i),gchord(id,i),gmc(id,i),gmcl(id,i), &
     &               gthick(id,i),gtwist(id,i),grloc(id,i)
      end do
      end do
      END IF
115   format(I7,F9.2,F6.2,F10.2,2F9.2,F17.2)

      write(6,*) '#######################################################################'

      end subroutine screen_display
