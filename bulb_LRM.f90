! Purpose: Create the hub geometry from an input file.

! Dates:                         Modifications:
! ========                       =======================
! 28/08/18                       Implementation of the bulb geometry
! 29/08/18			 Implementation of the pilar geometry
! 30/08/18                       Implementation of the tunnel geometry
! 01/09/18			 Implementation of the guide vane
! 05/09/18                       Implementation of the rotational guide vanes
! 30/10/18			 Fullness of tunnel fixed
! 
! ========                       =======================
      
!=========================================================================================
      module hub_var
!=========================================================================================
      implicit none 
      
      ! Main------------------------------------------------------------------------------
      double precision,parameter :: pi=4.d0*atan(1.d0)
      integer :: i,ipart,totpts,nxn
      integer :: t,nthet
      double precision :: xval,yval,zval,rval
      double precision :: dx,dy,dz
      character(80) :: fn
      ! Domain----------------------------------------------------------------------------
      double precision :: dxi,dxe,dyi,dye,dzi,dze
      ! LMR ------------------------------------------------------------------------------
      logical :: LRM
      integer :: nzone,temp_nparts,temp_nsect
      double precision,dimension(50) :: zone
      double precision,dimension(50,2) :: xlmr
      integer, dimension(50) :: lmr,temp_lmr
      double precision,dimension(50) :: temp_cxsect,temp_cysect,temp_czsect,temp_radsect
      double precision,dimension(50) :: temp_djc,temp_dkc,temp_dr

      ! TUNNEL GEOMETRY-------------------------------------------------------------------
      integer :: t_nsect,tti,ttj,ttk,tlay,t_nparts
      integer,dimension(50) :: itun,t_lmr
      ! Tunnel case:
      double precision :: txi,txe,tyi,tye,tzi,tze
      double precision :: xitc,xetc,yitc,yetc,zitc,zetc
      double precision,dimension(10000000,3) :: casepts
      ! Inside tube:
      integer,dimension(50) :: t_sect,t_geomtype,t_scale
      double precision,dimension(50) :: t_cxsect,t_cysect,t_czsect,t_radsect
      double precision,dimension(50) :: t_dr,t_djc,t_dkc
      double precision,dimension(10000000,3) :: tubpts
      logical :: fullness
      ! Tunnel face:
      double precision,dimension(10000000,3) :: facepts

     
      ! Bulb Case-------------------------------------------------------------------------
      integer :: sec,b_nsect,nlin,nbez,b_nparts,start
      integer,dimension(50) :: b_sect,b_geomtype,ip,b_lmr
      double precision,dimension(50) :: b_cxsect,b_cysect,b_czsect,b_radsect
      double precision,dimension(50) :: tipcx
      double precision,dimension(50,4) :: bezpts
      double precision,dimension(50) :: b_dr,b_djc,b_dkc
      double precision,dimension(100000,3,20) :: bulpts
      ! Pilar Case------------------------------------------------------------------------
      integer :: npil
      integer,dimension(50) :: ipil
      double precision,dimension(50) :: axpil,bypil,cxpil,cypil,czpil,orientation
      double precision,dimension(100000,3,10) :: pilpts


      ! Guide Vanes-----------------------------------------------------------------------
      integer :: nguide
      integer,dimension(10) :: gnsect,gnblade,ipg
      integer,dimension(10,10) :: gsect
      double precision,dimension(10) :: gcxloc
      double precision,dimension(10,10) :: gchord,gmc,gmcl,gthick,gtwist,grloc
      double precision,dimension(10,10) :: d_c,d_mc,d_mcl,d_thick,d_twist
      double precision,dimension(1000000,10) :: xnaca,ynaca,znaca 
      double precision,dimension(1000000,10) :: xntop,yntop,zntop
      double precision,dimension(1000000,10) :: xnbot,ynbot,znbot
      double precision,dimension(1000000) :: clth
      double precision,parameter :: dxx=0.0001,dyy=0.001,dzz=0.0001      


!=========================================================================================
      end module
!=========================================================================================






!#########################################################################################
!#########################################################################################
      program hub_generation
      use hub_var
      implicit none 
      
      integer :: n,np,ng
      double precision :: rad

      call read_infile
       
!      call screen_display
      call tunnel

      call bulb_case_generation
      
      ! Create the guide vanes geometry points--------------------------------------------
      do ng=1,nguide
        call naca_blades(ng)
      end do
      
      ! Creating the geometry points for the pilars---------------------------------------
      do np=1,npil
	call pilar_generation(np)
      end do
      
      ! Printing on the screen the total number of points---------------------------------
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
       write(19,*) 'ZONE T=TFACE'
	do i=1,itun(1)
          write(19,112) facepts(i,1),facepts(i,2),facepts(i,3)
	end do
	if(fullness.eq..FALSE.) write(19,*) 'ZONE T=TCASE'
	do i=1,itun(2)
          write(19,112) casepts(i,1),casepts(i,2),casepts(i,3)
        end do
	if(fullness.eq..FALSE.) write(19,*) 'ZONE T=TTUN'
        do i=1,itun(3)
          write(19,112) tubpts(i,1),tubpts(i,2),tubpts(i,3)
	end do
      close(19)
      end if
      
      if(b_nsect.ne.0) then
      open(unit=20,file='bulb.dat')
      write(20,*) 'ZONE T=BULB'
	Do ipart=1,b_nparts
          do i=1,ip(ipart)
          write(20,112) bulpts(i,1,ipart),bulpts(i,2,ipart),bulpts(i,3,ipart)
	  end do
	End do
      close(20)
      end if
      
      if(npil.ne.0) then
      open(unit=21,file='pilar.dat')
        write(21,*) 'ZONE T=PILART'
        Do ipart=1,np
          do i=1,ipil(ipart)
          write(21,112) pilpts(i,1,ipart),pilpts(i,2,ipart),pilpts(i,3,ipart)
	  end do
	End do
      close(21)
      end if

      if(nguide.ne.0) then
      open(unit=22,file='nacablade.dat')
      write(22,*) 'ZONE T=NACA'
      do ipart=1,nguide
	print *, 'ipg(ipart):', ipg(ipart)
        do i=1,ipg(ipart)
          write(22,112) xnaca(i,ipart)+1.5,ynaca(i,ipart)+0.6,znaca(i,ipart)+0.19
	end do
      end do 
      close(22)
      end if

      112 format(3F25.7)


      end program
!#########################################################################################
!#########################################################################################


! TUNNEL GEOMETRY
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine tunnel
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      
      integer :: itt,a,b,sneak
      double precision :: t_jc,t_kc,dist,n,xinit
      integer :: l,iz,c,is,submax,ix,m
      
      ipart=0
      do i=1,t_nsect-1
      ipart=ipart+1
      t_dr(ipart)=(t_radsect(i+1)-t_radsect(i))/(t_cxsect(i+1)-t_cxsect(i))
      t_djc(ipart)=(t_cysect(i+1)-t_cysect(i))/(t_cxsect(i+1)-t_cxsect(i))
      t_dkc(ipart)=(t_czsect(i+1)-t_czsect(i))/(t_cxsect(i+1)-t_cxsect(i))
      end do
      t_nparts=ipart
      
      call lmr_resolution(1) 

      xitc=txi+tlay*(dx/t_lmr(1))
      xetc=txe-tlay*(dx/t_lmr(t_nsect-1))
      
      print *,'xitc,zetc',xitc,xetc
      do n=1,t_nsect
	write(6,'(F10.5)') t_dr(n)
      end do 

      a=0
      b=0
      m=0
      DO l=1,tlay
        ipart=0
        Do n=1,t_nsect-1
	  ipart=ipart+1
	  write(6,'(3F10.5)') t_cxsect(n),t_cxsect(n+1),t_dr(n)
	  nxn=nint((t_cxsect(n+1)-t_cxsect(n))/(dx/t_lmr(ipart)))
	  rval=0
          do ix=1,nxn
	    xval=t_cxsect(n) + (ix-1)*(dx/t_lmr(ipart))
!	    write(6,*) xval
	    t_jc=t_cysect(n) + t_djc(ipart)*(ix-1)*(dx/t_lmr(ipart))
	    t_kc=t_czsect(n) + t_dkc(ipart)*(ix-1)*(dx/t_lmr(ipart))
	    rval=t_radsect(n) + (l-1)*(dy/t_lmr(ipart)) + t_dr(ipart)*(ix-1)*(dx/t_lmr(ipart))
	    IF(fullness.eq..TRUE.) THEN
	      call tunnel_face(a,t_jc,t_kc)
	    ELSE
	    if(xval.lt.xitc .or. xval.ge.xetc) then
	      if(l.eq.1) call tunnel_face(a,t_jc,t_kc)
	    else
	      nthet=nint(2*pi*rval/(dy/t_lmr(ipart)))
	      do t=1,nthet
	        b=b+1
	        tubpts(b,1)=xval
	        tubpts(b,2)=t_jc+rval*dcos(2.d0*pi/nthet*(t-1))
	        tubpts(b,3)=t_kc+rval*dsin(2.d0*pi/nthet*(t-1))
	      end do
	      call tunnel_case(l)
	    end if
	    END IF
          end Do
        End DO
      END do
      itun(1)=a
      itun(3)=b
      end subroutine

!-----------------------------------------------------------------------------------------
      subroutine tunnel_case(l)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      integer,intent(in) :: l
      integer :: itc,jtc,ktc,it
      
      yitc=tyi+(l-1)*(dy/t_lmr(ipart)) ; zitc=tzi+(l-1)*(dz/t_lmr(ipart))
      yetc=tye-(l-1)*(dy/t_lmr(ipart)) ; zetc=tze-(l-1)*(dz/t_lmr(ipart))

      tti=nint((txe-txi)/(dx/t_lmr(ipart)))
      ttj=nint((tye-tyi)/(dy/t_lmr(ipart))) 
      ttk=nint((tze-tzi)/(dz/t_lmr(ipart)))
      
      it=itun(2)
      Do jtc=1,ttj+1
	yval=tyi+(jtc-1)*(dy/t_lmr(ipart))
	IF(yval.le.yitc .or. yval.ge.yetc) THEN
	  do ktc=1,ttk+1
	    zval=tzi+(ktc-1)*(dz/t_lmr(ipart))
            it=it+1 ; casepts(it,1)=xval ; casepts(it,2)=yval ; casepts(it,3)=zval
	  end do
	ELSE
	  do ktc=1,ttk+1
	    zval=tzi+(ktc-1)*(dz/t_lmr(ipart))
	     if(zval.le.zitc .or. zval.ge.zetc) then
	       it=it+1 ; casepts(it,1)=xval ; casepts(it,2)=yval ; casepts(it,3)=zval
	     end if 
	  end do 
	END IF
      End do
      itun(2)=it

      end subroutine

!-----------------------------------------------------------------------------------------
      subroutine tunnel_face(a,t_jc,t_kc)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
 
      integer,intent(inout) :: a
      double precision,intent(in) :: t_jc,t_kc
      integer :: nalpha,m
      double precision :: ri,re,dr,ir,ttr
      
	      m=m+1
	      write(6,*) m,'ENTRY'
	      write(6,*) 'xval',xval
      ttj=nint((tye-tyi)/(dy/t_lmr(ipart))) ; ttk=nint((tze-tzi)/(dz/t_lmr(ipart)))

      if(ttj.ge.ttk) then
	ri=rval ; re=tye+10*dy ; dr=(dy/t_lmr(ipart))
      else
	ri=rval ; re=tze+10*dz ; dr=(dz/t_lmr(ipart))
      end if
      a=itun(1)     
      ttr=(re-rval)/(dz/t_lmr(ipart))
      DO ir=1,ttr
        rval=ri+(ir-1)*dr
	nalpha=nint(2*pi*rval/dr)
	do t=1,nalpha
	  yval=t_jc+rval*dcos(2.d0*pi/nalpha*(t-1))
	  zval=t_kc+rval*dsin(2.d0*pi/nalpha*(t-1))
	  IF(yval.ge.tyi .and. yval.le.tye) THEN
	    if(zval.ge.tzi .and. zval.le.tze) then
	      a=a+1 ; facepts(a,1)=xval ; facepts(a,2)=yval ; facepts(a,3)=zval
	    end if
	  END IF
	end do
      END DO
      itun(1)=a
      end subroutine


! BULB GEOMETRY:
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine bulb_case_generation
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      
      integer :: nlim,n,nparts,ix,iz,c,submax,d,is
      double precision :: b_jc,b_kc
   
      ! Calculates and store radius slope------------------------------------------------
      ipart=0
      write(6,*) 'Bulb gradients'
      do n=1,b_nsect-1
	ipart=ipart+1
	b_djc(n)=(b_cysect(n+1)-b_cysect(n))/(b_cxsect(n+1)-b_cxsect(n))
	b_dkc(n)=(b_czsect(n+1)-b_czsect(n))/(b_cxsect(n+1)-b_cxsect(n))
	b_dr(n)=(b_radsect(n+1)-b_radsect(n))/(b_cxsect(n+1)-b_cxsect(n))
      end do
      b_nparts=ipart
      print *, '====================================================================='
      print *, 'b_nparts_before',b_nparts,b_nsect      
      call lmr_resolution(2)

      print *, 'b_nparts_after',b_nparts,b_nsect
      ! Screen display the radius slope---------------------------------------------------
      ipart=0
      write(6,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      do n=1,b_nparts
	write(6,'(X,a,I3,X,a,F5.3)') 'Part:',n, 'Radius Slope:', b_dr(n)
      end do 
      write(6,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

      ! Calculates and store the points of the bulb case---------------------------------
      ipart=0
      DO n=1,b_nsect-1
      ipart=ipart+1 ; i=0 
      write(6,'(a,3F10.5)') 'b_dr',b_dr(ipart),b_cxsect(n),b_cxsect(n+1)
      nxn=nint((b_cxsect(n+1)-b_cxsect(n))/(dx/b_lmr(ipart)))
      do ix=1,nxn
	xval=b_cxsect(n) + (ix-1)*(dx/b_lmr(ipart))
	b_jc=b_cysect(n) + b_djc(ipart)*(ix-1)*(dx/b_lmr(ipart))
	b_kc=b_cysect(n) + b_dkc(ipart)*(ix-1)*(dx/b_lmr(ipart))
        rval=b_radsect(n)+ b_dr(ipart)*(ix-1)*(dx/b_lmr(ipart))
        nthet=nint(2.d0*pi*rval/dy)
	IF(nthet.eq.0) THEN
	  i=i+1
	  bulpts(i,1,ipart)=xval
	  bulpts(i,2,ipart)=b_jc
	  bulpts(i,3,ipart)=b_kc
	ELSE
	  do t=1,nthet
	    i=i+1
	    bulpts(i,1,ipart)=xval
            bulpts(i,2,ipart)=b_jc + rval*dcos(2.d0*pi/nthet*(t-1))
	    bulpts(i,3,ipart)=b_kc + rval*dsin(2.d0*pi/nthet*(t-1))
	  end do
	END IF
      end do
      ip(ipart)=i ; write(6,111) 'Part:',ipart,'/',b_nparts,'written || Points:', ip(ipart) 
      END DO
      111 format(X,a,X,I2,X,a,I2,X,a,I6)

      end subroutine
      
!LMR RESOLUTION
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine lmr_resolution(cas)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      integer, intent(in) :: cas
      integer :: nl,nlim,m,is,ib,b,n
      double precision :: dist

     
      Select case(cas)
      Case (1)
      temp_cxsect = t_cxsect ; temp_radsect = t_radsect	; temp_nparts = t_nparts
      temp_cysect = t_cysect ; temp_djc	    = t_djc	; temp_nsect  = t_nsect
      temp_czsect = t_czsect ; temp_dkc	    = t_dkc	; temp_lmr    = t_lmr
      temp_dr	  = t_dr
	
      call lmr_solver(start)

      t_cxsect = temp_cxsect ; t_radsect = temp_radsect ; t_nparts = temp_nparts
      t_cysect = temp_cysect ; t_djc     = temp_djc	; t_nsect  = temp_nsect
      t_czsect = temp_czsect ; t_dkc	 = temp_dkc	; t_lmr	   = temp_lmr
      t_dr     = temp_dr
      
      Case (2)
      temp_cxsect = b_cxsect ; temp_radsect = b_radsect	; temp_nparts = b_nparts
      temp_cysect = b_cysect ; temp_djc	    = b_djc	; temp_nsect  =	b_nsect
      temp_czsect = b_czsect ; temp_dkc	    = b_dkc	; temp_lmr    = b_lmr
      temp_dr	  = b_dr

      call lmr_solver(start)

      b_cxsect = temp_cxsect ; b_radsect = temp_radsect ; b_nparts = temp_nparts
      b_cysect = temp_cysect ; b_djc     = temp_djc	; b_nsect  = temp_nsect
      b_czsect = temp_czsect ; b_dkc	 = temp_dkc     ; b_lmr	   = temp_lmr
      b_dr     = temp_dr
      end select 

      end subroutine
!-----------------------------------------------------------------------------------------
      subroutine lmr_solver
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      integer :: nl,nlim,m,is,ib,b,n
      double precision :: dist

      nlim=temp_nsect
      do i=1,nzone
	do nl=1,nlim
	  if(xlmr(i,1).gt.temp_cxsect(nl) .and. xlmr(i,1).lt.temp_cxsect(nl+1)) then
	    is=nl+1
	     do m=nlim+1,is,-1
	       temp_cxsect(m)=temp_cxsect(m-1)
	       temp_cysect(m)=temp_cysect(m-1)
	       temp_czsect(m)=temp_czsect(m-1)
	       temp_radsect(m)=temp_radsect(m-1)
	       temp_djc(m)=temp_djc(m-1)
	       temp_dkc(m)=temp_dkc(m-1)
	       temp_dr(m)=temp_dr(m-1)
	     end do
	     nlim=nlim+1
	     dist=xlmr(i,1)-temp_cxsect(nl)
	     temp_cxsect(is)=xlmr(i,1)
	     temp_cysect(is)=temp_cysect(nl)+temp_djc(is)*dist
	     temp_czsect(is)=temp_czsect(nl)+temp_dkc(is)*dist
	     temp_radsect(is)=temp_radsect(nl)+temp_dr(is)*dist
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

!     TEST--------------------------------------------------------------------------------
!      do nl=1,temp_nsect
!      write(6,'(I3,3F10.5,I3)') nl,temp_cxsect(nl),temp_radsect(nl),temp_dr(nl),temp_lmr(nl)
!      end do
!      write(6,*) '=================================================================='

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
      double precision :: dist,ri,re,rot,alpha
      integer :: ns,nlth,ipts,l,bs,isum
      double precision,dimension(100000) :: xloc

          

      ! Calculate the radius borne of the naca blades based on the distance between
      ! the bulb casing and the tunnel
      ipart=0
      do i=1,b_nsect
	ipart=ipart+1
        if(gcxloc(ng).ge.b_cxsect(i) .and. gcxloc(ng).le.b_cxsect(i+1)) then
	  ri=b_radsect(i)+b_dr(ipart)*(gcxloc(ng)-b_cxsect(i))+dy
	end if
      end do 
      
      ipart=0
      do i=1,t_nsect
	ipart=ipart+1
	if(gcxloc(ng).ge.t_cxsect(i) .and. gcxloc(ng).le.t_cxsect(i+1)) then
	  re=t_radsect(i)+t_dr(ipart)*(gcxloc(ng)-b_cxsect(i))
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
!      ipart=0
!      do bs=1,b_nsect
!	ipart=ipart+1
!	do i=1,ipg(ng)
!          if(xnaca(i,ng).ge.b_sect(bs) .and. xnaca(i,ng).le.b_sect(bs+1)) then
!            ynaca(i,ng)=b_radsect(bs)+b_dr(ipart)*(xnaca(i,ng)-b_cxsect(bs))+dyy
!	  end if
!        end do
!      end do
      
      
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

      write(6,*) '  '
      write(6,'(x,a,F10.5)') 'yval:', yval
      write(6,'(x,a,F10.5)') 'chord:', clth(l)
      write(6,'(x,a,F10.5)') 'max chamber:', m
      write(6,'(x,a,F10.5)') 'location:', p
      write(6,'(x,a,F10.5)') 'thickness:',tc
      write(6,'(x,a,F10.5)') 'rot',rot
      
      is=isum
! Creation of the naca perimeter of the section layer l.
      ipts=0
      nxn=clth(l)/dxx
      write(6,'(x,a,I3)') 'nxn:',nxn
      do nx=1,nxn+2
        xc=(nx-1)*dxx/clth(l)
        if(xc.lt.p) then
          zc=m*(2*p*xc-xc**2)/p**2
          dzc=2*m*(p-xc)/p**2
!	  write(6,*) zc(nx),dzc(nx)
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

! Pablo twistin
      do i=is,isum+1
	xold=xnaca(i,ng) ; zold=znaca(i,ng)
	xnaca(i,ng)=(p1+(xnaca(i,ng)-p1)*cos(rot)-zold*sin(rot))!*clth(l)
	znaca(i,ng)=((xnaca(i,ng)-p1)*sin(rot)+zold*cos(rot))!*clth(l)
      end do 

! Adding the chord length of each section to the NACA perimeter ratio
      do i=is,isum+1
        xnaca(i,ng)=xnaca(i,ng)*clth(l)
        znaca(i,ng)=znaca(i,ng)*clth(l)
      end do

      end subroutine

! PILAR GEOMETRY
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine pilar_generation(np)
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none
      
      integer,intent(in) :: np
      double precision :: z,cz,zi,ze,rslope_temp,radstart,r,el_x,el_y,el_z
      double precision :: cxpilmin,cxpilmax,cx_start,ri,re
      integer :: partn,nlay,nrn,nr,ipts,l
       
      ipts=0
      DO l=1,tlay
      nthet=nint((2*pi*axpil(np)-(l-1)*dx)/dx)
      print *,nthet
      do t=1,nthet
	el_x=cxpil(np)+(axpil(np)-(l-1)*dx)*dcos(2.d0*pi/nthet*(t-1))
	el_y=cypil(np)+(bypil(np)-(l-1)*dy)*dsin(2.d0*pi/nthet*(t-1))
	ipart=0
	do i=start,b_nsect
	  ipart=ipart+1
	  if(el_x.ge.b_cxsect(i) .and. el_x.le.b_cxsect(i+1)) then
	    ri=(b_radsect(i)+b_dr(ipart)*(el_x-b_cxsect(i)))+dz
	  end if
	end do
	ipart=0
        do i=1,t_nsect-1
	  ipart=ipart+1
	  if(el_x.ge.t_cxsect(i) .and. el_x.le.t_cxsect(i+1)) then
	    re=t_radsect(i)+t_dr(ipart)*(el_x-t_cxsect(i))
	  end if
	end do 
	czpil(np)=t_czsect(i)
	nrn=nint((re-ri)/dz)
	do nr=1,nrn
	  ipts=ipts+1	
	  rval=ri+(nr-1)*dz
	  pilpts(ipts,1,np)=el_x
	  pilpts(ipts,2,np)=el_y
	  pilpts(ipts,3,np)=czpil(np)+sqrt(rval**2-(el_y-cypil(np))**2)*orientation(np)
	end do
      end do 
      END DO
      ipil(np)=ipts


      write(6,'(x,a,I9)') 'Total Pilar Points:', ipil(np)

      end subroutine pilar_generation
!=========================================================================================

! INPUT FILE:
!=========================================================================================
!-----------------------------------------------------------------------------------------
      subroutine read_infile
!-----------------------------------------------------------------------------------------
      use hub_var
      implicit none 
      
      character(1) :: logchar,st,sf
      character(80) :: dummyline
      integer :: id,ierr

      open(unit=10,file='test.cin',status='old',iostat=ierr)
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
      if(t_nsect.ne.0) then
      do i=1,t_nsect
      read(10,*) t_sect(i),t_geomtype(i),t_cxsect(i),t_cysect(i),t_czsect(i),t_radsect(i)
      end do
      end if
      do i=1,3
	read(10,*) dummyline
      end do
      read(10,*) fullness
      read(10,*) tlay

      ! pilar geometry--------------------------------------------------------------------
      do i=1,3
        read(10,*) dummyline
      end do
      read(10,*) npil
      read(10,*) dummyline
      read(10,*) dummyline
      if(npil.ne.0) then
        do i=1,npil
          read(10,*) ipil(i), axpil(i) , bypil(i), cxpil(i), cypil(i), orientation(i)
	end do 
      end  if

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
	  do id=1,4
	    read(10,*) dummyline
	  end do 
	  do id=1,gnsect(i)
            read(10,*) gsect(id,i),gchord(id,i),gmc(id,i),gmcl(id,i), &
     &                gthick(id,i),gtwist(id,i),grloc(id,i)
	  end do 
	end do
      end if 
      close(10)

!      open(12,file='infodom.cin',status='old',iostat=ierr)
!      if(ierr.ne.0) then
!	write(6,*) 'infodom file Not Found'
!	STOP
!      end if 
!!      read(12,*) nbdoms
!      read(12,*) dummyline
!      do i=1,nbdoms
!      read(12,*) ndo(i),reso(i),xlrm(i,1),xlrm(i,2),ylrm(i,1),ylrm(i,2),zlrm(i,1),zlrm(i,2)
!      end do
!      read(12,*) dummyline
!      read(12,*) i_div
!      read(12,*) j_div
!      read(12,*) k_div
!      close(12)
       
!      call subdomain_print
      if(LRM.eq..TRUE. .and. fullness.eq..TRUE.) then
	write(6,*) 'LRM and Fullness cannot be activated together'
	STOP
      end if 

!      open(28,file='infodomcopy.cin')
!      write(28,'(I4)') nbdoms
!      write(28,'(a)') '====================================================================='
!      do i=1,nbdoms
!      write(28,29) ndo(i),reso(i),xlrm(i,1),xlrm(i,2),ylrm(i,1),ylrm(i,2),zlrm(i,1),zlrm(i,2)
!      end do
!      write(28,'(a)') '==================================================================='
!      write(28,*) i_div
!      write(28,*) j_div
!      write(28,*) k_div
!      close(28)
!29    format(2I4,6F15.5)


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


      IF(npil.ne.0) THEN
      write(6,*) '#######################################################################'
      write(6,*) 'Pilar Case'
      write(6,*) '#######################################################################'
      write(6,'(I3,X,a)') npil, 'Pilars'
      write(6,*) '-----------------------------------------------------------------------'
      write(6,*) 'Number:   ax-length:   by-length:   Cx-loc:   Cy-loc:   Orientation:'
      do i=1,npil
	if(orientation(i).eq.1) str_orien='Top'
	if(orientation(i).eq.-1) str_orien='Bot'
	write(6,110) ipil(i),axpil(i),bypil(i),cxpil(i),cypil(i),str_orien
      end do 
      END IF
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
      do i=1,b_nsect
	if(b_geomtype(i).eq.1) then
	  write(6,'(I7,F14.3)') b_sect(i), tipcx(i)
	end if 
      end do 
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
      
