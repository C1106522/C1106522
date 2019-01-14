!##########################################################################
        program post_processing
!##########################################################################
        implicit none
        double precision, pointer, dimension(:,:,:) :: x,y,z
        double precision, pointer, dimension(:,:,:) :: p,pm,ppm,vis,vism
        double precision, pointer, dimension(:,:,:) :: u,um,uum
        double precision, pointer, dimension(:,:,:) :: v,vm,vvm
        double precision, pointer, dimension(:,:,:) :: w,wm,wwm
        double precision, pointer, dimension(:,:,:) :: uvm,uwm,vwm
        double precision, pointer, dimension(:,:,:) :: eps,epsm,ksgs
        double precision :: dx,dy,dz,ug,vg,wg
        double precision :: u_cn,v_cn,w_cn,um_cn,vm_cn,wm_cn,pm_cn,p_cn
        double precision :: uum_cn,vvm_cn,wwm_cn,uvml,uwml,vwml,vis_cn
        double precision :: UCN,VCN,WCN,UMCN,VMCN,WMCN,ppm_cn
        double precision :: uumCN,vvmCN,wwmCN,vism_cn,eps_cn,epsm_cn
        integer          :: i,j,k,tt,sn,n,npl
        integer          :: tti,ttj,ttk,toti,totj,totk
        integer          :: inind,jnind,knind
        character*8      :: chb
        character*25     :: gf
	    character*60	 :: command
      logical :: bulb_case
      integer :: t_nsect,m,ns,mi,mj,ic
      double precision :: dist,xval,yval,zval,rval,t_jc,t_kc,t_h,t_b
      double precision :: yi,ye,zi,ze
      integer,dimension(100) :: t_geomtype
      double precision,dimension(100) :: t_cxsect,t_cysect,t_czsect
      double precision,dimension(100) :: t_djc,t_dkc
      double precision,dimension(100) :: t_radsect,t_height,t_base
      double precision,dimension(100) :: t_dr,t_dh,t_db
      double precision,dimension(100) :: dxx,dyy,dzz
      double precision,dimension(10,2) :: tran_sect,tran_edge
      double precision,dimension(100) :: ecy,ecz,ecr
      double precision,dimension(100,2) :: edge,pt
      double precision :: A,B,C,D,rp
      double precision :: y1,y2,y3,z1,z2,z3


! variables=x,y,z,P,PM,ppm,vis,U,UM,uuM,V,VM,vvM,W,WM,wwM,uvM,uwM,vwM

        print*,'please enter the number of domains',   
     & ' (ie. how many tecplot files do you have?'
        read(6,'(i8)')tt
        print*,'you have ',tt,' number of domains!'

        do n=0,tt-1

           write(chb,'(i8)') n
           sn=len(trim(adjustl(chb)))
           chb=repeat('0',(4-sn))//trim(adjustl(chb))
           gf='tecbin'//trim(adjustl(chb))//'.bin'
           open (unit=700, file=gf, form='unformatted',status='old')

           read (700) tti,ttj,ttk
           read (700) npl
           read (700) inind,jnind,knind
           write(6,'(i3,a,3i7,3i3)') n,' dom--> ',tti,ttj,ttk

	   allocate(eps(tti,ttj,ttk),epsm(tti,ttj,ttk),ksgs(tti,ttj,ttk))
           allocate(x(tti,ttj,ttk),y(tti,ttj,ttk),z(tti,ttj,ttk))
           allocate(u(tti,ttj,ttk),um(tti,ttj,ttk),uum(tti,ttj,ttk))
           allocate(v(tti,ttj,ttk),vm(tti,ttj,ttk),vvm(tti,ttj,ttk))
           allocate(w(tti,ttj,ttk),wm(tti,ttj,ttk),wwm(tti,ttj,ttk))
           allocate(uvm(tti,ttj,ttk),uwm(tti,ttj,ttk),vwm(tti,ttj,ttk))
           allocate(p(tti,ttj,ttk),pm(tti,ttj,ttk),vis(tti,ttj,ttk))
           allocate(ppm(tti,ttj,ttk),vism(tti,ttj,ttk))

           do k=1,ttk
           do j=1,ttj
           do i=1,tti
              read (700) x(i,j,k),y(i,j,k),z(i,j,k),	      
     & p(i,j,k),pm(i,j,k),ppm(i,j,k),vis(i,j,k),vism(i,j,k),  
     & u(i,j,k),um(i,j,k),uum(i,j,k),			      
     & v(i,j,k),vm(i,j,k),vvm(i,j,k),			      
     & w(i,j,k),wm(i,j,k),wwm(i,j,k),			      
     & uvm(i,j,k),uwm(i,j,k),vwm(i,j,k),		      
     & ksgs(i,j,k),eps(i,j,k)!,epsm(i,j,k)		      
           end do
           end do
           end do
           close (700)

! Bulb Turbine Case
!#########################################################################################
      

      WRITE(6,*) 'Is it a bulb turbine case?'
      read(*,*) bulb_case
	   
      if(bulb_case.eq..TRUE.) then
	     
      open(unit=23,file='tunnel_spec.txt')
      do m=1,3
	read(23,*)
      end do
      read(23,*) t_nsect
	      
      do ns=1,t_nsect
	read(23,*) m,t_geomtype(m),t_cxsect(m),t_cysect(m),t_czsect(m),
     &             t_radsect(m),t_height(m),t_base(m),t_djc(m),t_dkc(m),
     &             t_dr(m),t_dh(m),t_db(m),dxx(m),dyy(m),dzz(m) 
      end do
      close(23)

	     
	     
      do i=1,tti ; do k=1,ttk ; do j=1,ttj
	xval=x(i,j,k) ; yval=y(i,j,k) ; zval=z(i,j,k)
	do ns=1,t_nsect-1
	  if(xval.ge.t_cxsect(ns) .and. xval.lt.t_cxsect(ns+1)) then
		   
	  IF(t_geomtype(ns).eq.1 .and. t_geomtype(ns).eq.t_geomtype(ns+1)) THEN
	    dist=xval-t_geomtype(ns)
	    t_jc=t_cysect(ns)+t_djc(ns)*dist
	    t_kc=t_czsect(ns)+t_dkc(ns)*dist
	    rval=t_radsect(ns)+t_dr(ns)*dist
	    rp=sqrt((yval-t_jc)**2+(zval-t_kc)**2)
	    if(rp.ge.rval) then
	      u(i,j,k)=0   ; um(i,j,k)=0  ; uum(i,j,k)=0
	      v(i,j,k)=0   ; vm(i,j,k)=0  ; vvm(i,j,k)=0
	      w(i,j,k)=0   ; wm(i,j,k)=0  ; wwm(i,j,k)=0
	      p(i,j,k)=0   ; pm(i,j,k)=0  ; ppm(i,j,k)=0
	      uvm(i,j,k)=0 ; uwm(i,j,k)=0 ; vwm(i,j,k)=0
	      vis(i,j,k)=0 ; vism(i,j,k)=0 ; ksgs(i,j,k)=0 ; eps(i,j,k)=0 !;epsm(i,j,k)=0
	    end if
		   
           ELSE IF(t_geomtype(ns).eq.2 .and. 
     &             t_geomtype(ns).eq.t_geomtype(ns+1)) THEN
	    dist=xval-t_geomtype(ns)
	    t_jc=t_cysect(ns)+t_djc(ns)*dist
		t_kc=t_czsect(ns)+t_dkc(ns)*dist
		t_h=t_height(ns)+t_dh(ns)*dist
		t_b=t_base(ns)+t_db(ns)*dist
		yi=t_jc-t_b/2 ; zi=t_kc-t_h/2
		ye=t_kc+t_b/2 ; ze=t_kc+t_b/2
		if(yval.le.yi .or. yval.ge.ye) then
		  u(i,j,k)=0   ; um(i,j,k)=0  ; uum(i,j,k)=0
		  v(i,j,k)=0   ; vm(i,j,k)=0  ; vvm(i,j,k)=0
		  w(i,j,k)=0   ; wm(i,j,k)=0  ; wwm(i,j,k)=0
		  p(i,j,k)=0   ; pm(i,j,k)=0  ; ppm(i,j,k)=0
		  uvm(i,j,k)=0 ; uwm(i,j,k)=0 ; vwm(i,j,k)=0
		  vis(i,j,k)=0 ; vism(i,j,k)=0 ; ksgs(i,j,k)=0 ; eps(i,j,k)=0 !;espm(i,j,k)=0
		else
		  if(zval.le.zi .or. zval.ge.ze) then
		    u(i,j,k)=0   ; um(i,j,k)=0  ; uum(i,j,k)=0
		    v(i,j,k)=0   ; vm(i,j,k)=0  ; vvm(i,j,k)=0
		    w(i,j,k)=0   ; wm(i,j,k)=0  ; wwm(i,j,k)=0
		    p(i,j,k)=0   ; pm(i,j,k)=0  ; ppm(i,j,k)=0
		    uvm(i,j,k)=0 ; uwm(i,j,k)=0 ; vwm(i,j,k)=0
		    vis(i,j,k)=0 ; vism(i,j,k)=0 ; ksgs(i,j,k)=0 ; eps(i,j,k)=0 !;espm(i,j,k)=0
		  end if
		end if
		  
	    ELSE IF(t_geomtype(ns).ne.t_geomtype(ns+1) .or.	
     &	            (t_geomtype(ns).eq.3 .and.			
     &	            t_geomtype(ns).eq.t_geomtype(ns+1))) then
	      
	      dist=xval-t_cxsect(ns)
	      do mi=1,8 ; do mj=1,2
		edge(mi,mj)=tran_sect(mi,mj) + tran_edge(mi,mj)*dist
	      end do
	      edge(9,1)=edge(1,1) ; edge(9,2)=edge(1,2)

	      do ic=1,7,2
	      pt(ic,1)=edge(ic,1)     ; pt(ic,2)=edge(ic,2)  
	      pt(ic+1,1)=edge(ic+1,1) ;	pt(ic+1,2)=edge(ic,2)
	      pt(ic+2,1)=edge(ic+2,1) ; pt(ic+2,2)=edge(ic+2,2)

	      y1=pt(ic,1)   ; z1=pt(ic,2) 
	      y2=pt(ic+1,1) ; z2=pt(ic+1,2) 
	      y3=pt(ic+2,1) ; z3=pt(ic+2,2)      
	  A=y1*(z2-z3)-z1*(y2-y3)+y2*z3-y3*z2
	  B=(y1**2+z1**2)*(z3-z2)+(y2**2+z2**2)*(z1-z3)+(y3**2+z3**2)*(z2-z1)
	  C=(y1**2+z1**2)*(y2-y3)+(y2**2+z2**2)*(y3-y1)+(y3**2+z3**2)*(y1-y2)
	  D=(y1**2+z1**2)*(y3*z2-y2*z3)+(y2**2+z2**2)*(y1*z3-y3*z1)+  
     &	    (y3**2+z3**2)*(y2*z1-y1*z2)
	      B=B/A ; C=C/A ; D=D/A ; A=1
	      ecy(ic)=-(B/2*A) ; ecz(ic)=-(C/2*A) 
	      ecr(ic)=sqrt((ecy(ic)-y1)**2+(ecz(ic)-z1)**2)
	      
	      SELECT CASE(ic)
		      
	      CASE(1)
	      if(zval.ge.edge(ic,2))then
		rp=sqrt((yval-ecy(ic))**2+(zval-ecz(ic))**2)
	        if(rp.ge.ecr(ic)) then
		  u(i,j,k)=0   ; um(i,j,k)=0  ; uum(i,j,k)=0
		  v(i,j,k)=0   ; vm(i,j,k)=0  ; vvm(i,j,k)=0
		  w(i,j,k)=0   ; wm(i,j,k)=0  ; wwm(i,j,k)=0
		  p(i,j,k)=0   ; pm(i,j,k)=0  ; ppm(i,j,k)=0
		  uvm(i,j,k)=0 ; uwm(i,j,k)=0 ; vwm(i,j,k)=0
		  vis(i,j,k)=0 ; vism(i,j,k)=0 ; ksgs(i,j,k)=0 ; eps(i,j,k)=0 !;espm(i,j,k)=0
		end if
	      end if
		      
	      CASE(5) 
	      if(zval.le.edge(ic,2)) then
		rp=sqrt((yval-ecy(ic))**2+(zval-ecz(ic))**2)
		if(rp.ge.ecr(ic)) then
		  u(i,j,k)=0   ; um(i,j,k)=0  ; uum(i,j,k)=0
		  v(i,j,k)=0   ; vm(i,j,k)=0  ; vvm(i,j,k)=0
		  w(i,j,k)=0   ; wm(i,j,k)=0  ; wwm(i,j,k)=0
		  p(i,j,k)=0   ; pm(i,j,k)=0  ; ppm(i,j,k)=0
		  uvm(i,j,k)=0 ; uwm(i,j,k)=0 ; vwm(i,j,k)=0
		  vis(i,j,k)=0 ; vism(i,j,k)=0 ; ksgs(i,j,k)=0 ; eps(i,j,k)=0 !;espm(i,j,k)=0
		end if
	      end if

	      CASE(3)
	      if(yval.le.edge(ic,1)) then
		rp=sqrt((yval-ecy(ic))**2+(zval-ecz(ic))**2)
		if(rp.ge.ecr(ic)) then
		  u(i,j,k)=0   ; um(i,j,k)=0  ; uum(i,j,k)=0
		  v(i,j,k)=0   ; vm(i,j,k)=0  ; vvm(i,j,k)=0
		  w(i,j,k)=0   ; wm(i,j,k)=0  ; wwm(i,j,k)=0
		  p(i,j,k)=0   ; pm(i,j,k)=0  ; ppm(i,j,k)=0
		  uvm(i,j,k)=0 ; uwm(i,j,k)=0 ; vwm(i,j,k)=0
		  vis(i,j,k)=0 ; vism(i,j,k)=0 ; ksgs(i,j,k)=0 ; eps(i,j,k)=0 !;espm(i,j,k)=0
		end if
	      end if

	      CASE(7) 
	      if(yval.ge.edge(ic,1)) then
		rp=sqrt((yval-ecy(ic))**2+(zval-ecz(ic))**2)
		if(rp.ge.ecr(ic)) then
		  u(i,j,k)=0   ; um(i,j,k)=0  ; uum(i,j,k)=0
		  v(i,j,k)=0   ; vm(i,j,k)=0  ; vvm(i,j,k)=0
		  w(i,j,k)=0   ; wm(i,j,k)=0  ; wwm(i,j,k)=0
		  p(i,j,k)=0   ; pm(i,j,k)=0  ; ppm(i,j,k)=0
		  uvm(i,j,k)=0 ; uwm(i,j,k)=0 ; vwm(i,j,k)=0
		  vis(i,j,k)=0 ; vism(i,j,k)=0 ; ksgs(i,j,k)=0 ; eps(i,j,k)=0 !;espm(i,j,k)=0
		end if
	      end if

	      END SELECT
	    end do
	    END IF
	    end if
	  end do
	end do ;  end do ; end do
	end if

!#########################################################################################	

        dx=x(2,2,2)-x(1,1,1)
        dy=y(2,2,2)-y(1,1,1)
        dz=z(2,2,2)-z(1,1,1)

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! NEED To BE DELETED LATER (as soon as exchange modification is applied)
           do j=2,ttj
           do i=2,tti
              k=1
              uvm(i,j,k)=3.0*uvm(i,j,k+1)-3.0*uvm(i,j,k+2)+uvm(i,j,k+3)
              uwm(i,j,k)=3.0*uwm(i,j,k+1)-3.0*uwm(i,j,k+2)+uwm(i,j,k+3)
              vwm(i,j,k)=3.0*vwm(i,j,k+1)-3.0*vwm(i,j,k+2)+vwm(i,j,k+3)
           end do
           end do

           do k=1,ttk
           do i=2,tti
              j=1
              uvm(i,j,k)=3.0*uvm(i,j+1,k)-3.0*uvm(i,j+2,k)+uvm(i,j+3,k)
              uwm(i,j,k)=3.0*uwm(i,j+1,k)-3.0*uwm(i,j+2,k)+uwm(i,j+3,k)
              vwm(i,j,k)=3.0*vwm(i,j+1,k)-3.0*vwm(i,j+2,k)+vwm(i,j+3,k)
           end do
           end do

           do k=1,ttk
           do j=1,ttj
              i=1
              uvm(i,j,k)=3.0*uvm(i+1,j,k)-3.0*uvm(i+2,j,k)+uvm(i+3,j,k)
              uwm(i,j,k)=3.0*uwm(i+1,j,k)-3.0*uwm(i+2,j,k)+uwm(i+3,j,k)
              vwm(i,j,k)=3.0*vwm(i+1,j,k)-3.0*vwm(i+2,j,k)+vwm(i+3,j,k)
           end do
           end do

        gf='tecturb'//trim(adjustl(chb))//'.dat'
        open (unit=88, file=gf)

        toti=tti-2*npl+1; totj=ttj-2*npl+1; totk=ttk-2*npl+1

        write (88,*) 'title = turb'
        write (88,*)
     &  'variables=x,y,z,p,pm,ppm,vis,vism,u,v,w,um,vm,wm,uum,vvm,wwm',
     &  ',uvm,uwm,vwm,eps,epsm'
        write (88,*)'zone  i=',toti,', ',
     &  ' j=',totj,', k= ',totk,' f=point'

        do k=npl,ttk-npl
        do j=npl,ttj-npl
        do i=npl,tti-npl

                 u_cn  =0.25*(u(i,j,k)+
     &u(i,j+1,k)+u(i,j,k+1)+
     &u(i,j+1,k+1))
                 um_cn  =0.25*(um(i,j,k)+
     &um(i,j+1,k)+um(i,j,k+1)+
     &um(i,j+1,k+1))
                 uum_cn  =0.25*(uum(i,j,k)+
     &uum(i,j+1,k)+uum(i,j,k+1)+
     &uum(i,j+1,k+1))

                 v_cn  =0.25*(v(i,j,k)+
     &v(i+1,j,k)+v(i,j,k+1)+
     &v(i+1,j,k+1))
                 vm_cn  =0.25*(vm(i,j,k)+
     &vm(i+1,j,k)+vm(i,j,k+1)+
     &vm(i+1,j,k+1))
                 vvm_cn  =0.25*(vvm(i,j,k)+
     &vvm(i+1,j,k)+vvm(i,j,k+1)+
     &vvm(i+1,j,k+1))

                 w_cn  =0.25*(w(i,j,k)+
     &w(i+1,j,k)+w(i,j+1,k)+
     &w(i+1,j+1,k)) 
                 wm_cn  =0.25*(wm(i,j,k)+
     &wm(i+1,j,k)+wm(i,j+1,k)+
     &wm(i+1,j+1,k)) 
                 wwm_cn  =0.25*(wwm(i,j,k)+
     &wwm(i+1,j,k)+wwm(i,j+1,k)+
     &wwm(i+1,j+1,k)) 

                 p_cn  =0.125*(p(i,j,k)+
     &p(i+1,j,k)    +p(i,j+1,k)+
     &p(i+1,j+1,k)  +p(i,j,k+1)+
     &p(i+1,j,k+1)  +p(i,j+1,k+1)+
     &p(i+1,j+1,k+1))
                 pm_cn  =0.125*(pm(i,j,k)+
     &pm(i+1,j,k)    +pm(i,j+1,k)+
     &pm(i+1,j+1,k)  +pm(i,j,k+1)+
     &pm(i+1,j,k+1)  +pm(i,j+1,k+1)+
     &pm(i+1,j+1,k+1))
		     ppm_cn =0.125*(ppm(i,j,k)+
     &ppm(i+1,j,k)    +ppm(i,j+1,k)+
     &ppm(i+1,j+1,k)  +ppm(i,j,k+1)+
     &ppm(i+1,j,k+1)  +ppm(i,j+1,k+1)+
     &ppm(i+1,j+1,k+1))
                 vis_cn  =0.125*(vis(i,j,k)+
     &vis(i+1,j,k)    +vis(i,j+1,k)+
     &vis(i+1,j+1,k)  +vis(i,j,k+1)+
     &vis(i+1,j,k+1)  +vis(i,j+1,k+1)+
     &vis(i+1,j+1,k+1))

                 vism_cn  =0.125*(vism(i,j,k)+
     &vism(i+1,j,k)    +vism(i,j+1,k)+
     &vism(i+1,j+1,k)  +vism(i,j,k+1)+
     &vism(i+1,j,k+1)  +vism(i,j+1,k+1)+
     &vism(i+1,j+1,k+1))

                 uvml  =0.125*(uvm(i,j,k)+
     &uvm(i+1,j,k)    +uvm(i,j+1,k)+
     &uvm(i+1,j+1,k)  +uvm(i,j,k+1)+
     &uvm(i+1,j,k+1)  +uvm(i,j+1,k+1)+
     &uvm(i+1,j+1,k+1))
                 uwml  =0.125*(uwm(i,j,k)+
     &uwm(i+1,j,k)    +uwm(i,j+1,k)+
     &uwm(i+1,j+1,k)  +uwm(i,j,k+1)+
     &uwm(i+1,j,k+1)  +uwm(i,j+1,k+1)+
     &uwm(i+1,j+1,k+1))
                 vwml  =0.125*(vwm(i,j,k)+
     &vwm(i+1,j,k)    +vwm(i,j+1,k)+
     &vwm(i+1,j+1,k)  +vwm(i,j,k+1)+
     &vwm(i+1,j,k+1)  +vwm(i,j+1,k+1)+
     &vwm(i+1,j+1,k+1))

                 eps_cn  =0.25*(eps(i,j,k)+
     &eps(i,j+1,k)+eps(i,j,k+1)+
     &eps(i,j+1,k+1))
                 epsm_cn  =0.25*(epsm(i,j,k)+
     &epsm(i,j+1,k)+epsm(i,j,k+1)+
     &epsm(i,j+1,k+1))


                 write (88,88) x(i,j,k),y(i,j,k),z(i,j,k),
     &  p_cn,pm_cn,ppm_cn,vis_cn,vism_cn,
     &  u_cn,v_cn,w_cn,um_cn,vm_cn,wm_cn,
     &  uum_cn,vvm_cn,wwm_cn,uvml,uwml,vwml,eps_cn,epsm_cn

        end do
        end do
        end do

        close (88)

           deallocate(x,y,z,uvm,uwm,vwm,p,pm,ppm,vis,vism)
           deallocate(u,um,uum,v,vm,vvm,w,wm,wwm)

!	CHANGE THE PATH TO THE CORRESPONDING ONE ON YOUR COMPUTER---------------------------------------------
	command='/opt/tecplot360/bin/preplot '//trim(adjustl(gf))

		CALL SYSTEM (command)

        end do

88      format (24e25.8)
        end program
!##########################################################################
