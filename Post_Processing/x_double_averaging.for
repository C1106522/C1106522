
        program x_avg
!##########################################################################
 !Make sure that tecbin variables are read as they are written in your version of the code.THIS can be checked
! in post.for in which tecbins are written. In this case there are 20 variables written in tecbins, therefore dm is 20
! and dmk is 20. If your version of the code does not write ksgs and eps in tecbins, these variables should be removed.  

	implicit none
	


	double precision,pointer, dimension (:,:,:) :: avy,avz
	double precision,pointer, dimension (:,:,:) :: avp,avpm,avppm
	double precision,pointer, dimension (:,:,:) :: avvis,avvism
	double precision,pointer, dimension (:,:,:) :: avu,avum,avuum
	double precision,pointer, dimension (:,:,:) :: avv,avvm,avvvm
	double precision,pointer, dimension (:,:,:) :: avw,avwm,avwwm
	double precision,pointer, dimension (:,:,:) :: avuvm,avuwm,avvwm
	double precision,pointer, dimension (:,:,:) :: avksgs,aveps,avepsm


	double precision,pointer, dimension (:,:,:) :: x,y,z
	double precision,pointer, dimension (:,:,:) :: p,pm,ppm
	double precision,pointer, dimension (:,:,:) :: vis,vism
	double precision,pointer, dimension (:,:,:) :: u,um,uum
	double precision,pointer, dimension (:,:,:) :: v,vm,vvm
	double precision,pointer, dimension (:,:,:) :: w,wm,wwm
	double precision,pointer, dimension (:,:,:) :: uvm,uwm,vwm
	double precision,pointer, dimension (:,:,:) :: ksgs,eps,epsm

	double precision, pointer, dimension (:,:,:) :: avfy,avfz
	double precision, pointer, dimension (:,:,:) :: avfp,avfpm,avfppm
	double precision, pointer, dimension (:,:,:) :: avfu,avfum,avfuum
	double precision, pointer, dimension (:,:,:) :: avfv,avfvm,avfvvm
	double precision, pointer, dimension (:,:,:) :: avfw,avfwm,avfwwm
	double precision, pointer, dimension (:,:,:) :: avfuvm,avfuwm,avfvwm
	double precision, pointer, dimension (:,:,:) :: avfvis,avfvism
	double precision, pointer, dimension (:,:,:) :: avfksgs,avfeps
	double precision, pointer, dimension (:,:,:) :: avfepsm
 	double precision, dimension(20) :: dm
        double precision, pointer, dimension(:,:,:) :: dmk


        double precision, pointer, dimension(:) :: dx,dy,dz
        integer         , pointer, dimension(:) :: tti,ttj,ttk


        double precision :: visc,u_tau,zpl

        integer          :: i,j,k,tt,ttx,sn,n,t,r,npl,ttyz
        integer          :: maxttj,maxttk,Re,caseno
        integer          :: inind,jnind,knind
        character*8      :: chb
        character*25     :: gf

! variables=x,y,z,P,PM,ppm,vis,U,UM,uuM,V,VM,vvM,W,WM,wwM,uvM,uwM,vwM

        print*,'please enter the number of domains',
     & ' (ie. how many tecplot files do you have?'
        read(*,'(i8)')tt
        print*,'you have ',tt,' domains!'

        print*,'please enter the number of domains IN X DIRECTION?'
        read(*,'(i8)')ttx
        print*,'you have ',ttx,' domains IN X DIRECTION!'

        allocate(dx(tt),dy(tt),dz(tt))
        allocate(tti(tt),ttj(tt),ttk(tt))
!	  allocate(minx(tt),miny(tt),minz(tt))
!	  allocate(maxx(tt),maxy(tt),maxz(tt))

!        minx=10000.0; miny=10000.0; minz=10000.0
!        maxx=-10000.0; maxy=-10000.0; maxz=-10000.0

	  maxttj=-1
        maxttk=-1
        do n=0,tt-1
           t=n+1
           write(chb,'(i8)') n
           sn=len(trim(adjustl(chb)))
           chb=repeat('0',(4-sn))//trim(adjustl(chb))
           gf='tecbin'//trim(adjustl(chb))//'.bin'
           open (unit=700+n, file=gf, form='unformatted',status='old')

           read (700+n) tti(t),ttj(t),ttk(t)

           maxttj=(max(maxttj,ttj(t)))
           maxttk=(max(maxttk,ttk(t)))
        end do

        allocate(avy(tt,maxttj,maxttk))
        allocate(avz(tt,maxttj,maxttk),avp(tt,maxttj,maxttk))
	allocate(avpm(tt,maxttj,maxttk),avppm(tt,maxttj,maxttk))
	allocate(avvis(tt,maxttj,maxttk),avvism(tt,maxttj,maxttk))
        allocate(avu(tt,maxttj,maxttk),avum(tt,maxttj,maxttk))
	allocate(avuum(tt,maxttj,maxttk))
        allocate(avv(tt,maxttj,maxttk),avvm(tt,maxttj,maxttk))
	allocate(avvvm(tt,maxttj,maxttk))
        allocate(avw(tt,maxttj,maxttk),avwm(tt,maxttj,maxttk))
	allocate(avwwm(tt,maxttj,maxttk))
        allocate(avuvm(tt,maxttj,maxttk),avuwm(tt,maxttj,maxttk))
	allocate(avvwm(tt,maxttj,maxttk))
    	allocate(avksgs(tt,maxttj,maxttk),aveps(tt,maxttj,maxttk))
	allocate(avepsm(tt,maxttj,maxttk))

        do n=0,tt-1
           t=n+1

           read (700+n) npl
           read (700+n) inind,jnind,knind
!           write(6,'(i3,a,3i7,1i3)') n,' npl--> ',npl
           write(6,'(i3,a,3i7,3i3)') n,' dom--> ',tti(t),ttj(t),ttk(t)

           allocate(x(tti(t),ttj(t),ttk(t)),y(tti(t),ttj(t),ttk(t)))
           allocate(z(tti(t),ttj(t),ttk(t)))
           allocate(u(tti(t),ttj(t),ttk(t)),um(tti(t),ttj(t),ttk(t)))
           allocate(uum(tti(t),ttj(t),ttk(t)))
           allocate(v(tti(t),ttj(t),ttk(t)),vm(tti(t),ttj(t),ttk(t)))
           allocate(vvm(tti(t),ttj(t),ttk(t)))
           allocate(w(tti(t),ttj(t),ttk(t)),wm(tti(t),ttj(t),ttk(t)))
           allocate(wwm(tti(t),ttj(t),ttk(t)))
           allocate(uvm(tti(t),ttj(t),ttk(t)),uwm(tti(t),ttj(t),ttk(t)))
           allocate(vwm(tti(t),ttj(t),ttk(t)))
           allocate(p(tti(t),ttj(t),ttk(t)),pm(tti(t),ttj(t),ttk(t)))
           allocate(ppm(tti(t),ttj(t),ttk(t)),vis(tti(t),ttj(t),ttk(t)))
	   allocate(vism(tti(t),ttj(t),ttk(t)),ksgs(tti(t),ttj(t),ttk(t)))
	   allocate(eps(tti(t),ttj(t),ttk(t)),epsm(tti(t),ttj(t),ttk(t)))
!!!Reading variables from tec bins,adjust according to your code version

           do k=1,ttk(t)
           do j=1,ttj(t)
           do i=1,tti(t)
              read (700+n) x(i,j,k),y(i,j,k),z(i,j,k),
     & p(i,j,k),pm(i,j,k),ppm(i,j,k),vis(i,j,k),vism(i,j,k),
     & u(i,j,k),um(i,j,k),uum(i,j,k),
     & v(i,j,k),vm(i,j,k),vvm(i,j,k),
     & w(i,j,k),wm(i,j,k),wwm(i,j,k),
     & uvm(i,j,k),uwm(i,j,k),vwm(i,j,k),
     & ksgs(i,j,k),eps(i,j,k),epsm(i,j,k)

           end do
           end do
           end do
           close (700+n)

           dx(t)=x(npl+1,npl+1,npl+1)-x(npl,npl,npl)
           dy(t)=y(npl+1,npl+1,npl+1)-y(npl,npl,npl)
           dz(t)=z(npl+1,npl+1,npl+1)-z(npl,npl,npl)

 !          minx(t)=min(minx(t),x(npl,npl,npl))
 !          miny(t)=min(miny(t),y(npl,npl,npl))
 !          minz(t)=min(minz(t),z(npl,npl,npl))
 !          maxx(t)=max(maxx(t),x(tti(t)-npl,ttj(t)-npl,ttk(t)-npl))
 !          maxy(t)=max(maxy(t),y(tti(t)-npl,ttj(t)-npl,ttk(t)-npl))
 !          maxz(t)=max(maxz(t),z(tti(t)-npl,ttj(t)-npl,ttk(t)-npl))

! This loops promediates in x direction for every block

           do j=1,ttj(t)
           do k=1,ttk(t)
              dm=0.0
           do i=1,tti(t)
              dm(1)=dm(1)+p(i,j,k)
              dm(2)=dm(2)+pm(i,j,k)
              dm(3)=dm(3)+ppm(i,j,k)
              dm(4)=dm(4)+vis(i,j,k)
	      dm(5)=dm(5)+vism(i,j,k)
              dm(6)=dm(6)+u(i,j,k)
              dm(7)=dm(7)+um(i,j,k)
              dm(8)=dm(8)+uum(i,j,k)
              dm(9)=dm(9)+v(i,j,k)
              dm(10)=dm(10)+vm(i,j,k)
              dm(11)=dm(11)+vvm(i,j,k)
              dm(12)=dm(12)+w(i,j,k)
              dm(13)=dm(13)+wm(i,j,k)
              dm(14)=dm(14)+wwm(i,j,k)
              dm(15)=dm(15)+uvm(i,j,k)
              dm(16)=dm(16)+uwm(i,j,k)
              dm(17)=dm(17)+vwm(i,j,k)
	      dm(18)=dm(18)+ksgs(i,j,k)
	      dm(19)=dm(19)+eps(i,j,k)
	      dm(20)=dm(20)+epsm(i,j,k)
           end do
		  avy(t,j,k)=y(1,j,k)
              avz(t,j,k)=z(1,j,k)
              avp(t,j,k)=dm(1)/(tti(t)-2*npl)
              avpm(t,j,k)=dm(2)/(tti(t)-2*npl)
              avppm(t,j,k)=dm(3)/(tti(t)-2*npl)
              avvis(t,j,k)=dm(4)/(tti(t)-2*npl)
	      avvism(t,i,k)=dm(5)/(tti(t)-2*npl)
              avu(t,j,k)=dm(6)/(tti(t)-2*npl)
              avum(t,j,k)=dm(7)/(tti(t)-2*npl)
              avuum(t,j,k)=dm(8)/(tti(t)-2*npl)
              avv(t,j,k)=dm(9)/(tti(t)-2*npl)
              avvm(t,j,k)=dm(10)/(tti(t)-2*npl)
              avvvm(t,j,k)=dm(11)/(tti(t)-2*npl)
              avw(t,j,k)=dm(12)/(tti(t)-2*npl)
              avwm(t,j,k)=dm(13)/(tti(t)-2*npl)
              avwwm(t,j,k)=dm(14)/(tti(t)-2*npl)
              avuvm(t,j,k)=dm(15)/(tti(t)-2*npl)
              avuwm(t,j,k)=dm(16)/(tti(t)-2*npl)
              avvwm(t,j,k)=dm(17)/(tti(t)-2*npl)
	      avksgs(t,j,k)=dm(18)/(tti(t)-2*npl)
	      aveps(t,j,k)=dm(19)/(tti(t)-2*npl)
	      avepsm(t,j,k)=dm(20)/(tti(t)-2*npl)
           end do
           end do

           deallocate(x,y,z,uvm,uwm,vwm,p,pm,ppm,vis,vism)
           deallocate(u,um,uum,v,vm,vvm,w,wm,wwm,ksgs,eps,epsm)

        end do

	  ttyz=tt/ttx

        allocate(avfy(ttyz,maxttj,maxttk))
        allocate(avfz(ttyz,maxttj,maxttk),dmk(20,maxttj,maxttk))
        allocate(avfu(ttyz,maxttj,maxttk),avfum(ttyz,maxttj,maxttk))
        allocate(avfuum(ttyz,maxttj,maxttk))
        allocate(avfv(ttyz,maxttj,maxttk),avfvm(ttyz,maxttj,maxttk))
        allocate(avfvvm(ttyz,maxttj,maxttk))
        allocate(avfw(ttyz,maxttj,maxttk),avfwm(ttyz,maxttj,maxttk))
        allocate(avfwwm(ttyz,maxttj,maxttk))
        allocate(avfuvm(ttyz,maxttj,maxttk),avfuwm(ttyz,maxttj,maxttk))
        allocate(avfvwm(ttyz,maxttj,maxttk))
        allocate(avfp(ttyz,maxttj,maxttk),avfpm(ttyz,maxttj,maxttk))
        allocate(avfppm(ttyz,maxttj,maxttk),avfvis(ttyz,maxttj,maxttk))
	allocate(avfvism(ttyz,maxttj,maxttk),avfksgs(ttyz,maxttj,maxttk))
	allocate(avfeps(ttyz,maxttj,maxttk),avfepsm(ttyz,maxttj,maxttk))

!	This loop promediates among blocks in x direction

	  do r=1,ttyz
		dmk=0
		do t=(r-1)*ttx+1,r*ttx
!           if(((minz(t)-minz(r)).lt.1e-8).and.
!     &	 ((miny(t)-miny(r)).lt.1e-8)) then
		  ttj(r)=ttj(t)	;	ttk(r)=ttk(t)
              do j=1,ttj(t)
              do k=1,ttk(t)
                 avfy(r,j,k)=avy(t,j,k)
                 avfz(r,j,k)=avz(t,j,k)
                 dmk(1,j,k)=dmk(1,j,k)+avp(t,j,k)/ttx
                 dmk(2,j,k)=dmk(2,j,k)+avpm(t,j,k)/ttx
                 dmk(3,j,k)=dmk(3,j,k)+avppm(t,j,k)/ttx
                 dmk(4,j,k)=dmk(4,j,k)+avvis(t,j,k)/ttx
		 dmk(5,j,k)=dmk(5,j,k)+avvism(t,j,k)/ttx
                 dmk(6,j,k)=dmk(6,j,k)+avu(t,j,k)/ttx
                 dmk(7,j,k)=dmk(7,j,k)+avum(t,j,k)/ttx
                 dmk(8,j,k)=dmk(8,j,k)+avuum(t,j,k)/ttx
                 dmk(9,j,k)=dmk(9,j,k)+avv(t,j,k)/ttx
                 dmk(10,j,k)=dmk(10,j,k)+avvm(t,j,k)/ttx
                 dmk(11,j,k)=dmk(11,j,k)+avvvm(t,j,k)/ttx
                 dmk(12,j,k)=dmk(12,j,k)+avw(t,j,k)/ttx
                 dmk(13,j,k)=dmk(13,j,k)+avwm(t,j,k)/ttx
                 dmk(14,j,k)=dmk(14,j,k)+avwwm(t,j,k)/ttx
                 dmk(15,j,k)=dmk(15,j,k)+avuvm(t,j,k)/ttx
                 dmk(16,j,k)=dmk(16,j,k)+avuwm(t,j,k)/ttx
                 dmk(17,j,k)=dmk(17,j,k)+avvwm(t,j,k)/ttx
		 dmk(18,j,k)=dmk(18,j,k)+avksgs(t,j,k)/ttx
		 dmk(19,j,k)=dmk(19,j,k)+aveps(t,j,k)/ttx
		 dmk(20,j,k)=dmk(20,j,k)+avepsm(t,j,k)/ttx


              end do
		  end do
!           end if
		enddo
              do j=1,ttj(r)								!interpolating quantities to the mesh
              do k=1,ttk(r)
		   avfp(r,j,k)=0.25*(dmk(1,j,k)+dmk(1,j+1,k)+dmk(1,j,k+1)+
     &				     dmk(1,j+1,k+1))
		   avfpm(r,j,k)=0.25*(dmk(2,j,k)+dmk(2,j+1,k)+dmk(2,j,k+1)+
     &				     dmk(2,j+1,k+1))
		   avfppm(r,j,k)=0.25*(dmk(3,j,k)+dmk(3,j+1,k)+dmk(3,j,k+1)+
     &				     dmk(3,j+1,k+1))
		   avfvis(r,j,k)=0.25*(dmk(4,j,k)+dmk(4,j+1,k)+dmk(4,j,k+1)+
     &				     dmk(4,j+1,k+1))
	           avfvism(r,j,k)=0.25*(dmk(5,j,k)+dmk(5,j+1,k)+dmk(5,j,k+1)+
     &				     dmk(5,j+1,k+1))
		   avfu(r,j,k)=0.25*(dmk(6,j,k)+dmk(6,j+1,k)+dmk(6,j,k+1)+
     &				     dmk(6,j+1,k+1))
		   avfum(r,j,k)=0.25*(dmk(7,j,k)+dmk(7,j+1,k)+dmk(7,j,k+1)+
     &				     dmk(7,j+1,k+1))
		   avfuum(r,j,k)=0.25*(dmk(8,j,k)+dmk(8,j+1,k)+dmk(8,j,k+1)+
     &				     dmk(8,j+1,k+1))
		   avfv(r,j,k)=0.5*(dmk(9,j,k)+dmk(9,j,k+1))
		   avfvm(r,j,k)=0.5*(dmk(10,j,k)+dmk(10,j,k+1))
		   avfvvm(r,j,k)=0.5*(dmk(11,j,k)+dmk(11,j,k+1))
		   avfw(r,j,k)=0.5*(dmk(12,j,k)+dmk(12,j+1,k))
		   avfwm(r,j,k)=0.5*(dmk(13,j,k)+dmk(13,j+1,k))
		   avfwwm(r,j,k)=0.5*(dmk(14,j,k)+dmk(14,j+1,k))
		 avfuvm(r,j,k)=0.25*(dmk(15,j,k)+dmk(15,j+1,k)+dmk(15,j,k+1)
     &				     +dmk(15,j+1,k+1))
		 avfuwm(r,j,k)=0.25*(dmk(16,j,k)+dmk(16,j+1,k)+dmk(16,j,k+1)
     &				     +dmk(16,j+1,k+1))
		 avfvwm(r,j,k)=0.25*(dmk(17,j,k)+dmk(17,j+1,k)+dmk(17,j,k+1)
     &				     +dmk(17,j+1,k+1))
              end do
		  end do

           write(chb,'(i8)') r
           sn=len(trim(adjustl(chb)))
           chb=repeat('0',(3-sn))//trim(adjustl(chb))
           gf='avg_slicex_'//trim(adjustl(chb))//'.dat'

        open (unit=86+r, file=gf)

        write (86+r,*)
     & 'VARIABLES= "y","z"',
     &	'"U","UM","uum","V","VM",vvm","W","WM","wwm"'

        write (86+r,'(a,i3,a)')'ZONE T="avg_zone:',r,'"'
	  write(86+r,*)'J=',(ttj(r)-2*npl+1),', K=',(ttk(r)-2*npl+1),
     &'F=POINT'

        do k=npl,ttk(r)-npl
        do j=npl,ttj(r)-npl
           write (86+r,88) avfy(r,j,k),avfz(r,j,k),
     &avfu(r,j,k),avfum(r,j,k),avfuum(r,j,k),avfv(r,j,k),
     &avfvm(r,j,k),avfvvm(r,j,k),avfw(r,j,k),avfwm(r,j,k),
     &avfwwm(r,j,k)
        end do
	end do

        close (86+r)

        end do


        deallocate(avz,avuvm,avuwm,avvwm,avp,avpm,avppm,avvis)
        deallocate(avu,avum,avuum,avv,avvm,avvvm,avw,avwm,avwwm)
        deallocate(avfz,avfy,avfuvm,avfuwm,avfvwm,avfp)
        deallocate(avfpm,avfppm,avfvis)
        deallocate(avfu,avfum,avfuum,avfv)
        deallocate(avfvm,avfvvm,avfw,avfwm,avfwwm)
        deallocate(dx,dy,dz,tti,ttj,ttk)
	deallocate(avfvism,avfksgs,avfeps,avfepsm)
!	  deallocate(minx,miny,minz)
!	  deallocate(maxx,maxy,maxz)

88      format (15e25.8)
        end program
!##########################################################################
