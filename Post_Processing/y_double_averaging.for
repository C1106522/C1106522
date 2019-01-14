	
	program	y_avg
!####################################################################################################

!Make sure that tecbin variables are read as they are written in your version of the code.THIS can be checked
! in post.for in which tecbins are written. In this case there are 20 variables written in tecbins, therefore dm is 20
! and dmk is 20. If your version of the code does not write ksgs and eps in tecbins, these variables should be removed.


	implicit none
	
	double precision, pointer, dimension (:) :: dx,dy,dz
	


	double precision,pointer, dimension (:,:,:) :: avx,avz
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

	double precision, pointer, dimension (:,:,:) :: avfx,avfz
	double precision, pointer, dimension (:,:,:) :: avfp,avfpm,avfppm
	double precision, pointer, dimension (:,:,:) :: avfu,avfum,avfuum
	double precision, pointer, dimension (:,:,:) :: avfv,avfvm,avfvvm
	double precision, pointer, dimension (:,:,:) :: avfw,avfwm,avfwwm
	double precision, pointer, dimension (:,:,:) :: avfuvm,avfuwm,avfvwm
	double precision, pointer, dimension (:,:,:) :: avfvis,avfvism
	double precision, pointer, dimension (:,:,:) :: avfksgs,avfeps
	double precision, pointer, dimension (:,:,:) :: avfepsm


	


	

	integer :: tt,tty,n,t,sn,npl,r,ttxz
	integer :: k,inind,jnind,knind,i,j
	integer :: maxtti,maxttk
	integer, pointer,dimension(:) :: tti,ttj,ttk

	double precision, dimension(20) :: dm
        double precision, pointer, dimension(:,:,:) :: dmk


	character*8 :: chb
	character*25 :: gf
	
	

	

	print*,'Please enter the number of domains'
	read(*,'(i4)')tt
	print*,'You have',tt,'domains'

	print*,'Please enter the number of domains in Y direction'
	read(*,'(i4)')tty
	print*,'You have',tty,'domains in Y direction'

	allocate(dx(tt),dy(tt),dz(tt))
	allocate(tti(tt),ttj(tt),ttk(tt))

	maxtti=-1
	maxttk=-1
		do n=0,tt-1

			t=n+1

	   write(chb,'(i8)') n
	   sn=len(trim(adjustl(chb)))
	   chb=repeat('0',(4-sn))//trim(adjustl(chb))
           gf='tecbin'//trim(adjustl(chb))//'.bin'
           open (unit=700+n, file=gf, form='unformatted',status='old')

	 
	      
	 read (700+n) tti(t),ttj(t),ttk(t)
	
	   maxtti=(max(maxtti,tti(t)))
	   maxttk=(max(maxttk,ttk(t)))

		end do

	
	allocate(avx(maxtti,tt,maxttk))
	allocate(avz(maxtti,tt,maxttk))
	allocate(avp(maxtti,tt,maxttk),avpm(maxtti,tt,maxttk))
	allocate(avppm(maxtti,tt,maxttk))
	allocate(avvis(maxtti,tt,maxttk),avvism(maxtti,tt,maxttk))
        allocate(avu(maxtti,tt,maxttk),avum(maxtti,tt,maxttk))
	allocate(avuum(maxtti,tt,maxttk))
        allocate(avv(maxtti,tt,maxttk),avvm(maxtti,tt,maxttk))
	allocate(avvvm(maxtti,tt,maxttk))
        allocate(avw(maxtti,tt,maxttk),avwm(maxtti,tt,maxttk))
        allocate(avwwm(maxtti,tt,maxttk))
        allocate(avuvm(maxtti,tt,maxttk),avuwm(maxtti,tt,maxttk))
	allocate(avvwm(maxtti,tt,maxttk),avksgs(maxtti,tt,maxttk))
	allocate(aveps(maxtti,tt,maxttk),avepsm(maxtti,tt,maxttk))


		do n=0,tt-1
			t=n+1

		read (700+n) npl
		read (700+n) inind,jnind,knind

	
		
	  write(*,'(i3,a,3i7,3i3)') n,' dom--> ',tti(t),ttj(t),ttk(t)
	  write(*,'(i3,a,3i7,1i3)') n,' npl--> ',npl


	 
           allocate(x(tti(t),ttj(t),ttk(t)),y(tti(t),ttj(t),ttk(t)))
           allocate(z(tti(t),ttj(t),ttk(t)))
	   allocate(p(tti(t),ttj(t),ttk(t)),pm(tti(t),ttj(t),ttk(t)))
           allocate(ppm(tti(t),ttj(t),ttk(t)),vis(tti(t),ttj(t),ttk(t)))
	   allocate(vism(tti(t),ttj(t),ttk(t)))
           allocate(u(tti(t),ttj(t),ttk(t)),um(tti(t),ttj(t),ttk(t)))
           allocate(uum(tti(t),ttj(t),ttk(t)))
           allocate(v(tti(t),ttj(t),ttk(t)),vm(tti(t),ttj(t),ttk(t)))
           allocate(vvm(tti(t),ttj(t),ttk(t)))
           allocate(w(tti(t),ttj(t),ttk(t)),wm(tti(t),ttj(t),ttk(t)))
           allocate(wwm(tti(t),ttj(t),ttk(t)))
           allocate(uvm(tti(t),ttj(t),ttk(t)),uwm(tti(t),ttj(t),ttk(t)))
           allocate(vwm(tti(t),ttj(t),ttk(t)))
           allocate(ksgs(tti(t),ttj(t),ttk(t)))
     	   allocate(eps(tti(t),ttj(t),ttk(t)),epsm(tti(t),ttj(t),ttk(t)))

		do k=1,ttk(t)
		do j=1,ttj(t)
		do i=1,tti(t)
!Reading variables from tecbins, adjust accodring to your code version		
           
              read (700+n) x(i,j,k),y(i,j,k),z(i,j,k),
     &  p(i,j,k),pm(i,j,k),ppm(i,j,k),
     &  vis(i,j,k),vism(i,j,k),
     &  u(i,j,k),um(i,j,k),uum(i,j,k),
     &  v(i,j,k),vm(i,j,k),vvm(i,j,k),
     &  w(i,j,k),wm(i,j,k),wwm(i,j,k),
     &  uvm(i,j,k),uwm(i,j,k),vwm(i,j,k),
     &  ksgs(i,j,k),eps(i,j,k),epsm(i,j,k)

           end do
           end do
           end do
           close (700+n)


	   dx(t)=x(npl+1,npl+1,npl+1)-x(npl,npl,npl)
           dy(t)=y(npl+1,npl+1,npl+1)-y(npl,npl,npl)
           dz(t)=z(npl+1,npl+1,npl+1)-z(npl,npl,npl)
		



	   do i=1,tti(t)
		do k=1,ttk(t)
		
		dm =0.0

		
	   do j=1,ttj(t)

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

	      avx(i,t,k)=x(i,1,k)
              avz(i,t,k)=z(i,1,k)
              avp(i,t,k)=dm(1)/(ttj(t)-2*npl)
              avpm(i,t,k)=dm(2)/(ttj(t)-2*npl)
              avppm(i,t,k)=dm(3)/(ttj(t)-2*npl)
              avvis(i,t,k)=dm(4)/(ttj(t)-2*npl)
	      avvism(i,t,k)=dm(5)/(ttj(t)-2*npl)
              avu(i,t,k)=dm(6)/(ttj(t)-2*npl)
              avum(i,t,k)=dm(7)/(ttj(t)-2*npl)
              avuum(i,t,k)=dm(8)/(ttj(t)-2*npl)
              avv(i,t,k)=dm(9)/(ttj(t)-2*npl)
              avvm(i,t,k)=dm(10)/(ttj(t)-2*npl)
              avvvm(i,t,k)=dm(11)/(ttj(t)-2*npl)
              avw(i,t,k)=dm(12)/(ttj(t)-2*npl)
              avwm(i,t,k)=dm(13)/(ttj(t)-2*npl)
              avwwm(i,t,k)=dm(14)/(ttj(t)-2*npl)
              avuvm(i,t,k)=dm(15)/(ttj(t)-2*npl)
              avuwm(i,t,k)=dm(16)/(ttj(t)-2*npl)
              avvwm(i,t,k)=dm(17)/(ttj(t)-2*npl)
	      avksgs(i,t,k)=dm(18)/(ttj(t)-2*npl)
	      aveps(i,t,k)=dm(19)/(ttj(t)-2*npl)
	      avepsm(i,t,k)=dm(20)/(ttj(t)-2*npl)


	      end do 
	      end do

	   deallocate(x,y,z,p,pm,ppm,vis,vism)
           deallocate(u,um,uum,v,vm,vvm,w,wm,wwm,uvm,uwm,vwm)
	   deallocate(ksgs,eps,epsm)
		end do
	  
	    ttxz=tt/tty

	   allocate(avfx(maxtti,ttxz,maxttk))
	   allocate(avfz(maxtti,ttxz,maxttk),dmk(maxtti,20,maxttk))
	   allocate(avfp(maxtti,ttxz,maxttk),avfpm(maxtti,ttxz,maxttk))
	   allocate(avfppm(maxtti,ttxz,maxttk),avfvis(maxtti,ttxz,maxttk))
    	   allocate(avfvism(maxtti,ttxz,maxttk))
	   allocate(avfu(maxtti,ttxz,maxttk))
  	   allocate(avfum(maxtti,ttxz,maxttk),avfuum(maxtti,ttxz,maxttk))
	   allocate(avfv(maxtti,ttxz,maxttk),avfvm(maxtti,ttxz,maxttk))
   	   allocate(avfvvm(maxtti,ttxz,maxttk),avfw(maxtti,ttxz,maxttk))
	   allocate(avfwm(maxtti,ttxz,maxttk),avfwwm(maxtti,ttxz,maxttk))
	   allocate(avfuvm(maxtti,ttxz,maxttk),avfuwm(maxtti,ttxz,maxttk))
	   allocate(avfvwm(maxtti,ttxz,maxttk),avfksgs(maxtti,ttxz,maxttk))
	   allocate(avfeps(maxtti,ttxz,maxttk),avfepsm(maxtti,ttxz,maxttk))
	 
	    do r=1,ttxz
		dmk=0

		do t=(r-1)*tty+1,r*tty


		tti(r)=tti(t); ttk(r)=ttk(t)

		do i=1,tti(t)
			do k=1,ttk(t)

		 avfx(i,r,k)=avx(i,t,k)
                 avfz(i,r,k)=avz(i,t,k)
                 dmk(i,1,k)=dmk(i,1,k)+avp(i,t,k)/tty
                 dmk(i,2,k)=dmk(i,2,k)+avpm(i,t,k)/tty
                 dmk(i,3,k)=dmk(i,3,k)+avppm(i,t,k)/tty
                 dmk(i,4,k)=dmk(i,4,k)+avvis(i,t,k)/tty
		 dmk(i,5,k)=dmk(i,5,k)+avvism(i,t,k)/tty
                 dmk(i,6,k)=dmk(i,6,k)+avu(i,t,k)/tty
                 dmk(i,7,k)=dmk(i,7,k)+avum(i,t,k)/tty
                 dmk(i,8,k)=dmk(i,8,k)+avuum(i,t,k)/tty
                 dmk(i,9,k)=dmk(i,9,k)+avv(i,t,k)/tty
                 dmk(i,10,k)=dmk(i,10,k)+avvm(i,t,k)/tty
                 dmk(i,11,k)=dmk(i,11,k)+avvvm(i,t,k)/tty
                 dmk(i,12,k)=dmk(i,12,k)+avw(i,t,k)/tty
                 dmk(i,13,k)=dmk(i,13,k)+avwm(i,t,k)/tty
                 dmk(i,14,k)=dmk(i,14,k)+avwwm(i,t,k)/tty
                 dmk(i,15,k)=dmk(i,15,k)+avuvm(i,t,k)/tty
                 dmk(i,16,k)=dmk(i,16,k)+avuwm(i,t,k)/tty
                 dmk(i,17,k)=dmk(i,17,k)+avvwm(i,t,k)/tty
		 dmk(i,18,k)=dmk(i,18,k)+avksgs(i,t,k)/tty
		 dmk(i,19,k)=dmk(i,19,k)+aveps(i,t,k)/tty
		 dmk(i,20,k)=dmk(i,20,k)+avepsm(i,t,k)/tty


		end do
			end do
		end do

		
		do i=1,tti(r)
			do k=1,ttk(r)

		avfp(i,r,k)=0.25*(dmk(i,1,k)+dmk(i+1,1,k)+dmk(i,1,k+1)+
     &	        dmk(i+1,1,k+1))
	
		avfpm(i,r,k)=0.25*(dmk(i,2,k)+dmk(i+1,2,k)+dmk(i,2,k+1)+
     &	        dmk(i+1,2,k+1))
	        
		avfppm(i,r,k)=0.25*(dmk(i,3,k)+dmk(i+1,3,k)+dmk(i,3,k+1)+
     &	        dmk(i+1,3,k+1))

		avfvis(i,r,k)=0.25*(dmk(i,4,k)+dmk(i+1,4,k)+dmk(i,4,k+1)+
     &	        dmk(i+1,4,k+1))

		avfvism(i,r,k)=0.25*(dmk(i,5,k)+dmk(i+1,5,k)+dmk(i,5,k+1)+
     &	        dmk(i+1,5,k+1))

		avfu(i,r,k)=0.25*(dmk(i,6,k)+dmk(i+1,6,k)+dmk(i,6,k+1)+
     &		dmk(i+1,6,k+1))

	        avfum(i,r,k)=0.25*(dmk(i,7,k)+dmk(i+1,7,k)+dmk(i,7,k+1)+
     &		dmk(i+1,7,k+1))
		
		avfuum(i,r,k)=0.25*(dmk(i,8,k)+dmk(i+1,8,k)+dmk(i,8,k+1)+
     &		dmk(i+1,8,k+1))

		avfv(i,r,k)=0.5*(dmk(i,9,k)+dmk(i,9,k+1))
		
		avfvm(i,r,k)=0.5*(dmk(i,10,k)+dmk(i,10,k+1))
		
		avfvvm(i,r,k)=0.5*(dmk(i,11,k)+dmk(i,11,k+1))
		
		avfw(i,r,k)=0.5*(dmk(i,12,k)+dmk(i,12,k+1))

	        avfwm(i,r,k)=0.5*(dmk(i,13,k)+dmk(i,13,k+1))
		
		avfwwm(i,r,k)=0.5*(dmk(i,14,k)+dmk(i,14,k+1))


		avfuvm(i,r,k)=0.25*(dmk(i,15,k)+dmk(i+1,15,k)+dmk(i,15,k+1)+
     &	        dmk(i+1,15,k+1))
		
		avfuwm(i,r,k)=0.25*(dmk(i,16,k)+dmk(i+1,16,k)+dmk(i,16,k+1)+
     &	        dmk(i+1,16,k+1))
		
		avfvwm(i,r,k)=0.25*(dmk(i,17,k)+dmk(i+1,17,k)+dmk(i,17,k+1)+
     &	        dmk(i+1,17,k+1))
		
		avfksgs(i,r,k)=0.25*(dmk(i,18,k)+dmk(i+1,18,k)+dmk(i,18,k+1)+
     &	        dmk(i+1,18,k+1))

		avfeps(i,r,k)=0.25*(dmk(i,19,k)+dmk(i+1,19,k)+dmk(i,19,k+1)+
     &	        dmk(i+1,19,k+1))

		avfepsm(i,r,k)=0.25*(dmk(i,20,k)+dmk(i+1,20,k)+dmk(i,20,k+1)+
     &	        dmk(i+1,20,k+1))


	
		
		end do
		end do
		
		  write(chb,'(i8)') r
           sn=len(trim(adjustl(chb)))
           chb=repeat('0',(3-sn))//trim(adjustl(chb))
           gf='avg_slicy_'//trim(adjustl(chb))//'.dat'

		

	    open (unit=86+r, file=gf)
	! variables=x,y,z,P,PM,ppm,vis,vism,U,UM,uuM,V,VM,vvM,W,WM,wwM,uvM,uwM,vwM,ksgs,eps,epsm
        write (86+r,*)
     &  'VARIABLES= "x","z"',
     &	'"U","UM","uum","V","VM",vvm","W","WM","wwm"'

  
        write (86+r,'(a,i3,a)')'ZONE T="avg_zone:',r,'"'
	    write(86+r,*)'I=',(tti(r)-2*npl+1),', K=',(ttk(r)-2*npl+1),
     &'F=POINT'
	       do k= npl,ttk(r)-npl
        	do i= npl,tti(r)-npl
           write (86+r,88) avfx(i,r,k),avfz(i,r,k),
     &avfu(i,r,k),avfum(i,r,k),avfuum(i,r,k),
     &avfv(i,r,k),avfvm(i,r,k),avfvvm(i,r,k),avfw(i,r,k),
     &avfwm(i,r,k),avfwwm(i,r,k)
	end do
	end do

		close(86+r)

	end do

	deallocate(avz,avx,avuvm,avuwm,avvwm,avp,avpm,avppm,avvis)
        deallocate(avu,avum,avuum,avv,avvm,avvvm,avw,avwm,avwwm)
	deallocate(avvism,avksgs,aveps)
        deallocate(avfz,avfx,avfuvm,avfuwm,avfvwm,avfp)
        deallocate(avfpm,avfppm,avfvis,dmk)
        deallocate(avfu,avfum,avfuum,avfv)
        deallocate(avfvm,avfvvm,avfw,avfwm,avfwwm)
	deallocate(avfvism,avfksgs,avfeps)
        deallocate(dx,dy,dz,tti,ttj,ttk)


88      format (15e15.5)
        
	end program














