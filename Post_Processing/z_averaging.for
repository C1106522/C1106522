	program  z_avg
!####################################################################################################
!Make sure that tecbin variables are read as they are written in your version of the code.THIS can be checked
! in post.for in which tecbins are written. In this case there are 20 variables written in tecbins, therefore dm is 20
! and dmk is 20. If your version of the code does not write ksgs and eps in tecbins, these variables should be removed.




	implicit none
	
	double precision, pointer, dimension (:) :: dx,dy,dz
	


	double precision,pointer, dimension (:,:,:) :: avx,avy
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

	double precision, pointer, dimension (:,:,:) :: avfx,avfy
	double precision, pointer, dimension (:,:,:) :: avfp,avfpm,avfppm
	double precision, pointer, dimension (:,:,:) :: avfu,avfum,avfuum
	double precision, pointer, dimension (:,:,:) :: avfv,avfvm,avfvvm
	double precision, pointer, dimension (:,:,:) :: avfw,avfwm,avfwwm
	double precision, pointer, dimension (:,:,:) :: avfuvm,avfuwm,avfvwm
	double precision, pointer, dimension (:,:,:) :: avfvis,avfvism
	double precision, pointer, dimension (:,:,:) :: avfksgs,avfeps
	double precision, pointer, dimension (:,:,:) :: avfepsm


	


	

	integer :: tt,ttz,n,t,sn,npl,r,ttxy
	integer :: k,inind,jnind,knind,i,j
	integer :: maxtti,maxttj
	integer, pointer,dimension(:) :: tti,ttj,ttk

	double precision, dimension(20) :: dm
        double precision, pointer, dimension(:,:,:) :: dmk


	character*8 :: chb
	character*25 :: gf
	

	print*,'Please enter the number of domains'
	read(*,'(i4)')tt
	print*,'You have',tt,'domains'

	print*,'Please enter the number of domains in Z direction'
	read(*,'(i4)')ttz
	print*,'You have',ttz,'domains in Y direction'

	allocate(dx(tt),dy(tt),dz(tt))
	allocate(tti(tt),ttj(tt),ttk(tt))

	maxtti=-1
	maxttj=-1


		do n=0,tt-1

			t=n+1

	   write(chb,'(i8)') n
	   sn=len(trim(adjustl(chb)))
	   chb=repeat('0',(4-sn))//trim(adjustl(chb))
           gf='tecbin'//trim(adjustl(chb))//'.bin'
           open (unit=700+n, file=gf, form='unformatted',status='old')

	 
	      
	 read (700+n) tti(t),ttj(t),ttk(t)
	
	   maxtti=(max(maxtti,tti(t)))
	   maxttj=(max(maxttj,ttj(t)))

		end do


	allocate(avx(maxtti,maxttj,tt))
	allocate(avy(maxtti,maxttj,tt))
	allocate(avp(maxtti,maxttj,tt),avpm(maxtti,maxttj,tt))
	allocate(avppm(maxtti,maxttj,tt))
	allocate(avvis(maxtti,maxttj,tt),avvism(maxtti,maxttj,tt))
        allocate(avu(maxtti,maxttj,tt),avum(maxtti,maxttj,tt))
	allocate(avuum(maxtti,maxttj,tt))
        allocate(avv(maxtti,maxttj,tt),avvm(maxtti,maxttj,tt))
	allocate(avvvm(maxtti,maxttj,tt))
        allocate(avw(maxtti,maxttj,tt),avwm(maxtti,maxttj,tt))
        allocate(avwwm(maxtti,maxttj,tt))
        allocate(avuvm(maxtti,maxttj,tt),avuwm(maxtti,maxttj,tt))
	allocate(avvwm(maxtti,maxttj,tt),avksgs(maxtti,maxttj,tt))
	allocate(aveps(maxtti,maxttj,tt),avepsm(maxtti,maxttj,tt))
	
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
		do j=1,ttj(t)
		
		dm =0.0

		
	   do k=1,ttk(t)

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

	      avx(i,j,t)=x(i,j,1)
              avy(i,j,t)=z(i,j,1)
              avp(i,j,t)=dm(1)/(ttk(t)-2*npl)
              avpm(i,j,t)=dm(2)/(ttk(t)-2*npl)
              avppm(i,j,t)=dm(3)/(ttk(t)-2*npl)
              avvis(i,j,t)=dm(4)/(ttk(t)-2*npl)
	      avvism(i,j,t)=dm(5)/(ttk(t)-2*npl)
              avu(i,j,t)=dm(6)/(ttk(t)-2*npl)
              avum(i,j,t)=dm(7)/(ttk(t)-2*npl)
              avuum(i,j,t)=dm(8)/(ttk(t)-2*npl)
              avv(i,j,t)=dm(9)/(ttk(t)-2*npl)
              avvm(i,j,t)=dm(10)/(ttk(t)-2*npl)
              avvvm(i,j,t)=dm(11)/(ttk(t)-2*npl)
              avw(i,j,t)=dm(12)/(ttk(t)-2*npl)
              avwm(i,j,t)=dm(13)/(ttk(t)-2*npl)
              avwwm(i,j,t)=dm(14)/(ttk(t)-2*npl)
              avuvm(i,j,t)=dm(15)/(ttk(t)-2*npl)
              avuwm(i,j,t)=dm(16)/(ttk(t)-2*npl)
              avvwm(i,j,t)=dm(17)/(ttk(t)-2*npl)
	      avksgs(i,j,t)=dm(18)/(ttk(t)-2*npl)
	      aveps(i,j,t)=dm(19)/(ttk(t)-2*npl)
	      avepsm(i,j,t)=dm(20)/(ttk(t)-2*npl)


	      end do 
	      end do

	   deallocate(x,y,z,p,pm,ppm,vis,vism)
           deallocate(u,um,uum,v,vm,vvm,w,wm,wwm,uvm,uwm,vwm)
	   deallocate(ksgs,eps,epsm)
		end do




	   ttxy=tt/ttz

	   allocate(avfx(maxtti,maxttj,ttxy))
	   allocate(avfy(maxtti,maxttj,ttxy),dmk(maxtti,maxttj,20))
	   allocate(avfp(maxtti,maxttj,ttxy),avfpm(maxtti,maxttj,ttxy))
	   allocate(avfppm(maxtti,maxttj,ttxy),avfvis(maxtti,maxttj,ttxy))
    	   allocate(avfvism(maxtti,maxttj,ttxy))
	   allocate(avfu(maxtti,maxttj,ttxy))
  	   allocate(avfum(maxtti,maxttj,ttxy),avfuum(maxtti,maxttj,ttxy))
	   allocate(avfv(maxtti,maxttj,ttxy),avfvm(maxtti,maxttj,ttxy))
   	   allocate(avfvvm(maxtti,maxttj,ttxy),avfw(maxtti,maxttj,ttxy))
	   allocate(avfwm(maxtti,maxttj,ttxy),avfwwm(maxtti,maxttj,ttxy))
	   allocate(avfuvm(maxtti,maxttj,ttxy),avfuwm(maxtti,maxttj,ttxy))
	   allocate(avfvwm(maxtti,maxttj,ttxy),avfksgs(maxtti,maxttj,ttxy))
	   allocate(avfeps(maxtti,maxttj,ttxy),avfepsm(maxtti,maxttj,ttxy))
	 
	    do r=1,ttxy
		dmk=0

		do t=(r-1)*ttz+1,r*ttz


		tti(r)=tti(t); ttk(r)=ttk(t)


		do i=1,tti(t)
			do j=1,ttj(t)

		 avfx(i,j,r)=avx(i,j,t)
                 avfy(i,j,r)=avy(i,j,t)
                 dmk(i,j,1)=dmk(i,j,1)+avp(i,j,t)/ttz
                 dmk(i,j,2)=dmk(i,j,2)+avpm(i,j,t)/ttz
                 dmk(i,j,3)=dmk(i,j,3)+avppm(i,j,t)/ttz
                 dmk(i,j,4)=dmk(i,j,4)+avvis(i,j,t)/ttz
		 dmk(i,j,5)=dmk(i,j,5)+avvism(i,j,t)/ttz
                 dmk(i,j,6)=dmk(i,j,6)+avu(i,j,t)/ttz
                 dmk(i,j,7)=dmk(i,j,7)+avum(i,j,t)/ttz
                 dmk(i,j,8)=dmk(i,j,8)+avuum(i,j,t)/ttz
                 dmk(i,j,9)=dmk(i,j,9)+avv(i,j,t)/ttz
                 dmk(i,j,10)=dmk(i,j,10)+avvm(i,j,t)/ttz
                 dmk(i,j,11)=dmk(i,j,11)+avvvm(i,j,t)/ttz
                 dmk(i,j,12)=dmk(i,j,12)+avw(i,j,t)/ttz
                 dmk(i,j,13)=dmk(i,j,13)+avwm(i,j,t)/ttz
                 dmk(i,j,14)=dmk(i,j,14)+avwwm(i,j,t)/ttz
                 dmk(i,j,15)=dmk(i,j,15)+avuvm(i,j,t)/ttz
                 dmk(i,j,16)=dmk(i,j,16)+avuwm(i,j,t)/ttz
                 dmk(i,j,17)=dmk(i,j,17)+avvwm(i,j,t)/ttz
		 dmk(i,j,18)=dmk(i,j,18)+avksgs(i,j,t)/ttz
		 dmk(i,j,19)=dmk(i,j,19)+aveps(i,j,t)/ttz
		 dmk(i,j,20)=dmk(i,j,20)+avepsm(i,j,t)/ttz


		end do
			end do
		end do



			do i=1,tti(r)
			do j=1,ttj(r)

		avfp(i,j,r)=0.25*(dmk(i,j,1)+dmk(i+1,j,1)+dmk(i,j+1,1)+
     &	        dmk(i+1,j+1,1))
	
		avfpm(i,j,r)=0.25*(dmk(i,j,2)+dmk(i+1,j,2)+dmk(i,j+1,2)+
     &	        dmk(i+1,j+1,2))
	        
		avfppm(i,j,r)=0.25*(dmk(i,j,3)+dmk(i+1,j,3)+dmk(i,j+1,3)+
     &	        dmk(i+1,j+1,3))

		avfvis(i,j,r)=0.25*(dmk(i,j,4)+dmk(i+1,j,4)+dmk(i,j+1,4)+
     &	        dmk(i+1,j+1,4))

		avfvism(i,j,r)=0.25*(dmk(i,j,5)+dmk(i+1,j,5)+dmk(i,j+1,5)+
     &	        dmk(i+1,j+1,5))

		avfu(i,j,r)=0.25*(dmk(i,j,6)+dmk(i+1,j,6)+dmk(i,j+1,6)+
     &		dmk(i+1,j+1,6))

	        avfum(i,j,r)=0.25*(dmk(i,j,7)+dmk(i+1,j,7)+dmk(i,j+1,7)+
     &		dmk(i+1,j+1,7))
		
		avfuum(i,j,r)=0.25*(dmk(i,j,8)+dmk(i+1,j,8)+dmk(i,j+1,8)+
     &		dmk(i+1,j+1,8))

		avfv(i,j,r)=0.5*(dmk(i,j,9)+dmk(i,j+1,9))
		
		avfvm(i,j,r)=0.5*(dmk(i,j,10)+dmk(i,j+1,10))
		
		avfvvm(i,j,r)=0.5*(dmk(i,j,11)+dmk(i,j+1,11))
		
		avfw(i,j,r)=0.5*(dmk(i,j,12)+dmk(i,j+1,12))

	        avfwm(i,j,r)=0.5*(dmk(i,j,13)+dmk(i,j+1,13))
		
		avfwwm(i,j,r)=0.5*(dmk(i,j,14)+dmk(i,j+1,14))


		avfuvm(i,j,r)=0.25*(dmk(i,j,15)+dmk(i+1,j,15)+dmk(i,j+1,15)+
     &	        dmk(i+1,j+1,15))
		
		avfuwm(i,j,r)=0.25*(dmk(i,j,16)+dmk(i+1,j,16)+dmk(i,j+1,16)+
     &	        dmk(i+1,j+1,16))
		
		avfvwm(i,j,r)=0.25*(dmk(i,j,17)+dmk(i+1,j,17)+dmk(i,j+1,17)+
     &	        dmk(i+1,j+1,17))
		
		avfksgs(i,j,r)=0.25*(dmk(i,j,18)+dmk(i+1,j,18)+dmk(i,j+1,18)+
     &	        dmk(i+1,j+1,18))

		avfeps(i,j,r)=0.25*(dmk(i,j,19)+dmk(i+1,j,19)+dmk(i,j+1,19)+
     &	        dmk(i+1,j+1,19))

		avfepsm(i,j,r)=0.25*(dmk(i,j,20)+dmk(i+1,j,20)+dmk(i,j+1,20)+
     &	        dmk(i+1,j+1,20))


	
		
		end do
		end do


		  write(chb,'(i8)') r
           sn=len(trim(adjustl(chb)))
           chb=repeat('0',(3-sn))//trim(adjustl(chb))
           gf='avg_slicez_'//trim(adjustl(chb))//'.dat'

		

	    open (unit=86+r, file=gf)
	! variables=x,y,z,P,PM,ppm,vis,vism,U,UM,uuM,V,VM,vvM,W,WM,wwM,uvM,uwM,vwM,ksgs,eps,epsm
        write (86+r,*)
     &  'VARIABLES= "x","z"',
     &	'"U","UM","uum","V","VM",vvm","W","WM","wwm"'

  
        write (86+r,'(a,i3,a)')'ZONE T="avg_zone:',r,'"'
	    write(86+r,*)'I=',(tti(r)-2*npl+1),', J=',(ttk(r)-2*npl+1),
     &'F=POINT'
	       do j= npl,ttj(r)-npl
        	do i= npl,tti(r)-npl
           write (86+r,88) avfx(i,j,r),avfy(i,j,r),
     &avfu(i,j,r),avfum(i,j,r),avfuum(i,j,r),
     &avfv(i,j,r),avfvm(i,j,r),avfvvm(i,j,r),avfw(i,j,r),
     &avfwm(i,j,r),avfwwm(i,j,r)
	end do
	end do

		close(86+r)

	end do

	deallocate(avy,avx,avuvm,avuwm,avvwm,avp,avpm,avppm,avvis)
        deallocate(avu,avum,avuum,avv,avvm,avvvm,avw,avwm,avwwm)
	deallocate(avvism,avksgs,aveps)
        deallocate(avfy,avfx,avfuvm,avfuwm,avfvwm,avfp)
        deallocate(avfpm,avfppm,avfvis,dmk)
        deallocate(avfu,avfum,avfuum,avfv)
        deallocate(avfvm,avfvvm,avfw,avfwm,avfwwm)
	deallocate(avfvism,avfksgs,avfeps)
        deallocate(dx,dy,dz,tti,ttj,ttk)


88      format (15e15.5)
        
	end program





