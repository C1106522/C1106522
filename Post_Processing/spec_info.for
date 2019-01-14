	program domainsetup
	implicit none
	
!Domain resolution variables
	real :: xi,xe,dx
	real :: yi,ye,dy
	real :: zi,ze,dz
	integer :: i,j,k,xdiv,ydiv,zdiv
!Infodom variables--------------------------------------------------------------
!np=number of processors
!npe=number of processors starting from 0
!nd=number of subdomains
!ndo=number of subdomain assigned
!don_number=iteration to print the domain number
!ndpp=number of subdomain per processor
!rest_do=rest of subdomain to be assigned
	integer :: np,npe,nd,ndpp,ndo,dom_number,rest_do,nl,l,d

!Interation variable and array declartion--------------------------------------
	integer :: jmax,status,i_div,j_div,k_div
	integer,allocatable,dimension(:,:) :: main
        double precision,dimension(100) :: x,y,z

!User input--------------------------------------------------------------------	
	x=0; y=0 ; z=0
	open(20,file='sum_subdom.cin')
	  do d=1,3
	    read(20,*)
	  end do
	  read(20,*) xdiv
	  read(20,*)
	  do i=1,xdiv
	    read(20,*) x(i)
	  end do
	  do d=1,3
	    read(20,*) 
	  end do 
          read(20,*) ydiv
	  read(20,*)
	  do j=1,ydiv
	    read(20,*) y(j)
	  end do
          do d=1,3
	    read(20,*) 
	  end do 
          read(20,*) zdiv
	  read(20,*)
	  do k=1,zdiv
	    read(20,*) z(k)
	  end do
	close(20)
        
	print *, 'Enter the number of processor 1-16:'
	read *, np

!	print *, 'Enter the number of x div'
!	read *, xdiv
!	do i=1,xdiv
!	  write(6,'(a23,I4)') 'Enter the xloc for div:', i
!	  read *, x(i)
!	end do 
	
!	print *, 'Enter the number of y div'
!	read *, ydiv
!	do j=1,ydiv
!	  write(6,'(a23,I4)') 'Enter the yloc for div:', j
!	  read *, y(j)
!	end do 
	
!	print *, 'Enter the number of z div'
!	read *, zdiv
!	do k=1,zdiv
!	  write(6,'(a23,I4)') 'Enter the zloc for div:', k
!	  read *, z(k)
!	end do 
	
	nd=(xdiv-1)*(ydiv-1)*(zdiv-1)
	i_div=xdiv-1
	j_div=ydiv-1
	k_div=zdiv-1

	ndo=1
	dom_number=0
	
!INFODOM FILE
!------------------------------------------------------------------------	
	open (unit=30, file='infodom2.cin')
	
	write(*,*) 'Writing infodom.cin'

        write(30,*) nd, 'number of domains'
	write(30,*) '==============================================='
	
	do k=1,zdiv-1 ; do j=1,ydiv-1 ; do i=1,xdiv-1
	  write(30,20) dom_number, ndo, x(i),x(i+1), y(j), y(j+1), z(k), z(k+1) 
	  dom_number=dom_number+1
	enddo; enddo ;enddo
	write(30,*) '==============================================='
	write(30,*) i_div, 'number of divisions in i'
	write(30,*) j_div, 'number of divisions in j'
	write(30,*) k_div, 'number of divisions in k'
	
!	write(30,'(3(a5,8X))') 'xdiv:','ydiv:','zdiv:'
!	if(xdiv.ge.ydiv .and. xdiv.ge.zdiv) nl=xdiv
!	if(ydiv.ge.xdiv .and. xdiv.ge.zdiv) nl=ydiv
!	if(zdiv.ge.xdiv .and. zdiv.ge.ydiv) nl=zdiv
!	do l=1,nl
!	  write(30,'(3(I4,8X))') x(l),y(l),z(l)
!        end do 
20	format(2I4, 6F15.5)
	close(unit=30)
!------------------------------------------------------------------------

!MDMAP FILE
!------------------------------------------------------------------------
	
	ndpp=nd/np
	npe=np-1
	rest_do=nd-ndpp*np
	write(*,*) ndpp,rest_do
	allocate(main(0:np,0:ndpp+2),stat=status)

!ASSIGN VALUES TO ARRAY
!----------------------
	do i=0,npe,1
	  main(i,0)=i
	  main(i,1)=ndpp
	  main(i,2)=i
!	  write(*,*) main(i,0), main(i,1), main(i,2)
	enddo
	
	do i=0,npe,1
	  do j=3,ndpp+1
	  main(i,j)= main(i,j-1)+np
!	  write(*,*) main(i,j-1), main(i,j)
	  enddo
	enddo
	
	if(rest_do.gt.0) then
	  do i=0,rest_do-1,1
	    main(i,1)=main(i,1)+1
	    main(i,ndpp+2)=main(i,ndpp+1)+np
	  end do
	end if

!WRITE ARRAY IN .CIN FILE
!------------------------
	open(unit=10, file='mdmap2.cin')
	write(*,*) 'Writing mdmap.cin'
        write(10,*) nd, 'number of domain'
        write(10,*) np, 'number of processor'
        write(10,*) '==============================================='
	do i=0,npe,1
	  if (i.le.rest_do-1) then
	     jmax=ndpp+2
	  else
	     jmax=ndpp+1
	  end if
        write(10,'(*(I5))') (main(i,j),j=0,jmax)    
	enddo
        write(10,*) '==============================================='
	close(unit=10)
!------------------------------------------------------------------------

	write(*,*) 'Both of your files have been created! Go check them!'
	end program
	
