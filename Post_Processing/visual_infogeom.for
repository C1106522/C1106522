	program view_zone_infodom
	implicit none

	
	integer :: ndoms,ndo,i,j,k,bodynum,ibm,u,l,tlines,strlen
	real,allocatable,dimension(:,:) :: xcor,ycor,zcor
	real,allocatable,dimension(:) :: cx,cy,cz
	real,allocatable,dimension(:) :: xval,yval,zval
	integer,allocatable,dimension(:) :: rdiv
	character*20,allocatable,dimension(:) :: fn
	character*8 :: char_block
	character*31 :: newname
	character(len=50) :: dummyline

	print *, 'Make sure infodom.cin,geom.cin are present'
	print *, 'as well as you geometry file with the line number at the top'
	print *, 'to visualise your geometries and domain:'
	print *, 'tec360 graphic_infodom.dat prepross_*'

! Read infodom and create the tecplot subdomain---------------------------------
	open(unit=10,file='infodom.cin')
	  read(10,*) ndoms
	  read(10,*) dummyline
	  allocate (xcor(0:ndoms-1,2),ycor(0:ndoms-1,2),zcor(0:ndoms-1,2))
	  allocate (rdiv(0:ndoms-1)) !LMR

	  do i=0,ndoms-1
		read(10,*) ndo,rdiv(ndo),xcor(ndo,1),xcor(ndo,2),ycor(ndo,1),
     & ycor(ndo,2),zcor(ndo,1),zcor(ndo,2)
	  end do
	close(unit=10)

	open(unit=20,file='prepross_infodom.dat')
	  write(20,*) 'TITLE="view_zone_infodom"'
	  write(20,*) 'VARIABLES="X","Y","Z"'
	  do ndo=0,ndoms-1
	write(20,*) 'ZONE T="SUBDOMAIN',ndo,'", I=2,J=2,K=2, DATAPACKING=POINT'
		do k=1,2
		  do j=1,2
			do i=1,2
			  write(20,20) xcor(ndo,i),ycor(ndo,j),zcor(ndo,k)
			end do
		  end do
		end do
	  end do

20	format(3F11.4) !Sometimes the format will require changes

	close(unit=20)


! Read geom.cin and add the cx,cy,cz to the initial geom cordinates-------------
	open(unit=30,file='geom.cin')
	  do i=1,4
	    read(30,*) dummyline
	  end do
	  read(30,*) bodynum
	  allocate(cx(1:bodynum),cy(1:bodynum),cz(1:bodynum))
	  allocate(fn(1:bodynum))
	  do ibm=1,bodynum
            do i=1,3
              read(30,*) dummyline
            end do
	    read(30,*) cx(ibm),cy(ibm),cz(ibm)
	    do i=1,3
              read(30,*) dummyline
            end do
	    read(30,*) fn(ibm)
	    do i=1,9
	      read(30,*) dummyline
	    end do
          end do
	close(unit=30)
	
	print *, 'geometry filenames:'
	do ibm=1,bodynum
	  print *, fn(ibm)
	end do

	u=31
	do ibm=1,bodynum
	  open(unit=u,file=fn(ibm))
	    read(u,*) tlines
	    allocate(xval(1:tlines),yval(1:tlines),zval(1:tlines))
	    do l=1,tlines
	      read(u,*) xval(l),yval(l),zval(l)
	    end do
	  close(unit=u)
          u=u+1
	
          newname='prepross_'//TRIM(ADJUSTL(fn(ibm)))

          open (unit=u, file=newname)
	    write(u,*) 'VARIABLES="X","Y","Z"'
	    do l=1,tlines
            write(u,10) xval(l)+cx(ibm),yval(l)+cy(ibm),zval(l)+cz(ibm)
            end do 
          close(unit=u)
	  u=u+1
	  deallocate(xval,yval,zval)
	end do 

10	format(3F11.4) !Sometimes the format will require changes
        
	end program
