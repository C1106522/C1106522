!##########################################################################
	program preplot

	implicit none

	integer :: ib,nb,it,nt
	integer :: i,j,k,tti,ttj,ttk
	integer :: ttimax,ttjmax,ttkmax
	integer :: sn,sn1
	integer :: t,tstart,tend,tint
	real, allocatable, dimension(:,:,:,:) :: x,y,z,phi,phisum
	real, allocatable, dimension(:,:,:,:) :: phiav
      character*8 :: chb,chb1
      character*31 :: gridfile,gridfile1,gridfilegz,dummy
	character*60	 :: command

	write(*,*) 'Enter the first time level (i.e. 1000) :'
	read(*,*) tstart
	write(*,*) 'Enter the final time level (i.e. 50000) :'
	read(*,*) tend
	write(*,*) 'Enter number of time steps between outputs :'
	read(*,*) tint
	write(*,*) 'Enter number of MPI blocks :'
	read(*,*) nb

	nt = (tstart-tend)/tint

	ttimax=0;ttjmax=0;ttkmax=0
	do ib=0,nb-1
	  t = tstart
	  do while (t.le.tend) !it=1,nt
          write(chb,'(i8)') ib
          write(chb1,'(i8)') t
          sn=len(trim(adjustl(chb)))
          sn1=len(trim(adjustl(chb1)))
          chb=repeat('0',(4-sn))//trim(adjustl(chb))
          chb1=repeat('0',(6-sn1))//trim(adjustl(chb1))
          gridfilegz='tecgrid'//trim(adjustl(chb))//'_'//
     & trim(adjustl(chb1))//'.dat.gz'	    
          gridfile='tecgrid'//trim(adjustl(chb))//'_'//
     & trim(adjustl(chb1))//'.dat'

	    write(*,*) 'Unzipping:',gridfilegz
          command='/bin/gunzip '//
     & trim(adjustl(gridfilegz))
          CALL SYSTEM (command)

	    write(*,*) 'Preplotting:',gridfile
          command='/opt/tec360/tecplot/bin/preplot '//
     & trim(adjustl(gridfile))
          CALL SYSTEM (command)

	    write(*,*) 'Zipping:',gridfile
          command='/bin/gzip '//
     & trim(adjustl(gridfile))
          CALL SYSTEM (command)
	  
	    t=t+tint
	  enddo
	enddo

	end program
