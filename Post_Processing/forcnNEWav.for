!##########################################################################
        program read
!##########################################################################
        implicit none
        integer no,i,totnum,stline
        real forcn,ctime,qst,qstp,flwsum
        character*80     :: dummy

        print*,'please enter the number of lines?'
        read(6,'(i8)')totnum
        print*,'you have ',totnum,' number of lines!'

        print*,'please enter the START line to start averaging?'
        read(6,'(i8)')stline
        print*,'you entered ',stline,' as a start line!'

        forcn=0.0
        open (unit=11, file='forcn.dat')
        open (unit=17, file='avg_dpdx.dat')

        read (11,*) dummy

        do i=1,totnum-1
           read (11,*) ctime,qst,qstp,flwsum
           if(i.ge.stline) forcn=forcn+qst
        end do

        forcn=forcn/(totnum-stline+1)

        write (17,*) forcn

        end program
!##########################################################################

