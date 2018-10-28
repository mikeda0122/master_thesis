module mod_profwage

  implicit none

contains

 subroutine profwage(ulogwage, hlogwage, hhgr, hugr, uhgr, uugr)

   implicit none

  integer :: age, i
  character(1) :: comma1, comma2, comma3, comma4
  real(8), intent(out) :: ulogwage(60), hlogwage(60)
  real(8), intent(out) :: hhgr(60), hugr(60), uhgr(60), uugr(60)

  integer :: ios

  open(unit = 75, iostat = ios, file ='profwage_log_u.csv', action = 'read', form = 'formatted', status ='old')
  if (ios /= 0) then
     write(*,*) 'Failed to open!'
     stop
  end if
  do i = 1, 60, 1
     read(75, *) ulogwage(i)
  end do
  close(75)

  open(unit = 76, iostat = ios, file ='profwage_log_h.csv', action = 'read', form = 'formatted', status ='old')
  if (ios /= 0) then
     write(*,*) 'Failed to open!'
     stop
  end if
  do i = 1, 60, 1
     read(76, *) hlogwage(i)
  end do
!  write(*,*) 'hlogwage', hlogwage
  close(76)

  open(unit = 71, iostat = ios, file = 'wage_growth_uugr.csv', action = 'read', form = 'formatted', status = 'old')
       if (ios /= 0) then
         write(*,*) 'Failed to open!'
         stop
       end if
  do i = 1,60,1 !!!69?
    read(71, *) uugr(i)
  end do
 
!  write(*,*) 'uugr', uugr
  close(71)

  open(unit = 72, iostat = ios, file = 'wage_growth_uhgr.csv', action = 'read', form = 'formatted', status = 'old')
       if (ios /= 0) then
         write(*,*) 'Failed to open!'
         stop
       end if
  do i = 1,60,1 !!!69?
    read(72, *) uhgr(i)
  end do
!  write(*,*) 'uhgr', uhgr
  close(72)

    open(unit = 73, iostat = ios, file = 'wage_growth_hugr.csv', action = 'read', form = 'formatted', &
       & status = 'old')
       if (ios /= 0) then
         write(*,*) 'Failed to open!'
         stop
       end if
  do i = 1,60,1 !!!69?
    read(73, *) hugr(i)
  end do
!  write(*,*) 'hugr', hugr
  close(73)

    open(unit = 74, iostat = ios, file = 'wage_growth_hhgr.csv', action = 'read', form = 'formatted', &
       & status = 'old')
       if (ios /= 0) then
         write(*,*) 'Failed to open!'
         stop
       end if
  do i = 1,60,1 !!!69?
    read(74, *) hhgr(i)
  end do
!  write(*,*) 'hhgr', hhgr
  close(74)

 end subroutine profwage
end module mod_profwage
