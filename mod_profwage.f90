module mod_profwage
 implicit none

contains

 subroutine profwage(ulogwage, hlogwage)

   implicit none

  integer :: age, i
  character(1) :: comma1, comma2, comma3, comma4
  real(8), intent(out) :: ulogwage(60)
  real(8), intent(out) :: hlogwage(60)

  integer :: ios

  open(unit = 75, iostat = ios, file ='profwage_log_u.csv', action = 'read', form = 'formatted', status ='old')
  if (ios /= 0) then
     write(*,*) 'Failed to open!'
     stop
  end if
  do i = 1, 60, 1
     read(75, *) ulogwage(i)
  end do
!  write(*,*) 'ulogwage', ulogwage
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


 end subroutine profwage
end module mod_profwage
