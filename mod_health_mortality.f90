module mod_health_mortality
 implicit none

contains

 subroutine health_mortality(mortality_good, mortality_bad, good_to_bad, bad_to_bad)
  implicit none
  integer :: age, i
  character(1) :: comma1, comma2
  real(8), intent(out) :: mortality_good(75)
  real(8), intent(out) :: mortality_bad(75)
  real(8), intent(out) :: good_to_bad(75)
  real(8), intent(out) :: bad_to_bad(75)
  integer :: ios


  open(unit = 10, iostat = ios, file = 'health_transition.csv', action = 'read', form = 'formatted', &
       & status = 'old')
       if (ios /= 0) then
         write(*,*) 'Failed to open!'
         stop
       end if
  do i = 1,70,1
    read(10, '(i2, a, f10.8, a, f10.8)') age, comma1, good_to_bad(i), comma2, bad_to_bad(i)
!    write(*, '(i2, X, f10.8, X, f10.8)') age, good_to_bad(i), bad_to_bad(i)
  end do
  do i = 71,75,1
    good_to_bad(i) = good_to_bad(70)
    bad_to_bad(i) = bad_to_bad(70)
  end do  
  close(10)

  open(unit = 11, iostat = ios, file = 'death_probability.csv', action = 'read', form = 'formatted', &
       & status = 'old')
       if (ios /= 0) then
         write(*,*) 'Failed to open!'
         stop
       end if
       do i = 1,75,1
         read(11, '(i2, a, f10.8, a, f10.8)') age, comma1, mortality_bad(i), comma2, mortality_good(i)
!         write(*, '(i2, X, f10.8, X, f10.8)') age, mortality_bad(i), mortality_good(i)
       end do
  close(11)

 end subroutine health_mortality
end module mod_health_mortality
