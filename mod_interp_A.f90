module mod_interp_A

  use mod_parameter

  implicit none

contains

  function interp(age, A, V, Astate) result(Vinterp)

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: A
    real(8), intent(in) :: V(:,:)
    real(8), intent(in) :: Astate(:)
    real(8), intent(out) :: Vinterp

    ! adjusted age index: age - mappage + 1
    integer(8) :: ageadj

    integer(8) :: Aidx
    integer(8) :: Ai
    real(8) :: Afrac

    ! starting from age workage=30
    ageadj = age - momage + 1_8

    ! locate A in terms of Astate(:)
    ! how to deal with extrapolation?
    if (A >= Astate(Anum)) then
        Aidx = Anum
    else if (A <= Astate(1)) then
       Aidx = 1
    else
       do Ai = 1, Anum-1
          if(A< Astate(Ai+1) .and. A >= Astate(Ai)) then
             Aidx = Ai
             exit
          end if
       end do
    end if

    ! compute weights: Afrac, AIMEfrac, and Wfrac
    if (Aidx == Anum) then
       Afrac = 1.0_8
    else if (A < Astate(1)) then
       Afrac = 1.0_8
    else
       Afrac = (A - Astate(Aidx))/(Astate(Aidx+1) - Astate(Aidx))
    end if

    if (Afrac > 1.0_8 .or. Afrac < 0.0_8) then
       print*, 'something wrong with interpolation'
       print*, Afrac
        read*
    end if
     
!    write(*,*) 'ageadj=', ageadj
!    write(*,*) 'Aidx=', Aidx
!    write(*,*) 'V=', V(ageadj+1, Aidx) 
    
    if (Aidx == Anum) then
       Vinterp = V(ageadj+1, Aidx)
    else
       Vinterp = Afrac*V(ageadj+1, Aidx)+(1.0_8-Afrac)*V(ageadj+1, Aidx+1)
    end if
    return

  end function interp
  
end module mod_interp_A
