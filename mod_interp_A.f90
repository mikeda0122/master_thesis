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

    ! adjusted age index: age - momage + 1
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
    else if (Astate(1) < A .and. Astate(Anum) > A) then
       do Ai = 1, Anum-1
          if(A< Astate(Ai+1) .and. A >= Astate(Ai)) then
             Aidx = Ai
             exit
          end if
       end do
    else
       write(*,*) 'Something Wrong with Interpolation !!'
       stop
    end if

    ! compute weights: Afrac, AIMEfrac, and Wfrac
    if (A >= Astate(Anum)) then
       Afrac = 1.0_8
    else if (A <= Astate(1)) then
       Afrac = 0.0_8
    else if (Astate(1) < A .and. Astate(Anum) > A) then
       Afrac = (A - Astate(Aidx))/(Astate(Aidx+1) - Astate(Aidx))
    else
       write(*,*) 'Something wrong with interpolation!!'
       stop
    end if

    if (Afrac > 1.0_8 .or. Afrac < 0.0_8) then
       print*, 'something wrong with interpolation'
       print*, Afrac
        read*
    end if
    
    if (Aidx == Anum) then
       Vinterp = V(ageadj+1, Aidx)
    else if (Aidx>=1_8 .and. Aidx < Anum) then
       Vinterp = (1.0_8-Afrac)*V(ageadj+1, Aidx)+(Afrac)*V(ageadj+1, Aidx+1)
    else
       write(*,*) 'something wrong with interpolation!!'
    end if

!    if (age==94 .and. Vinterp < -10.0_8) then   
!       write(*,*) 'ageadj=', ageadj
!       write(*,*) 'A=', A
!       write(*,*) 'Aidx=', Aidx
!       write(*,*) 'Astate=', Astate(Aidx)
!       write(*,*) 'Afrac=', Afrac
!       write(*,*) 'V=', V(ageadj+1, Aidx)
!       write(*,*) 'Vinterp=', Vinterp
!       read*
!    end if


    return

  end function interp
  
end module mod_interp_A
