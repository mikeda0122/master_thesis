module mod_interp_A_W

  use mod_parameter

  implicit none

contains

  function interp(age, A, W, V, Astate, Wstate) result(Vinterp)

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: A, W
    real(8), intent(in) :: V(:,:,:)
    real(8), intent(in) :: Astate(:), Wstate(:)
    real(8), intent(out) :: Vinterp

    ! adjusted age index: age - momage + 1
    integer(8) :: ageadj

    integer(8) :: Aidx, Widx
    integer(8) :: Ai, Wi
    real(8) :: Afrac, Wfrac

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

        ! locate W in terms of Wstate(:)
    if (W >= Wstate(Wnum)) then
        Widx = Wnum
    else if (W <= Wstate(1)) then
       Widx = 1
    else if (Wstate(1) < W .and. Wstate(Wnum) > W) then
       do Wi = 1, Wnum-1
          if(W< Wstate(Wi+1) .and. W >= Wstate(Wi)) then
             Widx = Wi
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

    if (W >= Wstate(Wnum)) then
       Wfrac = 1.0d0
    else if (W <= Wstate(1)) then
       Wfrac = 0.0d0
    else if (Wstate(1) < W .and. Wstate(Wnum) > W) then
       Wfrac = (W - Wstate(Widx))/(Wstate(Widx+1) - Wstate(Widx))
    else
       write(*,*) 'Something wrong with interpolation!!'
       stop
    end if


    if (Afrac > 1.0_8 .or. Afrac < 0.0_8 .or. Wfrac > 1.0_8 .or. Wfrac < 0.0_8) then
       print*, 'something wrong with interpolation'
       print*, Afrac, Wfrac
        read*
    end if
    
    if (Aidx == Anum) then
       if (Widx == Wnum) then
          Vinterp = Afrac*Wfrac*V(ageadj+1, Widx, Aidx)
       else
          Vinterp = Afrac*((1.0_8-Wfrac)*V(ageadj+1, Widx, Aidx) + Wfrac*V(ageadj+1, Widx+1, Aidx))
       end if
    else
       if (Widx==Wnum) then
          Vinterp = (1.0_8-Afrac)*Wfrac*V(ageadj+1, Widx, Aidx)+(Afrac)*Wfrac*V(ageadj+1, Widx, Aidx+1)
       else
          Vinterp = (1.0_8-Afrac)*((1.0_8-Wfrac)*V(ageadj+1, Widx, Aidx)+Wfrac*V(ageadj+1, Widx+1, Aidx))+ &
               Afrac*((1.0_8-Wfrac)*V(ageadj+1, Widx, Aidx+1)+Wfrac*V(ageadj+1, Widx+1, Aidx+1))
       end if
    end if

    return

  end function interp
  
end module mod_interp_A_W
