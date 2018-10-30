module mod_interp

  use mod_parameter

  implicit none

contains

  function interp(age, A, W, AIME, V, Astate, Wstate, AIMEstate) result(Vinterp)

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: A, W, AIME
    real(8), intent(in) :: V(:,:,:,:)
    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)
    real(8), intent(out) :: Vinterp

    ! adjusted age index: age - momage + 1
    integer(8) :: ageadj

    integer(8) :: Aidx, Widx, AIMEidx
    integer(8) :: Ai, Wi, AIMEi
    real(8) :: Afrac, Wfrac, AIMEfrac

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

    ! locate AIME in terms of AIMEstate(:)
    if (AIME >= AIMEstate(AIMEnum)) then
        AIMEidx = AIMEnum
    else if (AIME <= AIMEstate(1)) then
       AIMEidx = 1
    else if (AIMEstate(1) < AIME .and. AIMEstate(AIMEnum) > AIME) then
       do AIMEi = 1, AIMEnum-1
          if(AIME< AIMEstate(AIMEi+1) .and. AIME >= AIMEstate(AIMEi)) then
             AIMEidx = AIMEi
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

    if (AIME >= AIMEstate(AIMEnum)) then
       AIMEfrac = 1.0d0
    else if (AIME <= AIMEstate(1)) then
       AIMEfrac = 0.0d0
    else if (AIMEstate(1) < AIME .and. AIMEstate(AIMEnum) > AIME) then
       AIMEfrac = (AIME - AIMEstate(AIMEidx))/(AIMEstate(AIMEidx+1) - AIMEstate(AIMEidx))
    else
       write(*,*) 'Something wrong with interpolation!!'
       write(*,*) age, AIME
       stop
    end if


    if (Afrac > 1.0_8 .or. Afrac < 0.0_8 .or. Wfrac > 1.0_8 .or. Wfrac < 0.0_8) then
       print*, 'something wrong with interpolation'
       print*, Afrac, Wfrac
        read*
    end if
    
    if (Aidx == Anum) then
       if (AIME == AIMEnum) then
          if (Widx == Wnum) then
             Vinterp = Afrac*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx)
          else
             Vinterp = Afrac*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx) + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx))
          end if
       else
          if (Widx == Wnum) then
             Vinterp = Afrac*(1.0_8-AIMEfrac)*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx) + Afrac*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx+1, Widx, Aidx)
          else
             Vinterp = Afrac*(1.0_8-AIMEfrac)*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx) + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx)) &
                  + Afrac*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx+1, Widx, Aidx) + Wfrac*V(ageadj+1, AIMEidx+1, Widx+1, Aidx))
          end if
       end if
    else
       if (AIME == AIMEnum) then
          if (Widx == Wnum) then
             Vinterp = (1.0_8-Afrac)*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx) + Afrac*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx+1)
          else
             Vinterp = (1.0_8-Afrac)*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx) + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx)) + Afrac*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx+1) + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx+1))
          end if
       else
          if (Widx == Wnum) then
             Vinterp = (1.0_8-Afrac)*(1.0_8-AIMEfrac)*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx) + (1.0_8-Afrac)*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx+1, Widx, Aidx) &
                  + Afrac*(1.0_8-AIMEfrac)*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx+1) + Afrac*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx+1, Widx, Aidx+1) 
          else
             Vinterp = (1.0_8-Afrac)*(1.0_8-AIMEfrac)*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx) + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx)) &
                  + (1.0_8-Afrac)*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx+1, Widx, Aidx) + Wfrac*V(ageadj+1, AIMEidx+1, Widx+1, Aidx)) + &
                  Afrac*(1.0_8-AIMEfrac)*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx+1) + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx+1)) &
                  + Afrac*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx+1, Widx, Aidx+1) + Wfrac*V(ageadj+1, AIMEidx+1, Widx+1, Aidx+1))
          end if
       end if
    end if

    return

  end function interp
  
end module mod_interp

