module mod_optmum2

  use mod_parameter
  use mod_makegrids
  use mod_computeAfterTaxIncome
  use mod_computePIA
  use mod_pension
  use mod_ass
  use mod_utility
  use mod_interp

  implicit none

contains

  subroutine optmum2(age, A, AIME, M, Vgood, Vbad, Astate, Wstate, AIMEstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Copt, Aopt, Iopt, pbopt, ssopt, valopt)

    implicit none

    
    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME
    real(8), intent(in) :: M
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)

    real(8), intent(out) :: valopt, Copt, Aopt, Iopt, pbopt, ssopt

    integer(1) :: flag
    integer(8) :: Ci, i
    integer(8) :: currentB

    real(8) :: PIA, ss, pb, laborincome, income, cashonhand
    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: nextperiodassets, utils, bequestutils
    real(8) :: MTR, reduc
    real(8) :: Evtgood, Evtbad, Evtpo, val

    valopt = -10000000000.0_8

    currentB = 1_8
    PIA = computePIA(AIME)
    ss = PIA
    pb = predictpensionbenefits(PIA, age)
!    pb = 2*pb
!    pb = 0.0_8
    laborincome = 0.0_8 
    income = computeaftertaxincome(laborincome, A, MTR, 0.0_8, pb, taxtype, age)

    cashonhand = ss + income + A

    Cmin = cfloor
    Cmax = cashonhand
!    Cmax = Astate(Anum)
    
    do i = 1, Cnum
       Cstate(i) = Cmin + (i-1)*(Cmax-Cmin)/(Cnum-1)
    end do

!    if (age==90_8 .and. A==Astate(1)) then
!       write(*,*) Cstate
!    end if
    
    do Ci = 1, Cnum

       flag = 0_1
       
       C = Cstate(Ci)

       if (cashonhand - Astate(1) < cfloor) then
          C = cfloor
       end if
       
       call ass(currentB, income, C, laborincome, A, ss, reduc, nextperiodassets)

       if (nextperiodassets < 0) then
          flag = 1_1
       end if
       
       utils = U(C, 0.0_8, 0_1, M, nonsep)
!       utils = log(C)

       bequestutils = beq(nextperiodassets, nonsep)

       Evtgood = interp(age, nextperiodassets, 0.0_8, AIME, Vgood, Astate, Wstate, AIMEstate, Anum, Wnum, AIMEnum, currentB)
       Evtbad = interp(age, nextperiodassets, 0.0_8, AIME, Vbad, Astate, Wstate, AIMEstate, Anum, Wnum, AIMEnum, currentB)
!       write(*,*) Evt

       if (M==0.0_8) then
          
          Evtpo = ((1.0_8 - mortality_good(age-20+1))*((1.0_8-good_to_bad(age-20+1))*Evtgood &
               & + good_to_bad(age-20+1)*Evtbad))+ mortality_good(age-20+1)*bequestutils
       else if (M==1.0_8) then
          
          Evtpo = ((1.0_8 - mortality_bad(age-20+1))*((1.0_8-bad_to_bad(age-20+1))*Evtgood &
               & + bad_to_bad(age-20+1)*Evtbad))+ mortality_bad(age-20+1)*bequestutils
       else
          write(*,*) 'Health is neither 0 nor 1!!'
       end if
       
!          Evtpo = Evt
!       write(*,*) mortality(age-20+1)

!       write(*,*) Evtpo
!       read*
       
       val = utils + p_beta*Evtpo
       
       if (val > valopt .and. flag==0_1) then
          Copt = C
          Aopt = nextperiodassets
          Iopt = income
          pbopt = pb
          ssopt = ss
          valopt = val
       end if

    end do !End Ci loop

  end subroutine optmum2
end module mod_optmum2

