module mod_optmum3_gsearch

  use mod_parameter
  use mod_makegrids
  use mod_computeAfterTaxIncome
  use mod_computePIA
  use mod_ass
  use mod_pension
  use mod_utility

  implicit none

contains

  subroutine optmum3_gsearch(age, A, AIME, Astate, AIMEstate, M, Copt, Aopt, Iopt, pbopt, ssopt, valopt)

    implicit none

    
    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME
    real(8), intent(in) :: Astate(:), AIMEstate(:) 
    real(8), intent(in) :: M
   
    real(8), intent(out) :: valopt, Copt, Aopt, Iopt, pbopt, ssopt

    integer(1) :: flag
    integer(8) :: Ci, i

    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: PIA, ss, pb, laborincome, income, cashonhand
    real(8) :: nextperiodassets, utils, bequestutils
    real(8) :: MTR, reduc
    real(8) :: val

    valopt = -10000000000.0_8

    PIA = computePIA(AIME)
    ss = PIA
    pb = predictpensionbenefits(PIA, age)
!    pb = pb*2
!    pb = 0.0_8
    laborincome = 0.0_8
 
    income = computeaftertaxincome(laborincome, A, MTR, 0.0_8, pb, taxtype, age)

    cashonhand = ss + income + A
    flag = 0_1

    Cmin = cfloor
    Cmax = cashonhand

    do i = 1, Cnum
       Cstate(i) = Cmin + (i-1)*(Cmax-Cmin)/(Cnum-1)
    end do

    do Ci = 1, Cnum

       C = Cstate(Ci)

       if (cashonhand - Astate(1) < cfloor) then
          C = cfloor
       end if

       call ass(1_8, income, C, laborincome, A, ss, reduc, nextperiodassets)
       
       utils = U(C, 0.0_8, 0_1, M, nonsep)
!       utils = log(C)

       bequestutils = beq(nextperiodassets, nonsep)

       val = utils + p_beta*bequestutils
       
       if (val > valopt) then
          Copt = C
          Aopt = nextperiodassets
          Iopt = income
          pbopt = pb
          ssopt = ss
          valopt = val
       end if

    end do !End Ci loop

  end subroutine optmum3_gsearch
end module mod_optmum3_gsearch



    
    












    
