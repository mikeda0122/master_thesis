module mod_optmum3_gsearch

  use mod_parameter
  use mod_makegrids
  use mod_computePIA
  use mod_utility

  implicit none

contains

  subroutine optmum3_gsearch(age, A, AIME, Astate, AIMEstate, M, Copt, Aopt, valopt)

    implicit none

    
    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME
    real(8), intent(in) :: Astate(:), AIMEstate(:) 
    real(8), intent(in) :: M
   
    real(8), intent(out) :: valopt, Copt, Aopt

    integer(1) :: flag
    integer(8) :: Ci, i

    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: PIA, ss, laborincome, income, cashonhand
    real(8) :: nextperiodassets, utils, bequestutils
    real(8) :: val

    valopt = -10000000000.0_8

    PIA = computePIA(AIME)
    ss = PIA
    laborincome = 0.0_8
 
    income = laborincome

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

       nextperiodassets = cashonhand - C

       utils = U(C, 0.0_8, 0_1, M, 1_1)
!       utils = log(C)

       bequestutils = beq(nextperiodassets, 1_1)

       val = utils + p_beta*bequestutils
       
       if (val > valopt) then
          Copt = C
          Aopt = nextperiodassets
          valopt = val
       end if

    end do !End Ci loop

  end subroutine optmum3_gsearch
end module mod_optmum3_gsearch



    
    












    
