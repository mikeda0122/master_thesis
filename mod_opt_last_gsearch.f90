module mod_opt_last_gsearch

  use mod_parameter
  use mod_makegrids
  use mod_utility

  implicit none

contains

  subroutine opt_last_gsearch(age, A, Astate, M, Copt, Aopt, valopt)

    implicit none

    
    integer(8), intent(in) :: age
    real(8), intent(in) :: A 
    real(8), intent(in) :: Astate(:) 
    real(8), intent(in) :: M
   
    real(8), intent(out) :: valopt, Copt, Aopt

    integer(1) :: flag
    integer(8) :: Ci, i

    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: laborincome, income, cashonhand
    real(8) :: nextperiodassets, utils, bequestutils
    real(8) :: val

    valopt = -10000000000.0_8

    laborincome = 0.0_8
 
    income = laborincome

    cashonhand = income + A
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

  end subroutine opt_last_gsearch
end module mod_opt_last_gsearch


    
    












    
