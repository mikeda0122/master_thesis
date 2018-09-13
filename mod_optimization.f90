module mod_optimization

  use mod_parameter
  use mod_makegrids
  use mod_utility
  use mod_interp_A

  implicit none

contains

  subroutine optimization(age, A, V, Astate, mortality, Copt, Aopt, valopt)

    implicit none

    
    integer(8), intent(in) :: age
    real(8), intent(in) :: A
    
    real(8), intent(in) :: V(:,:)
    real(8), intent(in) :: Astate(:) 
    real(8), intent(in) :: mortality(:)

    real(8), intent(out) :: valopt, Copt, Aopt

    integer(1) :: flag
    integer(8) :: Ci, i

    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: laborincome, income, cashonhand
    real(8) :: nextperiodassets, utils, bequestutils
    real(8) :: Evt, Evtpo, val

    valopt = -10000000000.0_8

    if (age <=70) then
       laborincome = 2000.0_8
    else
       laborincome = 0.0_8
    end if
 
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

       utils = U(C, 0.0_8, 0_1, 0.0_8, 1_1)

       bequestutils = beq(nextperiodassets, 1_1)

       Evt = interp(age, nextperiodassets, V, Astate)

!       write(*,*) Evt
       
       Evtpo = (1.0_8 - mortality(age-20+1))*Evt + mortality(age-20+1)*bequestutils
!       write(*,*) mortality(age-20+1)

!       write(*,*) Evtpo
!       read*
       
       val = utils + p_beta*Evtpo
       if (val > valopt) then
          Copt = C
          Aopt = nextperiodassets
          valopt = val
       end if

    end do !End Ci loop

  end subroutine optimization
end module mod_optimization

    
    












    
