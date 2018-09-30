module mod_optimization

  use mod_parameter
  use mod_makegrids
  use mod_utility
  use mod_interp_A

  implicit none

contains

  subroutine optimization(age, A, V, Astate, Hstate, mortality, logwage, Copt, Hopt, Aopt, valopt)

    implicit none

    
    integer(8), intent(in) :: age
    real(8), intent(in) :: A
    
    real(8), intent(in) :: V(:,:)
    real(8), intent(in) :: Astate(:) , Hstate(:)
    real(8), intent(in) :: mortality(:), logwage(:)

    real(8), intent(out) :: valopt, Copt, Hopt, Aopt

    integer(1) :: flag
    integer(8) :: Ci, Hi, pi, i

    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: H
    integer(1) :: particip
    real(8) :: laborincome, income, cashonhand
    real(8) :: nextperiodassets, utils, bequestutils
    real(8) :: Evt, Evtpo, val

    valopt = -10000000000.0_8

    do pi = 1, 2
          do Hi = 1, Hnum
             if (pi == 1_8) then
                particip = 1_1
                H = Hstate(Hi)
             else if (pi == 2_8) then
                particip = 0_1
                H = 0.0_8
             else
                print*, 'This is not what you want!!'
                read*
             end if
             if (age>=70 .and. age < 95) then
                laborincome = 0.0_8
             else if (30 <= age .and. age < 70) then
                laborincome = H*exp(logwage(age-momage+1))
             else
                print*, 'This is not what you want!!'
                read*
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

                utils = U(C, H, particip, 0.0_8, 1_1)

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
                   Hopt = H
                   Aopt = nextperiodassets
                   valopt = val
                end if

             end do !End Ci loop
          end do !End Hi loop
       end do !end particip loop

  end subroutine optimization
end module mod_optimization

    
    












    
