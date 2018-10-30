module mod_optmum1

  use mod_parameter
  use mod_makegrids
  use mod_computeAIME
  use mod_computePIA
  use mod_utility
  use mod_interp
  use mod_integral

  implicit none

contains

  subroutine optmum1(age, A, AIME, W, M, Vgood, Vbad, Astate, AIMEstate, Wstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, Copt, Hopt, Aopt, valopt)

    implicit none
    
    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    real(8), intent(in) :: M
    real(8), intent(in) :: Vgood(:,:,:,:), Vbad(:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), Hstate(:), Wstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)

    real(8), intent(out) :: valopt, Copt, Hopt, Aopt

    integer(1) :: flag
    integer(8) :: Ci, Hi, pi, i, Wi

    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: H
    integer(8) :: currentB
    integer(1) :: particip
    real(8) :: PIA, ss, laborincome, income, cashonhand
    real(8) :: nextperiodassets, nextperiodAIME, utils, bequestutils
    real(8) :: wtpogood, wtpobad
    real(8) :: Evtgood, Evtbad, Evtpo, val

    valopt = -10000000000.0_8

    currentB = 0_8

    if (age<65) then
       currentB = 0_8
    else if (age>=65) then
       currentB = 1_8
    end if
    
    PIA = computePIA(AIME)
    ss = currentB*PIA
    
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

          if (M==0.0_8) then
             laborincome = H*W
          else if (M==1.0_8) then
             laborincome = H*W
          else
             write(*,*) 'Health is neither 0 nor 1!!'
          end if

          income = laborincome

          if (currentB==0_8) then
             call computeAIME(AIME, laborincome, age, currentB, nextperiodAIME)
          else if (currentB==1_8) then
             nextperiodAIME = AIME
          else
             write(*,*) 'something is wrong with nextperiodAIME!! in optmum1'
             read*
          end if

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

             utils = U(C, H, particip, M, 1_1)
             !                utils = log(C) + log(H)

             bequestutils = beq(nextperiodassets, 1_1)

             call nextwage(age, W, M, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, wtpogood, wtpobad)
             Evtgood = integral(age, nextperiodassets, wtpogood, nextperiodAIME, Vgood, Astate, Wstate, AIMEstate)
             Evtbad = integral(age, nextperiodassets, wtpobad, nextperiodAIME, Vbad, Astate, Wstate, AIMEstate)
             if(M == 0.0_8) then
                Evtpo = ((1.0_8-mortality_good(age-20+1))*((1.0_8-good_to_bad(age-20+1))*Evtgood &
                     + good_to_bad(age-20+1)*Evtbad) + mortality_good(age-20+1)*bequestutils)
             else if (M==1.0_8) then
                Evtpo = ((1.0_8-mortality_bad(age-20+1))*((1.0_8-bad_to_bad(age-20+1))*Evtgood &
                     +bad_to_bad(age-20+1)*Evtbad) + mortality_bad(age-20+1)*bequestutils)
             else                
                write(*,*) 'Health is neither 0 nor 1!!'
             end if


             !       write(*,*) Evt

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

     end subroutine optmum1
   end module mod_optmum1
   

    
    












    
