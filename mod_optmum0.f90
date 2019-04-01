module mod_optmum0

  use mod_parameter
  use mod_makegrids
  use mod_computelaborincome
  use mod_computeAfterTaxIncome
  use mod_computeaime
  use mod_computePIA
  use mod_computeval0
  use mod_ass
  use mod_pension
  use mod_utility
  use mod_interp
  use mod_integral
  use mod_sprob

  implicit none

contains

  subroutine optmum0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
       hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)

    implicit none


    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    real(8), intent(in) :: Cinit
    real(8), intent(in) :: M
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), Hstate(:), Wstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)

    real(8), intent(out) :: valopt, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt,pbopt, ssopt

    integer(1) :: flag
    integer(8) :: Ci, Hi, i, Wi

    real(8) :: Cstate(Cnum), p_Cstate(Cnum), m_Cstate(Cnum), C, Cmin, Cmax
    real(8) :: H
    integer(8) :: currentB
    integer(1) :: particip
    real(8) :: laborincome, income, cashonhand, pb, PIA, ss
    real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA
    real(8) :: MTR, reduc
    real(8) :: nextperiodassets, nextperiodAIME, utils, bequestutils, borrowamount
    real(8) :: wtpogood, wtpobad
    real(8) :: Evtgood, Evtbad, Evtpo
    real(8) :: utilsp, utilspp, utilsm, utilsmm
    real(8) :: Evtpop, Evtpopp, Evtpom, Evtpomm
    real(8) :: valp, valpp, valm, valmm, val

    real(8) :: temp_Cinit

    real(8) :: temp_valopt, temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt

    integer(8) :: Cglob
    real(8) :: plot_Cstate(Cnum*5)

    valopt = -10000000000.0_8
    temp_valopt = -10000000000.0_8
    currentB = 0_8
    PIA = computePIA(AIME)
    pb = predictpensionbenefits(PIA, age)
    ss = 0.0_8 !PIA !
    do Hi = 1, Hnum

       H = Hstate(Hi)
       if (H > 0.0_8) then
          particip = 1_1
       else if (H == 0.0_8) then
          particip = 0_1
       else
          print*, 'This is not what you want!!'
          read*
       end if

       if (p_leispref - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
          cycle
       end if

       laborincome = computelaborincome(W, H)

       income = computeaftertaxincome(laborincome, A, MTR, W, pb, taxtype, age)

       call computeAIME(AIME, laborincome, age, currentB, nextperiodAIME)

       borrowamount = 0.0_8
       if(liquid==0_1) then
         borrowamount = 0.6*nextperiodAIME*gvec(age)
       endif
       cashonhand = ss + income + A !+ borrowamount

       temp_valopt = -10000000000.0_8

       Cmin = cfloor
       Cmax = cashonhand

       if (Cmin>Cmax) then
          Cmax=Cmin
       end if

       if (Cinit>Cmax) then
          temp_Cinit=Cmax
       else
          temp_Cinit=Cinit
       end if

       do i = 1, Cnum
          p_Cstate(i) = temp_Cinit + (i-1)*(Cmax-temp_Cinit)/(Cnum-1)
       end do

       do i = 1, Cnum
          m_Cstate(i) = temp_Cinit + (i-1)*(Cmin-temp_Cinit)/(Cnum-1)
       end do

       call computeval0(age, A, W, M, temp_Cinit, H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
            Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, val)

       call computeval0(age, A, W, M, p_Cstate(2), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
            Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utilsp, Evtpop, valp)

       call computeval0(age, A, W, M, p_Cstate(3), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
            Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utilspp, Evtpopp, valpp)

       call computeval0(age, A, W, M, m_Cstate(2), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
            Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utilsm, Evtpom, valm)

       call computeval0(age, A, W, M, m_Cstate(3), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
            Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utilsmm, Evtpomm, valmm)

       if (valp>val) then
          Cstate = p_Cstate
       else if (valm>val) then
          Cstate = m_Cstate
       else if (val>=valm .and. val>=valp) then
          Cstate = p_Cstate
       else
          write(*,*) 'Value function is not concave in consumption!!'

          open(unit=43, file='values_cons.csv')
          write(43,"(A)") "C, utils, Evtpo, value"
          do i = 1, Cnum*5
             plot_Cstate(i) = Cmin + (i-1)*(Cmax-Cmin)/(Cnum*5-1)
          end do

          do i = 1, Cnum*5
             call computeval0(age, A, W, M, plot_Cstate(i), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
                  Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, val)
             write(43, '(f18.10, a, f18.10, a, f18.10, a, f18.10)') plot_Cstate(i), ',', utils, ',', Evtpo, ',', val
          end do

          close(43)
          read*
       end if

       do Ci = 1, Cnum

          C = Cstate(Ci)

          if (cashonhand - Astate(1) < cfloor) then
             C = cfloor
             nextperiodassets = Astate(1)
          end if

          call computeval0(age, A, W, M, C, H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
               Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, val)

          if (val > temp_valopt) then
             temp_Copt = C
             temp_Hopt = H
             temp_Aopt = nextperiodassets
             temp_Wopt_good = wtpogood
             temp_Wopt_bad = wtpobad
             temp_AIMEopt = nextperiodAIME
             temp_Iopt = income
             temp_pbopt = pb
             temp_ssopt = ss
             temp_valopt = val
          else if (val < temp_valopt) then
             exit
          end if

       end do !End Ci loop
       if (temp_valopt > valopt) then
          Copt = temp_Copt
          Hopt = temp_Hopt
          Aopt = temp_Aopt
          Wopt_good = temp_Wopt_good
          Wopt_bad = temp_Wopt_bad
          AIMEopt = temp_AIMEopt
          Iopt = temp_Iopt
          pbopt = temp_pbopt
          ssopt = temp_ssopt
          valopt = temp_valopt
       end if

    end do !End Hi loop

     end subroutine optmum0
   end module mod_optmum0
