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

  subroutine optmum0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
       hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)

    implicit none


    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    real(8), intent(in) :: Cinit
    real(8), intent(in) :: M
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), Wstate(:), inCstate(:), Hstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)

    real(8), intent(out) :: valopt, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt,pbopt, ssopt

    integer(1) :: flag
    integer(8) :: Ci, initCi, Clen
    integer(8) :: Hi, i, Wi

    real(8) ::  C, Cmin, Cmax
    real(8), allocatable :: Cstate(:), p_Cstate(:), m_Cstate(:)
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

    integer(8) :: count

    valopt = -10000000000.0_8
    temp_valopt = -10000000000.0_8
    currentB = 0_8
    PIA = computePIA(AIME)
    pb = predictpensionbenefits(PIA, age)
    ss = 0.0_8 !PIA 
       
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

       if (nonsep==1_1) then          
          if (p_leispref - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
             cycle
          end if
       else
          if (5280.0_8 - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
             cycle
          end if
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

       if (Cinit<inCstate(2)) then
          initCi = 1
          Clen = size(inCstate)
          allocate(Cstate(Clen))
          Cstate = inCstate
       else if (Cinit>=inCstate(Cnum-1)) then
          initCi = Cnum
          Clen = size(inCstate)
          allocate(Cstate(Clen))
          do i = Cnum, 1, -1
             Cstate(Cnum-i+1) = inCstate(i)
          end do
       else if (inCstate(2)<=Cinit .and. Cinit<inCstate(Cnum-1)) then
          do i = 2, Cnum-1
             if (inCstate(i)<=Cinit .and. Cinit<inCstate(i+1)) then
                initCi = i
                exit
             end if
          end do
          
          call computeval0(age, A, W, M, inCstate(initCi+1), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
               Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valp)
          if (initCi<=Cnum-2) then
             call computeval0(age, A, W, M, inCstate(initCi+2), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
                  Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valpp)
          else if (initCi==Cnum-1) then
             valpp=vpanish
          else
             write(*,*) 'something is wrong with initCi!!'
          end if

          call computeval0(age, A, W, M, inCstate(initCi), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
               Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valm)
          call computeval0(age, A, W, M, inCstate(initCi-1), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
               Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valmm)
          allocate(p_Cstate(Cnum-initCi))
          allocate(m_Cstate(initCi))

          do i = initCi+1, Cnum
             p_Cstate(i-initCi) = inCstate(i)
          end do

          do i = 1, initCi
             m_Cstate(i) = inCstate(initCi-i+1)
          end do

          if (valmm>=valm .or. valm<vpanish/100 .or. valp<vpanish/100 .or. (valpp<vpanish/100 .and. valm>=valp))then
             Clen = size(m_Cstate)
             allocate(Cstate(Clen))
             Cstate = m_Cstate
          else if (valpp>=valp) then
             Clen = size(p_Cstate)
             allocate(Cstate(Clen))
             Cstate = p_Cstate
          else if (valp>=valm .or. (valpp<vpanish/100 .and. valm<valp)) then
             val = valp
             Clen = 1_8
             allocate(Cstate(Clen))
             Cstate(1) = inCstate(initCi+1)
          else if (valm>valp) then
             val = valm
             Clen = 1_8
             allocate(Cstate(Clen))
             Cstate(1) = inCstate(initCi-1)
          else
             write(*,*) 'Something is wrong with optmum1!!!'
             write(*,*) 'valpp=', valpp
             write(*,*) 'valp=', valp
             write(*,*) 'valm=', valm
             write(*,*) 'valmm=', valmm
             read*
          end if
       end if

       do Ci = 1, Clen

          if (Cstate(Ci)>cashonhand) then
             cycle
          end if

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

       if (inCstate(2)<=Cinit .and. Cinit<inCstate(Cnum-1)) then
          deallocate(p_Cstate)
          deallocate(m_Cstate)
          deallocate(Cstate)
       else if (Cinit<inCstate(2) .or. Cinit>=inCstate(Cnum-1)) then
          deallocate(Cstate)
       end if
       
    end do !End Hi loop

    if (count==7) then
       write(*,*) 'stop!!'
       read*
    end if
    
     end subroutine optmum0
   end module mod_optmum0
