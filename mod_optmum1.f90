module mod_optmum1

  use mod_parameter
  use mod_makegrids
  use mod_computelaborincome
  use mod_computeAfterTaxIncome
  use mod_computeAIME
  use mod_computePIA
  use mod_pension
  use mod_ass
  use mod_utility
  use mod_interp
  use mod_integral
  use mod_computeval1
  use mod_getadj
  use mod_sprob

  implicit none

contains

  subroutine optmum1(age, A, AIME, B, W, M, Cinit, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, Hstate, &
       mortality_good, mortality_bad, good_to_bad, bad_to_bad, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Bopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    integer(8), intent(in) :: B
    real(8), intent(in) :: M
    real(8), intent(in) :: Cinit
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), Hstate(:), Wstate(:), inCstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)

    real(8), intent(out) :: valopt, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt
    integer(8), intent(out) :: Bopt

    integer(1) :: flag
    integer(8) :: Ci, initCi, Clen
    integer(8) :: Hi, i, Wi, Bi

    real(8) :: C, Cmin, Cmax
    real(8), allocatable :: Cstate(:), p_Cstate(:), m_Cstate(:)
    real(8) :: H
    integer(8) :: currentB
    real(8) :: cumadj2, eretadj, bigcred, cumeretadj, litcred
    integer(1) :: particip, apply
    real(8) :: PIA, ss, ss2, pb, laborincome, income, cashonhand, adjAIME
    real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA
    real(8) :: tempnextAIME, tempnextPIA, earlyretirement, makeadjust
    real(8) :: MTR, reduc
    real(8) :: borrowamount, nextperiodassets, nextperiodAIME, utils, bequestutils
    real(8) :: wtpogood, wtpobad
    real(8) :: Evtgood, Evtbad, Evtpo
    real(8) :: valp, valpp, valm, valmm, val

    real(8) :: temp_valopt, temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt
    integer(8) :: temp_Bopt
    real(8) :: temp2_valopt, temp2_Copt, temp2_Hopt, temp2_Aopt, temp2_Wopt_good, temp2_Wopt_bad, temp2_AIMEopt, temp2_Iopt, temp2_pbopt, temp2_ssopt
    integer(8) :: temp2_Bopt

    integer(8) :: Cglob, modval

    Cglob = 0_8
    modval = 1_8
    
    valopt = -10000000000.0_8
    temp_valopt = -10000000000.0_8

    do Bi = 0, 1

       if(B == 1_8) then
          currentB = B
          apply = 0_8
       else
          currentB = Bi
          if (currentB==1_8) then
             apply = 1_8
          else
             apply = 0_8
          end if
       end if

       PIA = computePIA(AIME)
       ss = currentB*PIA

       call getadj(age, currentB, cumadj2, eretadj, bigcred, cumeretadj, litcred)

!       if (apply==1_8 .and. age<nret) then
       if (currentB == 1_8 .and. age<nret) then
          ss = ss *cumeretadj
       end if

       temp_valopt = -10000000000.0_8

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

          PIA = computePIA(AIME)
          pb = predictpensionbenefits(PIA, age)

          income = computeaftertaxincome(laborincome, A, MTR, W, pb, taxtype, age)

          call computeAIME(AIME, laborincome, age, currentB, tempnextAIME)
          tempnextPIA = computePIA(tempnextAIME)

          borrowamount = 0.0_8
          if(liquid==0_1) then
            borrowamount = 0.6*tempnextAIME*gvec(age)
          endif

          cashonhand = ss + income + A ! + borrowamount
          flag = 0_1

          Cmin = cfloor
          Cmax = cashonhand

          if (Cglob==0) then
             if (Cinit<inCstate(2)) then
                initCi = 1
                Clen = size(inCstate)
                Cstate = inCstate
             else if (Cinit>=inCstate(Cnum-1)) then
                initCi = Cnum
                Clen = size(inCstate)
                do i = Cnum, 1, -1
                   Cstate(i) = inCstate(i)
                end do
             else if (inCstate(2)<=Cinit .and. Cinit<inCstate(Cnum)) then
                do i = 2, Cnum-1
                   if (inCstate(i)<=Cinit .and. Cinit<inCstate(i+1)) then
                      initCi = i
                      exit
                   end if
                end do

                call computeval1(age, A, W, AIME, M, inCstate(initCi+1), H, particip, currentB, tempnextPIA,income, ss, laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, eretadj, bigcred, cumeretadj, litcred, Vgood, Vbad, nextperiodassets, nextperiodAIME, wtpogood, wtpobad, valp)
                if (initCi<=Cnum-2) then
                   call computeval1(age, A, W, AIME, M, inCstate(initCi+2), H, particip, currentB, tempnextPIA, income, ss, laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, eretadj, bigcred, cumeretadj, litcred, Vgood, Vbad, nextperiodassets, nextperiodAIME, wtpogood, wtpobad, valpp)
                else if (initCi==Cnum-1) then
                   valpp=-100000000000
                else
                   write(*,*) 'something is wrong with initCi!!!!'
                end if
                call computeval1(age, A, W, AIME, M, inCstate(initCi), H, particip, currentB, tempnextPIA, income, ss, laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, eretadj, bigcred, cumeretadj, litcred, Vgood, Vbad, nextperiodassets, nextperiodAIME, wtpogood, wtpobad, valm)
                call computeval1(age, A, W, AIME, M, inCstate(initCi-1), H, particip, currentB, tempnextPIA, income, ss, laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, eretadj, bigcred, cumeretadj, litcred, Vgood, Vbad, nextperiodassets, nextperiodAIME, wtpogood, wtpobad, valmm)

                allocate(p_Cstate(Cnum-initCi))
                allocate(m_Cstate(initCi))

                do i = initCi+1, Cnum
                   p_Cstate(i-initCi) = inCstate(i)
                end do

                do i = 1, initCi
                   m_Cstate(i) = inCstate(initCi-i+1)
                end do

                if (valpp>=valp) then
                   Clen = size(p_Cstate)
                   allocate(Cstate(Clen))
                   Cstate = p_Cstate
                else if (valmm>=valm) then
                   Clen = size(m_Cstate)
                   allocate(Cstate(Clen))
                   Cstate = m_Cstate
                else if (valp>=valm) then
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
          else
             Clen = size(inCstate)
             allocate(Cstate(Clen))
             Cstate=inCstate
          end if
          
          temp2_valopt = -10000000000.0_8

          do Ci = 1, Clen
             if (Cstate(Ci)>cashonhand) then
                cycle
             end if
             
             C = Cstate(Ci)

             if (cashonhand - Astate(1) < cfloor) then
                C = cfloor
!                nextperiodassets = Astate(1)
             end if

             call computeval1(age, A, W, AIME, M, C, H, particip, currentB, tempnextPIA, income, ss, laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, eretadj, bigcred, cumeretadj, litcred, Vgood, Vbad, nextperiodassets, nextperiodAIME, wtpogood, wtpobad, val)
             
             if (val > temp2_valopt .and. currentB >= B) then
                temp2_Copt = C
                temp2_Hopt = H
                temp2_Aopt = nextperiodassets
                temp2_Bopt = currentB
                temp2_Wopt_good = wtpogood
                temp2_Wopt_bad = wtpobad
                temp2_AIMEopt = nextperiodAIME
                temp2_Iopt = income
                temp2_pbopt = pb
                temp2_ssopt = ss
                temp2_valopt = val
             else if (val<temp2_valopt) then
                exit
             end if

          end do !End Ci loop

          if (temp2_valopt > temp_valopt .and. temp2_Bopt >= B) then
             temp_Copt = temp2_Copt
             temp_Hopt = temp2_Hopt
             temp_Aopt = temp2_Aopt
             temp_Bopt = temp2_Bopt
             temp_Wopt_good = temp2_Wopt_good
             temp_Wopt_bad = temp2_Wopt_bad
             temp_AIMEopt = temp2_AIMEopt
             temp_Iopt = temp2_Iopt
             temp_pbopt = temp2_pbopt
             temp_ssopt = temp2_ssopt
             temp_valopt = temp2_valopt
          end if

          if (Cglob==0) then
             deallocate(p_Cstate)
             deallocate(m_Cstate)
          end if
          
          deallocate(Cstate)

       end do !End Hi loop

       if (temp_valopt > valopt .and. temp_Bopt >= B) then
          Copt = temp_Copt
          Hopt = temp_Hopt
          Aopt = temp_Aopt
          Bopt = temp_Bopt
          Wopt_good = temp_Wopt_good
          Wopt_bad = temp_Wopt_bad
          AIMEopt = temp_AIMEopt
          Iopt = temp_Iopt
          pbopt = temp_pbopt
          ssopt = temp_ssopt
          valopt = temp_valopt
       end if

       if (age>=62 .and. B==1_8 .and. Bi==0_8) then
          exit
       end if

    end do !End Bi loop

  end subroutine optmum1
end module mod_optmum1
