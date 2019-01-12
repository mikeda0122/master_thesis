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
  use mod_getadj
  use mod_sprob

  implicit none

contains

  subroutine optmum1(age, A, AIME, B, W, M, Vgood, Vbad, Astate, AIMEstate, Wstate, Hstate, &
       mortality_good, mortality_bad, good_to_bad, bad_to_bad, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Bopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    integer(8), intent(in) :: B
    real(8), intent(in) :: M
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), Hstate(:), Wstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)

    real(8), intent(out) :: valopt, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt
    integer(8), intent(out) :: Bopt

    integer(1) :: flag
    integer(8) :: Ci, Hi, i, Wi, Bi

    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: H
    integer(8) :: currentB
    real(8) :: cumadj2, eretadj, bigcred, cumeretadj, litcred
    integer(1) :: particip, apply
    real(8) :: PIA, ss, ss2, pb, laborincome, income, cashonhand, adjAIME
    real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA
    real(8) :: tempnextAIME, tempnextPIA, earlyretirement, makeadjust
    real(8) :: MTR, reduc
    real(8) :: nextperiodassets, nextperiodAIME, utils, bequestutils
    real(8) :: wtpogood, wtpobad
    real(8) :: Evtgood, Evtbad, Evtpo, val

    valopt = -10000000000.0_8
     
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
          ss = ss*cumeretadj
       end if

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

          laborincome = computelaborincome(W, H)

          PIA = computePIA(AIME)
          pb = predictpensionbenefits(PIA, age)
          
          income = computeaftertaxincome(laborincome, A, MTR, W, pb, taxtype, age)

          call computeAIME(AIME, laborincome, age, currentB, tempnextAIME)
          tempnextPIA = computePIA(tempnextAIME)

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

             !Compute next period's AIME with adujustment if necessary

             call ass(currentB, income, C, laborincome, A, ss, reduc, nextperiodassets)

             earlyretirement = 1.0_8
             makeadjust = 0.0_8
                         
             if (currentB==1_8 .and. age<nret) then
                earlyretirement = earlyretirement*eretadj
!                earlyretirement = earlyretirement*cumeretadj
                makeadjust = 1.0_8
             else if (currentB==0_8 .and. age>=nret) then
                earlyretirement = earlyretirement*(1+litcred)
                makeadjust = 1.0_8
             end if

             if (currentB==1_8) then
                if(age<nret) then
                   earlyretirement = earlyretirement*(1+bigcred*reduc)
!                   earlyretirement = earlyretirement*(1+bigcred)
                else if (age>=nret) then
                   earlyretirement = earlyretirement*(1+litcred*reduc)
!                   earlyretirement = earlyretirement*(1+litcred)
                else
                   write(*,*) 'something wrong with earlyretirement!!'
                end if
                makeadjust = 1.0_8
             end if

             if (makeadjust==1.0_8) then
                call getnextPIA(tempnextPIA, earlyretirement, nextPIA)
!                nextPIA = tempnextPIA
                nextperiodAIME = findAIME(nextPIA)
             else
                call computeAIME(AIME, laborincome, age, currentB, nextperiodAIME)
             end if

             !adjust next period assets based on pension accrue
             call computepenaccrue(age, ageshift, laborincome, penacc1)
             nextPIA = computePIA(nextperiodAIME)
             nextpenbenpred = predictpensionbenefits(nextPIA, penbensstart+1)
             penbenpred = predictpensionbenefits(PIA, penbensstart+1)
             penacc2 = nextpenbenpred - penbenpred
             penacc2=penacc2*gvec(age + 1-bornage)
             nextperiodassets=nextperiodassets+penacc1-penacc2

             utils = U(C, H, particip, M, nonsep)

             bequestutils = beq(nextperiodassets, nonsep)

             !if (age==69) then
             !   Evtgood = interp(age, nextperiodassets, 0.0_8, AIME, Vgood, Astate, Wstate, AIMEstate, Anum, Wnum, AIMEnum, CurrentB)
             !   Evtbad = interp(age, nextperiodassets, 0.0_8, AIME, Vbad, Astate, Wstate, AIMEstate, Anum, Wnum, AIMEnum, CurrentB)
             !else

             call nextwage(age, W, M, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, wtpogood, wtpobad)

             !if (W==Wstate(7) .and. A==Astate(8) .and. AIME==AIMEstate(10)) then
             !write(*,*) 'before integral'
             !write(*,*) Hi, Bi, Ci
             !end if

             Evtgood = integral(age, nextperiodassets, wtpogood, nextperiodAIME, Vgood, Astate, Wstate, AIMEstate, currentB)
             !if (W==Wstate(7) .and. A==Astate(8) .and. AIME==AIMEstate(10)) then
             !write(*,*) 'integral done'
             !write(*,*) Hi, Bi, Ci
             !end if

             Evtbad = integral(age, nextperiodassets, wtpobad, nextperiodAIME, Vbad, Astate, Wstate, AIMEstate, currentB)
             !end if

             if(M == 0.0_8) then
                Evtpo = ((1.0_8-mortality_good(age-20+1))*((1.0_8-good_to_bad(age-20+1))*Evtgood &
                     + good_to_bad(age-20+1)*Evtbad) + mortality_good(age-20+1)*bequestutils)
             else if (M==1.0_8) then
                Evtpo = ((1.0_8-mortality_bad(age-20+1))*((1.0_8-bad_to_bad(age-20+1))*Evtgood &
                     +bad_to_bad(age-20+1)*Evtbad) + mortality_bad(age-20+1)*bequestutils)
             else
                write(*,*) 'Health is neither 0 nor 1!!'
             end if

             !write(*,*) mortality(age-20+1)

             !read*

             val = utils + p_beta*Evtpo
             if (val > valopt .and. currentB >= B) then
                Copt = C
                Hopt = H
                Aopt = nextperiodassets
                Bopt = currentB
                Wopt_good = wtpogood
                Wopt_bad = wtpobad
                AIMEopt = nextperiodAIME
                Iopt = income
                pbopt = pb
                ssopt = ss
                valopt = val
             end if

          end do !End Ci loop
       end do !End Hi loop

       if (age>=62 .and. B==1_8 .and. Bi==0_8) then
          exit
       end if

    end do !End Bi loop

  end subroutine optmum1
end module mod_optmum1
