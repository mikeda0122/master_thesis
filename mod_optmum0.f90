module mod_optmum0

  use mod_parameter
  use mod_makegrids
  use mod_computelaborincome
  use mod_computeAfterTaxIncome
  use mod_computeaime
  use mod_computePIA
  use mod_ass
  use mod_pension
  use mod_utility
  use mod_interp
  use mod_integral
  use mod_sprob
  
  implicit none

contains

  subroutine optmum0(age, A, AIME, W, M, Vgood, Vbad, Astate, AIMEstate, Wstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
       hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, valopt)

    implicit none

    
    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    real(8), intent(in) :: M
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), Hstate(:), Wstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)
    
    real(8), intent(out) :: valopt, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt

    integer(1) :: flag
    integer(8) :: Ci, Hi, i, Wi

    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: H
    integer(8) :: currentB
    integer(1) :: particip
    real(8) :: laborincome, income, cashonhand, pb, PIA, ss
    real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA
    real(8) :: MTR, reduc
    real(8) :: nextperiodassets, nextperiodAIME, utils, bequestutils
    real(8) :: wtpogood, wtpobad
    real(8) :: Evtgood, Evtbad, Evtpo, val

    valopt = -10000000000.0_8
    currentB = 0_8
    PIA = computePIA(AIME)
    pb = predictpensionbenefits(PIA, age)
    ss = 0.0_8
    
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

       income = computeaftertaxincome(laborincome, A, MTR, W, pb, taxtype, age)

       call computeAIME(AIME, laborincome, age, currentB, nextperiodAIME)

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

          call ass(currentB, income, C, laborincome, A, ss, reduc, nextperiodassets)

!         if (age==penage-1) then
!             call computepenaccrue(age, ageshift, laborincome, penacc1)
!             nextPIA = computePIA(nextperiodAIME)
!             nextpenbenpred = predictpensionbenefits(nextPIA, penbensstart+1)
!             penbenpred = predictpensionbenefits(PIA, penbensstart+1)
!             penacc2 = nextpenbenpred - penbenpred
!             penacc2=penacc2*gvec(age-bornage+1)
!             nextperiodassets=nextperiodassets+penacc1-penacc2
!          end if
          
          utils = U(C, H, particip, M, nonsep)
          !                utils = log(C) + log(H)

          bequestutils = beq(nextperiodassets, nonsep)

          call nextwage(age, W, M, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, wtpogood, wtpobad)
          Evtgood = integral(age, nextperiodassets, wtpogood, nextperiodAIME, Vgood, Astate, Wstate, AIMEstate, currentB)
          Evtbad = integral(age, nextperiodassets, wtpobad, nextperiodAIME, Vbad, Astate, Wstate, AIMEstate, currentB)
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
             Wopt_good = wtpogood
             Wopt_bad = wtpobad
             AIMEopt = nextperiodAIME
             Iopt = income
             valopt = val
          end if

       end do !End Ci loop
    end do !End Hi loop

     end subroutine optmum0     
   end module mod_optmum0
 









    
