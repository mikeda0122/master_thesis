module mod_computeval0

  use mod_parameter
  use mod_ass
  use mod_pension
  use mod_computePIA
  use mod_utility
  use mod_integral

  implicit none

contains

  subroutine computeval0(age, A, W, M, C, H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, val)

      implicit none
    integer(8), intent(in) :: age
    real(8), intent(in) :: A, W
    real(8), intent(in) :: M

    real(8), intent(in) :: C, H

    integer(1), intent(in) :: particip
    integer(8), intent(in) :: currentB

    real(8), intent(in) :: nextperiodAIME
    real(8), intent(in) :: income, ss, laborincome, PIA

    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: ageshift(:), gvec(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)

    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)

    real(8), intent(out) :: wtpogood, wtpobad
    real(8), intent(out) :: nextperiodassets
    real(8), intent(out) :: utils, Evtpo
    real(8), intent(out) :: val

    real(8) :: reduc

    real(8) :: penacc1, penacc2
    real(8) :: nextPIA, nextpenbenpred, penbenpred

    real(8) :: bequestutils

    real(8) :: Evtgood, Evtbad


    call ass(age, currentB, income, C, laborincome, A, ss, reduc, nextperiodassets)

!    if (age==penage-1) then
!       call computepenaccrue(age, ageshift, laborincome, penacc1)
!       nextPIA = computePIA(nextperiodAIME)
!       nextpenbenpred = predictpensionbenefits(nextPIA, penbensstart+1)
!       penbenpred = predictpensionbenefits(PIA, penbensstart+1)
!       penacc2 = nextpenbenpred - penbenpred
!       penacc2=penacc2*gvec(age-bornage+1)
!       nextperiodassets=nextperiodassets+penacc1-penacc2
!    end if
    utils = U(C, H, particip, M, nonsep)

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

    val = utils + p_beta*Evtpo

    if (val > -100000.0_8 .and. val < 100000.0_8) then
       return
    else
       !For those who works more than their time endowment.
       write(*,*) 'val', val
       write(*,*) 'utils', utils, 'Evtpo', Evtpo
       val = -1000000000.0_8
       write(*,*) 'asset', nextperiodassets, 'C', C
       write(*,*) 'H', H, 'particip', particip
       write(*,*) 'M', M, 'nonsep', nonsep
       write(*,*) 'You are not supposed to be here!!'
       read*
    end if

  end subroutine computeval0
end module mod_computeval0
