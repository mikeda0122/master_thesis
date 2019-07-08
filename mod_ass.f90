module mod_ass
  use mod_parameter


  implicit none

contains

  subroutine ass(age, B, AfterTaxIncome, c, laborincome, A, ss, reduc, nextass)
  ! Finds next period's assets, while checking earnings test (ret30.cpp: line 243-287)
  ! Arguments:
  !    intent(in)
  !       apply: vector of integer(1).
  !       AfterTaxIncome: vector of real(8).
  !       c: vector of real(8). consumption
  !       laborincome: vector of real(8). laborincome = working hours * wage
  !       assets: vector of real(8). current asset
  !    intent(out)
  !       reduc: vector of integer(8). how much benefits are reduced
  !       ss: vector of real(8). Social Security Benefit
  !       nextass: vector of real(8). next period asset

    implicit none
    integer(8), intent(in) :: age, B
    real(8), intent(in) :: AfterTaxIncome, c, laborincome, A
    real(8), intent(in) :: ss
    real(8), intent(out) :: reduc
    real(8), intent(out) :: nextass
    real(8) :: ssearntest, redss

    if (age > etstage-1) then
      taxfrac = 0.0_8
      earnlev = 9999999
    elseif (age > eret -1 .and. age <etstage .and. etest==1_8) then
      taxfrac =0.5_8
      earnlev = 6000
    else
      taxfrac =0.0_8
      earnlev = 9999999
    endif
    ! didn't apply for benefits

    if (B == 0) then
       nextass = A + AfterTaxIncome - c
       reduc = 0.0_8

    ! earnings below earnings test threshold levels
    else if (laborincome < earnlev .and. B == 1) then  !See algrs55.src line 195
       nextass = A + ss + AfterTaxIncome - c
       reduc = 0.0_8
       if (nextass > Asmax) then
          nextass = Asmax
       end if

    ! only part of benefits are reduced

    else if ((laborincome - earnlev) * taxfrac < ss .and. B == 1) then   !taxfrac: 0.5 or 0 *see algs55.src line 195
       ssearntest = (laborincome - earnlev) * taxfrac    ! lost benefits
       reduc = ssearntest / ss    ! fraction of the benefits reduced
       redss = ss - ssearntest
       nextass = A + AfterTaxIncome + redss - c    ! ss: post-reduction value of ssbenes
       if (nextass > Asmax) then
          nextass = Asmax
       end if

       if (reduc==0.0 .and. laborincome/=6000.0_8) then
          write(*,*) 'laborincome', laborincome
          write(*,*) 'reduc', reduc
       end if

    ! all benefits are reduced

    else
       nextass = A + AfterTaxIncome - c
       reduc = 1.0_8
       if (nextass > Asmax) then
          nextass = Asmax
       end if

    endif

    return
  end subroutine ass

end module mod_ass
