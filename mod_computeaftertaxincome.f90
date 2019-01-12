module mod_computeAfterTaxIncome

  use mod_parameter
  implicit none
  !real(8) :: spicns, spiage1, spiage2, spiage3, spiage4, spiage5, spilnw, tbk1, tbk2, tbk3, tbk4, tbk5, tbk6,&
  !           mtr1, mtr2, mtr3, mtr4, mtr5, mtr6, mtr7, ati1, ati2, ati3, ati4, ati5, ati6, ror,&
  !          mtr1a, mtr2a, mtr3a, mtr4a, mtr5a, mtr6a, mtr7a, ati1a, ati2a, ati3a, ati4a, ati5a, ati6a
!rate of return given in msm16.gau

contains

  function  computeAfterTaxIncome(laborincome, assets, MTR, wage, penbenefits, taxtype, age) result(AfterTaxIncome)
  ! Arguments:
  !    intent(in)
  !    intent(out)

    implicit none
    real(8), intent(in) :: laborincome
    real(8), intent(in) :: assets
    real(8), intent(inout) :: MTR
    real(8), intent(in) :: wage
    real(8), intent(in) :: penbenefits
    integer(1), intent(in) :: taxtype
    integer(8), intent(in) :: age
    real(8), intent(out) :: AfterTaxIncome

    real(8) :: Y
    real(8) :: spinc

    ! Find spousal income
    spinc = .7202*(spicns+spiage1*age + spiage2*age**2 + spiage3*age**3 + spiage4*age**4 + spiage5*age**5 + spilnw*log(wage))

    if (taxtype == 0) then
       if (spinc < 0) then
          spinc = 0 ! spousal income can't be negative
       end if

       Y = ror*assets + laborincome + spinc + penbenefits !ror: rate of return
       
       if (Y < tbk1) then
          MTR = mtr1
          AfterTaxIncome = ((1-mtr1) * Y)

       else if (tbk1 <= Y .and. Y < tbk2) then
          MTR = mtr2
          AfterTaxIncome = ati1 + (1-mtr2)*(Y-tbk1)

       else if (tbk2 <= Y .and. Y < tbk3) then
          MTR = mtr3
          AfterTaxIncome = ati2 + (1-mtr3)*(Y-tbk2)

       else if (tbk3 <= Y .and. Y < tbk4) then
          MTR = mtr4
          AfterTaxIncome = ati3 + (1-mtr4)*(Y-tbk3)

       else if (tbk4 <= Y .and. Y < tbk5) then
          MTR = mtr5
          AfterTaxIncome = ati4 + (1-mtr5)*(Y-tbk4)

       else if (tbk5 <= Y .and. Y < tbk6) then
          MTR = mtr6
          AfterTaxIncome = ati5 + (1-mtr6)*(Y-tbk5)

       else if (Y >= tbk6) then
          MTR = mtr7
          AfterTaxIncome = ati6 + (1-mtr7)*(Y-tbk6)
       else
          write(*,*) 'This is not what you want!! tax'
       end if

    else if (taxtype == 1) then !with SS taxes reduced 20%
       if (spinc < 0) then
          spinc = 0 ! spousal income can't be negative
       end if

       Y = ror*assets + laborincome + spinc + penbenefits !ror: rate of return

       if (Y < tbk1) then
          MTR = mtr1a
          AfterTaxIncome = ((1-mtr1a) * Y)

       else if (tbk1 <= Y .and. Y < tbk2) then
          MTR = mtr2a
          AfterTaxIncome = ati1a + (1-mtr2a)*(Y-tbk1)

       else if (tbk2 <= Y .and. Y < tbk3) then
          MTR = mtr3a
          AfterTaxIncome = ati2a + (1-mtr3a)*(Y-tbk2)

       else if (tbk3 <= Y .and. Y < tbk4) then
          MTR = mtr4a
          AfterTaxIncome = ati3a + (1-mtr4a)*(Y-tbk3)

       else if (tbk4 <= Y .and. Y < tbk5) then
          MTR = mtr5a
          AfterTaxIncome = ati4a + (1-mtr5a)*(Y-tbk4)

       else if (tbk5 <= Y .and. Y < tbk6) then
          MTR = mtr6a
          AfterTaxIncome = ati5a + (1-mtr6a)*(Y-tbk5)

       else if (Y >= tbk6) then
          MTR = mtr7a
          AfterTaxIncome = ati6a + (1-mtr7a)*(Y-tbk6)
       else
          write(*,*) 'This is not what you want!! tax'
       end if
    else
       write(*,*) 'This is not what you want!! taxtype'
    end if


  end function computeAfterTaxIncome

end module  mod_computeAfterTaxIncome
