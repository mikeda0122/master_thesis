module mod_makegrids

  use mod_parameter
  use mod_pension
  use mod_computePIA

    implicit none

contains

  subroutine make_A(Astate)
    real(8), intent(inout) :: Astate(:)
    real(8) :: A
    integer(8) :: i

    Astate(1:Anint) = [(Amin + (i-1)*((Aint-Amin)/(Anint-1)), i=1,Anint)]
    Astate(Anint+1:Anum) = [(Aint+i*((Amax - Aint)/Anint), i=1,Anint)]

  end subroutine make_A

  subroutine make_C(Cstate, Cmax, Cmin)
    real(8), intent(inout) :: Cstate(:)
    real(8), intent(in) :: Cmax, Cmin

    real(8) :: Cint
    integer(8) :: Cnint, i

    Cint=Cmin+((Cmax-Cmin)/2)

!    Cstate(1:Cnint) = [(Cmin+(i-1)*((Cint-Cmin)/(Cnint-1)), i=1, Cnint)]
!    Cstate(Cnint+1:Cnum) = [(Cint+i*((Cmax-Cint)/Cnint), i=1,Cint)]

    Cstate(1:Cnum) =[((Cmin+(i-1)*((Cmax-Cmin)/(Cnum-1))), i=1, Cnum)]
  end subroutine make_C

  subroutine make_H(Hstate)
    real(8), intent(inout) :: Hstate(:)
    integer(8) :: i

    Hstate(1) = 0.0_8
    Hstate(2:Hnint) = [(Hmin+(i-2)*((Hint-Hmin)/(Hnint-2)),i=2,Hnint)]
    Hstate(Hnint+1:Hnum) = [(Hint+i*(Hmax-Hint)/Hnint, i=2,Hnint)]

    !Hstate(2:Hnum) =[((Hmin+(i-1)*((Hmax-Hmin)/(Hnum-2))), i=1, Hnum-1)]
  end subroutine make_H

  subroutine make_W(Wstate)

    implicit none

    real(8), intent(inout) :: Wstate(:)
    integer(8) :: i

    Wstate(1:Wnint) = [(Wmin+(i-1)*((Wint-Wmin)/(Wnint-1)), i=1,Wnint)]
    Wstate(Wnint+1:Wnum) = [(Wint+i*(Wmax - Wint)/Wnint, i=1, Wnint)]

  end subroutine make_W

  subroutine make_AIME(AIMEstate)

    implicit none

    real(8), intent(inout) :: AIMEstate(:)
    integer(8) :: i

    AIMEstate(1:AIMEnint) = [(AIMEmin+(i-1)*((AIMEint-AIMEmin)/(AIMEnint-1)), i=1,AIMEnint)]
    AIMEstate(AIMEnint+1:AIMEnum) = [(AIMEint+i*(AIMEmax - AIMEint)/AIMEnint, i=1, AIMEnint)]

  end subroutine make_AIME

  subroutine make_AIME_2(age, AIMEstate)
    integer(8), intent(in) :: age
    real(8), intent(inout) :: AIMEstate(:)
    real(8) :: kink_pb(3)
    integer(8) :: i
    !pension benefit starts at 62
    if (age >69) then
       kink_pb(1) = predictpensionbenefits(computePIA(AIMEbk1), 70_8)
       kink_pb(2) = predictpensionbenefits(pbbk, 70_8)
       kink_pb(3) = predictpensionbenefits(computePIA(AIMEbk2), 70_8)
    else
       kink_pb(1) = predictpensionbenefits(computePIA(AIMEbk1), age)
       kink_pb(2) = predictpensionbenefits(pbbk, age)
       kink_pb(3) = predictpensionbenefits(computePIA(AIMEbk2), age)
    end if
    AIMEstate(1) = AIMEmin
    AIMEstate(2) = AIMEbk1
    AIMEstate(3:5) = [(findAIME(pbbk)+i*(AIMEbk2 - findAIME(pbbk))/3, i=0,2)]
    AIMEstate(6:AIMEnum) = [(AIMEbk2+i*(AIMEmax - AIMEbk2)/4, i=0,4)]
  end subroutine make_AIME_2

end module mod_makegrids
  
