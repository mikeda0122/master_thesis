module mod_makegrids

    use mod_parameter

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
    integer(8) :: Cnint

    Cint=Cmin+((Cmax-Cmin)/2)

    Cstate(1:Cnint) = [(Cmin+(i-1)*((Cint-Cmin)/(Cnint-1))), i=1, Cnint]
    Cstate(Cnint+1:Cnum) = [(Cint+i*((Cmax-Cint)/Cnint)), i=1,Cint]

  end subroutine make_C

  subroutine make_H(Hstate)
    real(8), intent(inout) :: Hstate(:)
    integer(8) :: i
    
    Hstate(1:Hnint) = [(Hmin+(i-1)*((Hint-Hmin)/(Hnint-1)),i=1,Hnint]
    Hstate(Hint+1:Hnum) = [(Hint+i*(Hmax-Hint)/Hnint, i=1,Hnint]

  end subroutine make_H
  
end module mod_makegrids
  
