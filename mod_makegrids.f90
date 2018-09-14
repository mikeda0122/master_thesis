module mod_makegrids

    use mod_parameter

    implicit none

contains

  subroutine make_A(Astate)
    real(8), intent(inout) :: Astate(:)
    real(8) :: A
    integer(8) :: i

    Astate(1:Anint) = [(Amin + (i-1)*((Aint-Amin)/(Anint-1)), i=1,Anint)]
    Astate(Anint+1:Anum) = [Aint+i*((Amax - Aint)/Anint), i=1,Anint)]

  end subroutine make_A


  end module mod_makegrids
  
