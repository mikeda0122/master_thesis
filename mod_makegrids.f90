module mod_makegrids

    use mod_parameter

    implicit none

contains

    subroutine make_A_1(Astate)
        real(8), intent(inout) :: Astate(:)
        real(8) :: A
        integer(8) :: i

        Astate(1:15) = [(Amin + i*(Aint-Amin)/Anint, i=0,14)]
        Astate(16:Anum) = [(Aint+i*(Amax - Aint)/(Anint-1), i=0,14)]

    end subroutine make_A_1


  end module mod_makegrids
  
