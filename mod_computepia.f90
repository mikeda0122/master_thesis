! COMPUTEPIA: convert AIME to PIA
module mod_computePIA
  use mod_parameter
  implicit none


contains
  function computePIA(AIME) result(PIA)
    implicit none
    real(8), intent(in) :: AIME
    real(8), intent(out) :: PIA


    if (AIME<AIMEbk1) then
      PIA = mpr1 * AIME
    else if (AIME<AIMEbk2) then
      PIA = PIA1 + mpr2 * (AIME - AIMEbk1)
    else
      PIA = PIA2 + mpr3 * (AIME - AIMEbk2)
    end if

 end function computePIA

    function findAIME(PIA) result(AIME)
        implicit none
            real(8), intent(in) :: PIA
            real(8), intent(out) :: AIME

            if ((PIA/mpr1)<(AIMEbk1)) then
                AIME = (PIA/mpr1)

            else if ((AIMEbk1+((PIA-PIA1)/mpr2))<(AIMEbk2)) then
                AIME = (AIMEbk1+(PIA-PIA1)/mpr2)
            else

                AIME=AIMEbk2+(PIA-PIA2)/mpr3
                if (AIME > AIMEmax) then
                    AIME = AIMEmax
                endif
            endif
    end function findAIME

end module mod_computePIA