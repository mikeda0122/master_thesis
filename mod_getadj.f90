module mod_GETADJ

  use mod_parameter
  implicit none

   !real(8) :: pb1, pb2, pbbk
   !integer(8) :: nret, apply, estage


contains

  subroutine getadj(age, B, cumadj2, eretadj, bigcred, cumeretadj, litcred)
    integer(8), intent(in) :: age, B
    real(8), intent(out) :: cumadj2, eretadj, bigcred, cumeretadj, litcred


    if ((age < etstage)) then
      if ((nret >= 65) .and. (age <= 65)) then !for policy experiment
        cumadj2 = eretadj64 ** (nret-65)
      end if
      if (age > 64) then
        eretadj = eretadj64
        cumeretadj = eretadj ** (nret-age)
        bigcred = bigcred64
      end if
      if (age == 64) then
        eretadj = eretadj64
        cumeretadj = eretadj64 * cumadj2
        bigcred = bigcred64
      end if
      if (age == 63) then
        eretadj = eretadj63
        cumeretadj = eretadj64 * eretadj63 * cumadj2
        bigcred = bigcred63
      end if
      if (age == 62) then
        eretadj = eretadj62
        cumeretadj = eretadj64*eretadj63*eretadj62*cumadj2
        bigcred=bigcred62
      end if
    end if

    if ((age < etstage)) then
      if (age < 65) then
        litcred = litcred65
      else if (age == 65) then
        litcred = litcred65
      else if (age == 66) then
        litcred = litcred66
      else if (age == 67) then
        litcred = litcred67
      else if (age == 68) then
        litcred = litcred68
      else if (age == 69) then
        litcred = litcred69
     ! else
     !   litcred = litcred69
      end if
    end if

    return
  end subroutine getadj

end module mod_GETADJ
