module mod_lifecycle_prof

  use mod_parameter
  
  implicit none

contains

  subroutine trac_lifecycle(A0, optC, optA, optH, Astate, Hstate, prof_C, prof_A, prof_H)

    implicit none

    real(8), intent(in) :: A0
    real(8), intent(in) :: optC(:,:), optA(:,:), optH(:,:)
    real(8), intent(in) :: Astate(:), Hstate(:)

    real(8), intent(out) :: prof_C(dieage-30+1), prof_A(dieage-30+1), prof_H(dieage-30+1)

    real(8) :: prvA
    
    integer(8) :: age, i
    integer(8) :: Aindex

    prvA = A0

    open(unit=20, file='decision_prof.csv')
    write(20,"(A)") "age, A, C, H"
    
    do age = 30, dieage

       call locate_Aindex(prvA, Astate, Aindex)

       
       prvA = optA(age-bornage+1, Aindex)
       prof_C(age-bornage+1) = optC(age-bornage+1, Aindex)
       prof_A(age-bornage+1) = prvA
       prof_H(age-bornage+1) = optH(age-bornage+1, Aindex)

       
       write(20, '(i2, a, f12.5, a, f12.5, a, f12.5)') age, ',', prof_A(age-bornage+1),',', prof_C(age-bornage+1), ',', prof_H(age-bornage+1)
    end do

  end subroutine trac_lifecycle

  subroutine locate_Aindex(prvA, Astate, Aindex)

    implicit none

    real(8), intent(in) :: prvA
    real(8), intent(in) :: Astate(:)
    integer(8), intent(out) :: Aindex

    integer(8) :: i
    
    if (prvA < (Astate(1)+Astate(2))/2) then
       Aindex = 1_8
    else if ((Astate(Anum-1)+Astate(Anum))/2 <= prvA) then
       Aindex = Anum
    else if ((Astate(1)+Astate(2))/2 <= prvA .and. prvA < (Astate(Anum-1)+Astate(Anum))/2) then
       do i = 2, Anum-1
          if ((Astate(i-1)+Astate(i))/2 <= prvA .and. prvA < (Astate(i)+Astate(i+1))/2) then
             Aindex = i
             exit
          end if
       end do
    else
       print*, 'This is not what you want!!'
       read*
    end if

  end subroutine locate_Aindex
end module mod_lifecycle_prof


    
