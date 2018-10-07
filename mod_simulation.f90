module mod_simulation

  use mod_parameter
  
  implicit none

contains

  subroutine simulation_mean(A_dist, mortality, optC, optA, Astate, mean_prof_C, mean_prof_A)

    implicit none
        
    real(8), intent(in) :: A_dist(:)
    real(8), intent(in) :: mortality(:)
    real(8), intent(in) :: optC(:,:), optA(:,:)
    real(8), intent(in) :: Astate(:)
    
    real(8), intent(out) :: mean_prof_C(:), mean_prof_A(:)

    real(8) :: prof_C(dieage-bornage+1), prof_A(dieage-bornage+1)
    real(8) :: pop(dieage-bornage+1)
    integer(8) :: death_age
    integer(8) :: i, age, n
    

    n = size(A_dist)

    do age = 1, dieage-bornage+1
       mean_prof_C(age) = 0.0_8
       mean_prof_A(age) = 0.0_8
       pop(age) = 0.0_8
    end do

    open(unit=54, file='simulated_prof_ind.csv')
    write(54, "(A)") "id, age, A, C"
    
    do i = 1, n

       call trac_lifecycle(A_dist(i), i, n, mortality, optC, optA, Astate, death_age, prof_C, prof_A)
       
       mean_prof_C = mean_prof_C + prof_C
       mean_prof_A = mean_prof_A + prof_A

       do age = 1, death_age-bornage+1
          pop(age) = pop(age)+1.0_8
          write(54,'(i4, a, i2, a, f18.5, a, f18.5)') i, ',', age+bornage-1, ',', prof_A(age), ',', prof_C(age)
       end do
       
    end do

    mean_prof_C = mean_prof_C/pop
    mean_prof_A = mean_prof_A/pop

    open(unit=69, file='simulated_prof.csv')
    write(69, "(A)") "age, A, C"

    do age = 1, dieage-bornage+1
       write(69, '(i2, a, f18.5, a, f18.5)') age+bornage-1, ',', mean_prof_A(age), ',', mean_prof_C(age)
    end do

    close(54)
    close(69)

  end subroutine simulation_mean
  
  subroutine trac_lifecycle(A0, id, numind, mortality, optC, optA, Astate, death_age, prof_C, prof_A)

    implicit none

    real(8), intent(in) :: A0
    integer(8), intent(in) :: id, numind
    real(8), intent(in) :: optC(:,:), optA(:,:)
    real(8), intent(in) :: Astate(:)
    real(8), intent(in) :: mortality(:)

    integer(8), intent(out) :: death_age
    real(8), intent(out) :: prof_C(dieage-30+1), prof_A(dieage-30+1)

    real(8) :: death(dieage-bornage+1)
    real(8) :: prvA
    integer(8) :: age, i
    integer(8) :: Aindex

    call death_draw(mortality, id, numind, death)
    
    prvA = A0
    death_age = 30_8

    do age = bornage, dieage
       if (death(age-bornage+1)==1.0_8) then
          prvA = 0.0_8
          prof_C(age-bornage+1) = 0.0_8
          prof_A(age-bornage+1) = 0.0_8
       else if (death(age-bornage+1)==0.0_8) then
                    
          call locate_Aindex(prvA, Astate, Aindex)

          prvA = optA(age-bornage+1, Aindex)
          prof_C(age-bornage+1) = optC(age-bornage+1, Aindex)
          prof_A(age-bornage+1) = prvA

          death_age = death_age+1_8
       else
          write(*,*) 'This is not what you want!'
          read*
       end if
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

  subroutine death_draw(mortality, id, numind, death)

    implicit none

    real(8), intent(in) :: mortality(:)
    integer(8), intent(in) :: id, numind
    real(8), intent(out) :: death(:)

    real(8), allocatable :: rarray(:)
    integer(8) :: i, n, rlength
    integer(8), allocatable :: seed(:)

    rlength = numind*(dieage-bornage+1_8)
    allocate(rarray(rlength))
    
    call random_seed(size=n)
    allocate(seed(n))

    seed = 6273884937_8
    
    call random_seed(put=seed)
    call random_number(rarray)
    
    if (rarray((id-1_8)*66_8+1)<mortality(1)) then
       death(1) = 1.0_8
    else if (rarray((id-1_8)*66_8+1) >= mortality(1)) then
       death(1) = 0.0_8
    else
       write(*,*) 'This is not what you expect!!'
       read*
    end if
    
    do i = 2, dieage-bornage+1
       if (rarray((id-1_8)*66+i)<mortality(i) .or. death(i-1)==1.0_8) then
          death(i) = 1.0_8
       else if (rarray((id-1_8)*66+i)>=mortality(i)) then
          death(i) = 0.0_8
       else
          write(*,*) 'This is not what you expect!!'
          read*
       end if
    end do

    deallocate(rarray)
    
  end subroutine death_draw

!  subroutine random_seed_clock()

!    implicit none

!    integer :: nseed, clock
!    integer, allocatable :: seed(:)

!    integer :: i

!    call random_seed(size=nseed)
!    allocate(seed(nseed))

!    call random_seed(put=seed)

!    deallocate(seed)

!  end subroutine random_seed_clock
  
  
end module mod_simulation



    
