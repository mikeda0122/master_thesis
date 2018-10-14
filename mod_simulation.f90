module mod_simulation

  use mod_parameter
  
  implicit none

contains

  subroutine simulation_mean(A_dist, M_dist, mortality_good, mortality_bad, good_to_bad, bad_to_bad, optC_good, optC_bad, optA_good,&
       & optA_bad, optH_good, optH_bad, Astate, mean_prof_C_good, mean_prof_C_bad, mean_prof_A_good, mean_prof_A_bad, mean_prof_H_good, mean_prof_H_bad)

    implicit none
        
    real(8), intent(in) :: A_dist(:), M_dist(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: optC_good(:,:), optC_bad(:,:)
    real(8), intent(in) :: optA_good(:,:),  optA_bad(:,:)
    real(8), intent(in) :: optH_good(:,:),  optH_bad(:,:)
    real(8), intent(in) :: Astate(:)
    
    real(8), intent(out) :: mean_prof_C_good(:), mean_prof_C_bad(:)
    real(8), intent(out) :: mean_prof_A_good(:), mean_prof_A_bad(:)
    real(8), intent(out) :: mean_prof_H_good(:), mean_prof_H_bad(:)

    real(8) :: prof_C(dieage-bornage+1), prof_A(dieage-bornage+1), prof_H(dieage-bornage+1)
    real(8) :: pop_good(dieage-bornage+1), pop_bad(dieage-bornage+1)
    real(8) :: health(dieage-bornage+1)
    integer(8) :: death_age
    integer(8) :: i, j, age, n
    
    n = size(A_dist)

    do age = 1, dieage-bornage+1
       mean_prof_C_good(age) = 0.0_8
       mean_prof_C_bad(age) = 0.0_8
       mean_prof_A_good(age) = 0.0_8
       mean_prof_A_bad(age) = 0.0_8
       mean_prof_H_good(age) = 0.0_8
       mean_prof_H_bad(age) = 0.0_8
       pop_good(age) = 0.0_8
       pop_bad(age) = 0.0_8
    end do

    open(unit=54, file='simulated_prof_ind.csv')
    write(54, "(A)") "id, age, M, C, A, H"
    
    do i = 1, n

       call trac_lifecycle(A_dist(i), M_dist(i), i, n, mortality_good, mortality_bad, good_to_bad, bad_to_bad, optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, Astate, death_age, health, prof_C, prof_A, prof_H)

       do age = 1, dieage-bornage+1
          if(health(age)==0.0_8) then             
             mean_prof_C_good(age) = mean_prof_C_good(age) + prof_C(age)
             mean_prof_A_good(age) = mean_prof_A_good(age) + prof_A(age)
             mean_prof_H_good(age) = mean_prof_H_good(age) + prof_H(age)

             pop_good(age) = pop_good(age) + 1
          else if (health(age)==1.0_8) then
             mean_prof_C_bad(age) = mean_prof_C_bad(age) + prof_C(age)
             mean_prof_A_bad(age) = mean_prof_A_bad(age) + prof_A(age)
             mean_prof_H_bad(age) = mean_prof_H_bad(age) + prof_H(age)

             pop_bad(age) = pop_bad(age) + 1             
          end if
          
          write(54,'(i4, a, i2, a, f4.2, a, f18.5, a, f18.5, a, f18.5)') i, ',', age+bornage-1, ',', health(age), ',', prof_C(age), ',',  prof_A(age), ',', prof_H(age)
       end do
       
    end do

    mean_prof_C_good = mean_prof_C_good/pop_good
    mean_prof_C_bad = mean_prof_C_bad/pop_bad
    mean_prof_A_good = mean_prof_A_good/pop_good
    mean_prof_A_bad = mean_prof_A_bad/pop_bad
    mean_prof_H_good = mean_prof_H_good/pop_good
    mean_prof_H_bad = mean_prof_H_bad/pop_bad

    open(unit=69, file='simulated_prof.csv')
    write(69, "(A)") "age, C_good, C_bad, A_good, A_bad, H_good, H_bad"

    do age = 1, dieage-bornage+1
       write(69, '(i2, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5)') age+bornage-1, ',', mean_prof_C_good(age),&
            & ',', mean_prof_C_bad(age), ',', mean_prof_A_good(age), ',', mean_prof_A_bad(age), ',', mean_prof_H_good(age), ',', mean_prof_H_bad(age)
    end do

    close(54)
    close(69)

  end subroutine simulation_mean
  
  subroutine trac_lifecycle(A0, M0, id, numind, mortality_good, mortality_bad, good_to_bad, bad_to_bad, optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, Astate, death_age, health, prof_C, prof_A, prof_H)

    implicit none

    real(8), intent(in) :: A0, M0
    integer(8), intent(in) :: id, numind
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: optC_good(:,:), optC_bad(:,:)
    real(8), intent(in) :: optA_good(:,:), optA_bad(:,:)
    real(8), intent(in) :: optH_good(:,:), optH_bad(:,:)
    real(8), intent(in) :: Astate(:)

    integer(8), intent(out) :: death_age
    real(8), intent(out) :: health(:)
    real(8), intent(out) :: prof_C(:), prof_A(:), prof_H(:)

    real(8) :: death(dieage-bornage+1)
    real(8) :: prvA
    integer(8) :: age, i
    integer(8) :: Aindex

    call health_draw(M0, good_to_bad, bad_to_bad, id, numind, health)    
    call death_draw(health, mortality_good, mortality_bad, id, numind, death)
    
    prvA = A0
    death_age = 30_8

    do age = bornage, dieage
       if (death(age-bornage+1)==1.0_8) then
          prvA = 0.0_8
          prof_C(age-bornage+1) = 0.0_8
          prof_A(age-bornage+1) = 0.0_8
          prof_H(age-bornage+1) = 0.0_8          
       else if (death(age-bornage+1)==0.0_8) then
          
          call locate_Aindex(prvA, Astate, Aindex)
          
          if(health(age-bornage+1)==0.0) then
             
             prvA = optA_good(age-bornage+1, Aindex)

             prof_C(age-bornage+1) = optC_good(age-bornage+1, Aindex)
             prof_A(age-bornage+1) = prvA
             prof_H(age-bornage+1) = optH_good(age-bornage+1, Aindex)
          else if (health(age-bornage+1)==1.0_8) then

             prvA = optA_bad(age-bornage+1, Aindex)

             prof_C(age-bornage+1) = optC_bad(age-bornage+1, Aindex)
             prof_A(age-bornage+1) = prvA
             prof_H(age-bornage+1) = optH_bad(age-bornage+1, Aindex)
          else
             write(*,*) 'Something wrong with trac_lifecycle!!'
             read*
          end if
          
          death_age = death_age+1.0_8
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

  subroutine death_draw(health, mortality_good, mortality_bad, id, numind, death)

    implicit none

    real(8), intent(in) :: health(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:)
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

!    if (health(1)==0.0_8) then       
!       if (rarray((id-1_8)*66_8+1)<mortality_good(1)) then
!          death(1) = 1.0_8
!       else if (rarray((id-1_8)*66_8+1) >= mortality_good(1)) then
!          death(1) = 0.0_8
!       else
!          write(*,*) 'Something is wrong with death_draw!!'
!          read*
!       end if
!    else if (health(1)==1.0_8) then
!       if (rarray((id-1_8)*66_8+1)<mortality_bad(1)) then
!          death(1) = 1.0_8
!       else if (rarray((id-1_8)*66_8+1) >= mortality_bad(1)) then
!          death(1) = 0.0_8
!       else
!          write(*,*) 'Something is wrong with death_draw!!'
!          read*
!       end if
!    else
!       write(*,*) 'Something is wrong with health_draw!!'
!       read*
!    end if

    death(1) = 0.0_8
    
    do i = 2, dieage-bornage+1
       if (health(i)==0.0_8) then
          if (rarray((id-1_8)*66+i)<mortality_good(i) .or. death(i-1)==1.0_8) then
             death(i) = 1.0_8
          else if (rarray((id-1_8)*66+i)>=mortality_good(i)) then
             death(i) = 0.0_8
          else
             write(*,*) 'Something is wrong with death_draw!!'
             read*
          end if
       else if (health(i)==1.0_8) then
          if (rarray((id-1_8)*66+i)<mortality_bad(i) .or. death(i-1)==1.0_8) then
             death(i) = 1.0_8
          else if (rarray((id-1_8)*66+i)>=mortality_bad(i)) then
             death(i) = 0.0_8
          else
             write(*,*) 'Something is wrong with death_draw!!'
             read*
          end if
       else
          write(*,*) 'Something is wrong with health_draw!!'
       end if
    end do

    deallocate(rarray)
    
  end subroutine death_draw

  subroutine health_draw(M0, good_to_bad, bad_to_bad, id, numind, health)

    implicit none

    real(8), intent(in) :: M0
    real(8), intent(in) :: good_to_bad(:), bad_to_bad(:)
    integer(8), intent(in) :: id, numind
    real(8), intent(out) :: health(:)

    real(8), allocatable :: rarray(:)
    integer(8) :: i, n, rlength
    integer(8), allocatable :: seed(:)

    rlength = numind*(dieage-bornage+1_8)
    allocate(rarray(rlength))
    
    call random_seed(size=n)
    allocate(seed(n))

    seed = 325867443345_8
    
    call random_seed(put=seed)
    call random_number(rarray)

    health(1) = M0
    
       do i = 2, dieage-bornage+1
          if (health(i-1)==0.0_8) then
             if (rarray((id-1_8)*66+i)<good_to_bad(i)) then
                health(i) = 1.0_8
             else if (rarray((id-1_8)*66+i)>=good_to_bad(i)) then
                health(i) = 0.0_8
             else
                write(*,*) 'Something is wrong with health_draw!!'
                read*
             end if
          else if (health(i-1)==1.0_8) then
             if (rarray((id-1_8)*66+i)<bad_to_bad(i)) then
                health(i) = 1.0_8
             else if (rarray((id-1_8)*66+i)>=bad_to_bad(i)) then
                health(i) = 0.0_8
             else
                write(*,*) 'Something is wrong with health_draw!!'
                read*
             end if
          else
             write(*,*) 'Something is wrong with health_draw!!'
             read*
          end if
       end do    

    deallocate(rarray)
    
  end subroutine health_draw

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



    
