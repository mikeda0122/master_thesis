program main

  use mod_parameter
  use mod_makegrids
  use mod_opt_last_gsearch
  use mod_optimization

  implicit none

  real(8) :: Astate(Anum), Wstate(Wnum), AIMEstate(AIMEnum)
  integer(8) :: Ai
  integer(8) :: age
  real(8) :: Copt, Aopt, valopt
  real(8) :: V(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)

  real(8) :: mortality(75)

  integer :: ios
  integer :: j

  open(unit=10, iostat=ios, file='mortality.csv',&
       & action='read', form='formatted', status='old')

  if (ios/=0) then
     write(*,*) 'Failed to open!!'
     stop
  end if
  do j = 1,75,1
     read(10,*) mortality(j)
  end do
  
  close(10)

  call make_A_1(Astate)

  Wstate(1) = 10.0_8
  do j = 2, Wnum
     Wstate(j) = 10.0_8 + Wstate(j-1)
  end do
  AIMEstate(1) = 10.0_8
  do j = 2, AIMEnum
     AIMEstate(j) = 10.0_8 + AIMEstate(j-1)
  end do
  

  p_gamh = 0.593194_8
  p_gamc = 3.51206_8
  p_leispref = 4762.64_8
  p_leisprefbad = 258.115_8
  p_fixcost = 928.774_8
  p_beta = 0.984991_8
  p_bequest = 0.0255898_8
  p_conspref = 10000000.0_8
  p_beqk = 500000.0_8
  p_onemgamc = 1.0_8
  if (p_gamc/=1.0_8) then
     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
  end if
    p_onemgamh = 1.0_8
  if (p_gamh/=1.0_8) then
     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
  end if
  nonsep = 1_1
  tiedwage = 0_8

  open(unit=15, file='valuesopt.csv')
  write(15,"(A)") "age, A, Aindex, Aopt, Copt, value"
  do Ai = 1, Anum
     call opt_last_gsearch(95_8, Astate(Ai), Astate, Wstate, AIMEstate, Copt, Aopt, valopt)
!     V(95-bornage+1, 1_8, 1_8, Ai, 1_8) = valopt
!     write(*,*) 'V', valopt, 'Ai=', Ai
     write(15,'(i2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f18.10)') 95, ',', Astate(Ai), ',', Ai, ',', Aopt, ',', Copt, ',', valopt
  end do

  do age = dieage-1, 30, -1
     do Ai = 1, Anum
        call optimization(age, Astate(Ai), V, Astate,Wstate, AIMEstate, mortality, Copt, Aopt, valopt)
!        V(age-bornage+1, 1_8, 1_8, Ai, 1_8) = valopt
       write(*,*) 'valopt=', valopt
       read*
        write(15,'(i2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f18.10)') age, ',', Astate(Ai), ',', Ai, ',', Aopt, ',', Copt, ',', valopt
     end do
  end do
  close(15)
end program main

