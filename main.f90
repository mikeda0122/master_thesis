program main

  use mod_parameter
  use mod_makegrids
  use mod_opt_last_gsearch
  use mod_optimization
  use mod_lifecycle_prof
  
  implicit none

  real(8) :: Astate(Anum), Hstate(Hnum)
  integer(8) :: Ai
  integer(8) :: age
  real(8) :: Copt, Aopt, Hopt, valopt
  real(8) :: V(dieage-bornage+1, Anum), C(dieage-bornage+1, Anum), A(dieage-bornage+1, Anum), H(dieage-bornage+1, Anum)
  real(8) :: prof_A(dieage-bornage+1), prof_C(dieage-bornage+1), prof_H(dieage-bornage+1)

  real(8) :: A0

  real(8) :: mortality(75), logwage(75)

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

  open(unit=11, iostat=ios, file='logwage.csv', action='read', form='formatted', status='old')

  if (ios/=0) then
     write(*,*) 'Failed to open "logwage.csv"!!'
     stop
  end if
  do j=1,60
     read(11,*) logwage(j)
  end do

  close(11)

  call make_A(Astate)
  call make_H(Hstate)

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
  write(15,"(A)") "age, A, Aindex, Aopt, Copt, Hopt, value"
  do Ai = 1, Anum
     call opt_last_gsearch(95_8, Astate(Ai), Astate, Copt, Aopt, valopt)
     V(95_8-bornage+1_8, Ai)=valopt
     C(95-bornage+1_8, Ai)=Copt
     A(95-bornage+1_8, Ai)=Aopt
     H(95-bornage+1_8, Ai)=0.0_8
     write(15,'(i2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') 95, ',', Astate(Ai), ',', Ai, ',', Aopt, ',', Copt, ',', 0.0_8, ',', valopt
  end do

  do age = dieage-1, 30, -1
     do Ai = 1, Anum
        call optimization(age, Astate(Ai), V, Astate, Hstate, mortality, logwage, Copt, Hopt, Aopt, valopt)
        V(age-bornage+1_8, Ai) = valopt
        C(age-bornage+1_8, Ai)=Copt
        A(age-bornage+1_8, Ai)=Aopt
        H(age-bornage+1_8, Ai)=Hopt
        write(15,'(i2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') age, ',', Astate(Ai), ',', Ai, ',', Aopt, ',', Copt, ',', Hopt, ',', valopt
     
     end do
  end do
  close(15)

  A0 = Astate(Anint)
  call trac_lifecycle(A0, C, A, H, Astate, Hstate, prof_C, prof_A, prof_H) 
  
end program main

