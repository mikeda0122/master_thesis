program main

  use mod_parameter
  use mod_makegrids
  use mod_optmum0
  use mod_optmum1
  use mod_optmum2
  use mod_optmum3_gsearch
  use mod_simulation
  use mod_health_mortality
  use mod_profwage
  use mod_ageshifter

  implicit none

  real(8) :: Astate(Anum), Hstate(Hnum), Wstate(Wnum), AIMEstate(AIMEnum), Cstate(Cnum)
  real(8) :: Copt, Aopt, Hopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt
  integer(8) :: Bopt
  real(8) :: M

  real(8) :: Vgood(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), Vbad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optC_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optC_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optA_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optA_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optH_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optH_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  integer(8) :: optP_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optP_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  integer(8) :: optB_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optB_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optW_good_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optW_good_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optW_bad_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optW_bad_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optAIME_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optAIME_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optI_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optI_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optpb_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optpb_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)
  real(8) :: optss_good(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum), optss_bad(dieage-bornage+1, AIMEnum, Wnum, Anum, Bnum)

  real(8) :: prof_A_good(dieage-bornage+1), prof_A_bad(dieage-bornage+1)
  real(8) :: prof_C_good(dieage-bornage+1), prof_C_bad(dieage-bornage+1)
  real(8) :: prof_C(dieage-bornage+1)
  real(8) :: prof_H_good(dieage-bornage+1), prof_H_bad(dieage-bornage+1)
  real(8) :: prof_H(dieage-bornage+1)
  real(8) :: prof_H_work_good(dieage-bornage+1), prof_H_work_bad(dieage-bornage+1)
  real(8) :: prof_H_work(dieage-bornage+1)
  real(8) :: prof_B_good(dieage-bornage+1), prof_B_bad(dieage-bornage+1)
  real(8) :: prof_B(dieage-bornage+1)
  real(8) :: prof_W_good(dieage-bornage+1), prof_W_bad(dieage-bornage+1)
  real(8) :: prof_P_good(dieage-bornage+1), prof_P_bad(dieage-bornage+1)
  real(8) :: prof_I(dieage-bornage+1)
  real(8) :: prof_pb(dieage-bornage+1)
  real(8) :: prof_ss(dieage-bornage+1)
  real(8) :: prof_AIME(dieage-bornage+1)

  integer(8) :: Ai, Wi, AIMEi, Bi, Bidx
  integer(8) :: age
  real(8) :: Cinit

  real(8), allocatable :: A_dist(:), M_dist(:), W_dist(:), AIME_dist(:)

  real(8) :: mortality_good(75), mortality_bad(75), good_to_bad(75), bad_to_bad(75)
  real(8) :: hlogwage(75), ulogwage(75)
  real(8) :: hhgr(60), hugr(60), uhgr(60), uugr(60)
  real(8) :: gvec(66), ageshift(66)

  integer(8) :: sim_length
  integer(8) :: j
  integer, dimension(3) :: time_start
  integer, dimension(3) :: time_end
  call itime(time_start)

  call health_mortality(mortality_good, mortality_bad, good_to_bad, bad_to_bad)
  call profwage(hlogwage, ulogwage, hhgr, hugr, uhgr, uugr)
  call computeg(gvec)
  call ageshifter(ageshift)

!  do age=dieage, bornage, -1
!     write(*,*) 'age:', age
!     write(*,*) 'gvec:', gvec(age-bornage+1)
!     write(*,*) 'ageshift:', ageshift(age-bornage+1)
!  end do

  call make_A(Astate)
  !print*, "Asset Grid", Astate
  call make_H(Hstate)
  !print*, "H Grid", Hstate
  call make_W(Wstate)
  !print*, "Wage Grid", Wstate
  call make_AIME(AIMEstate)
  !print*, "AIME Grid", AIMEstate
  call make_C2(Cstate)
!  print*, "C Grid", Cstate
!call make_AIME_2(95_8, AIMEstate)
  !write(*,*) AIMEstate

!  p_gamh = 0.593194_8
!  p_gamc = 3.51206_8
!  !p_gamc = 1.0_8
!  p_leispref = 4762.64_8
!  p_leisprefbad = 258.115_8
!  p_fixcost = 928.774_8
!  p_beta = 0.984991_8
!  p_bequest = 0.0255898_8
!  p_conspref = 1000000.0_8
!  p_beqk = 500000.0_8
!  p_onemgamc = 1.0_8
!  if (p_gamc/=1.0_8) then
!     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
!  end if
!    p_onemgamh = 1.0_8
!  if (p_gamh/=1.0_8) then
!     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
!  end if
!  tiedwage = 0_8

!**********parameters in the "params.out" file**********
!Note p_leispref > 4000+p_fixcost+p_leispref_bad
  p_gamh = 0.54767_8
  p_gamc = 2.8943_8
  p_leispref = 5150.0_8
  !p_leispref = 5280.0_8
  p_leisprefbad = 279.19_8
  p_fixcost = 963.92_8
  p_beta = 0.99298_8
  p_bequest = 0.8892886 !0.88914_8
  p_conspref = 1000000.0_8
  p_beqk = 500000.0_8
  p_onemgamc = 1.0_8
  if (p_gamc/=1.0_8) then
     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
  end if
    p_onemgamh = 1.0_8
  if (p_gamh/=1.0_8) then
     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
  end if

!**********parameter(1) in the French(2005) TABLE2**********
!  p_gamh = 0.578_8
!  p_gamc = 3.34_8
  !p_gamc = 1.0_8
!  p_leispref = 4466.0_8
  !p_leispref = 5280.0_8
!  p_leisprefbad = 318.0_8
  !p_fixcost = 963.92_8
!  p_fixcost = 1313.0_8
!  p_beta = 0.992_8
!  p_bequest = 1.69_8
!  p_conspref = 1000000.0_8
!  p_beqk = 500000.0_8
!  p_onemgamc = 1.0_8
!  if (p_gamc/=1.0_8) then
!     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
!  end if
!  p_onemgamh = 1.0_8
!  if (p_gamh/=1.0_8) then
!     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
!  end if

!**********parameter(3) in the French(2005) TABLE2**********
! p_gamh = 0.533_8
! p_gamc = 3.19_8
 !p_gamc = 1.0_8
! p_leispref = 3900.0_8
!p_leispref = 5280.0_8
! p_leisprefbad = 196.0_8
!p_fixcost = 963.92_8
! p_fixcost = 335.0_8
! p_beta = 0.981_8
! p_bequest = 1.70_8
! p_conspref = 1000000.0_8
! p_beqk = 500000.0_8
! p_onemgamc = 1.0_8
! if (p_gamc/=1.0_8) then
!    p_onemgamc = 1.0_8/(1.0_8-p_gamc)
! end if
!   p_onemgamh = 1.0_8
! if (p_gamh/=1.0_8) then
!    p_onemgamh = 1.0_8/(1.0_8-p_gamh)
! end if

!**********parameter(2) in the French(2005) TABLE2**********
!  p_gamh = 0.602_8
!  p_gamc = 3.78_8
!  p_leispref = 4889.0_8
!  p_leisprefbad = 191.0_8
!  p_fixcost = 1292.0_8
!  p_beta = 0.985_8
!  p_bequest = 2.58_8
!  p_conspref = 1000000.0_8
!  p_beqk = 500000.0_8
!  p_onemgamc = 1.0_8
!  if (p_gamc/=1.0_8) then
!     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
!  end if
!    p_onemgamh = 1.0_8
!  if (p_gamh/=1.0_8) then
!     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
!  end if

!**********parameter(4) in the French(2005) TABLE2**********
!  p_gamh = 0.615_8
!  p_gamc = 7.69_8
!  p_leispref = 3399.0_8
!  p_leisprefbad = 202.0_8
!  p_fixcost = 240.0_8
!  p_beta = 1.04_8
!  p_bequest = 0.037_8
!  p_conspref = 1000000.0_8
!  p_beqk = 500000.0_8
!  p_onemgamc = 1.0_8
!  if (p_gamc/=1.0_8) then
!     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
!  end if
!    p_onemgamh = 1.0_8
!  if (p_gamh/=1.0_8) then
!     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
!  end if

  write(*,*) 'nonsep=', nonsep
  write(*,*) 'taxtype=', taxtype
  write(*,*) 'tiedwage=', tiedwage

  open(unit=15, file='valuesopt.csv')
  write(15,"(A)") "age, M, A, Aindex, AIME, AIMEindex, Wage, Windex, Aopt, Copt, Hopt, Bopt, nextAIMEopt, Iopt, pbopt, ssopt, value"
  do Ai = 1, Anum
     do AIMEi = 1, AIMEnum

        call optmum3_gsearch(95_8, Astate(Ai), AIMEstate(AIMEi), Astate, AIMEstate, Cstate, 0.0_8, Copt, Aopt, Iopt, pbopt, ssopt, valopt)
        do Bi = 1, 2
           Vgood(95_8-bornage+1_8, AIMEi, 1_8, Ai, Bi)=valopt
           optC_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Copt
           optA_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Aopt
           optH_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
           optB_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=1_8
           optW_good_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
           optW_good_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
           optAIME_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=AIMEstate(AIMEi)
           optI_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Iopt
           optpb_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=pbopt
           optss_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=ssopt
        end do
        write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
             95, ',', 0.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',',  0.0_8, ',', 1_8, ',', Aopt, ',', Copt, ',', 0.0_8, ',', 1_8, ',', AIMEstate(AIMEi),',', Iopt,',', pbopt,',', ssopt,',', valopt

        call optmum3_gsearch(95_8, Astate(Ai), AIMEstate(AIMEi), Astate, AIMEstate, Cstate, 1.0_8, Copt, Aopt, Iopt, pbopt, ssopt, valopt)
        do Bi = 1, 2
           Vbad(95_8-bornage+1_8, AIMEi, 1_8, Ai, Bi)=valopt
           optC_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Copt
           optA_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Aopt
           optH_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
           optB_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=1_8
           optW_bad_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
           optW_bad_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
           optAIME_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=AIMEstate(AIMEi)
           optI_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Iopt
           optpb_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=pbopt
           optss_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=ssopt
        end do

        write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
             95, ',', 1.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',',  0.0_8, ',', 1_8, ',', Aopt, ',', Copt, ',', 0.0_8, ',', 1_8, ',', AIMEstate(AIMEi),',', Iopt,',', pbopt,',', ssopt,',', valopt

        do Bi = 1, 2
           do Wi = 2, Wnum
              Vgood(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = Vgood(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optC_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optC_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optA_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optA_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optH_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optH_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optB_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optB_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optW_good_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_good_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optW_good_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_good_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optAIME_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optAIME_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optI_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optI_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optpb_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optpb_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optss_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optss_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)

              Vbad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = Vbad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optC_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optC_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optA_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optA_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optH_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optH_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optB_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optB_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optW_bad_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_bad_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optW_bad_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_bad_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optAIME_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optAIME_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optI_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optI_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optpb_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optpb_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
              optss_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optss_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
           end do
        end do

     end do !AIME loop
  end do !A loop


do age = dieage-1, etstage, -1 !age 94-70
   write(*,*) age
   !call make_AIME_2(age, AIMEstate)
   do Ai = 1, Anum
      do AIMEi = 1, AIMEnum
         call optmum2(age, Astate(Ai), AIMEstate(AIMEi), 0.0_8, Vgood, Vbad, Astate, Wstate, AIMEstate, Cstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Copt, Aopt, Iopt, pbopt, ssopt, valopt)
         do Bi =1, 2
            Vgood(age-bornage+1_8, AIMEi, 1_8, Ai, Bi) = valopt
            optC_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Copt
            optA_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Aopt
            optH_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
            optB_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=1_8
            optW_good_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=3.0_8
            optW_good_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=3.0_8
            optAIME_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=AIMEstate(AIMEi)
            optI_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Iopt
            optpb_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=pbopt
            optss_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=ssopt
         end do
         write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
               age,',', 0.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi), ',', AIMEi,',', 3.0_8,',', 1_8,',', Aopt,',', Copt,',', 0.0_8,',', 1_8,',', AIMEstate(AIMEi),',', Iopt,',', pbopt,',', ssopt, ',', valopt

         call optmum2(age, Astate(Ai), AIMEstate(AIMEi), 1.0_8, Vgood, Vbad, Astate, Wstate, AIMEstate, Cstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Copt, Aopt, Iopt, pbopt, ssopt, valopt)
         do Bi =1, 2
            Vbad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi) = valopt
            optC_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Copt
            optA_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Aopt
            optH_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
            optB_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=1_8
            optW_bad_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=3.0_8
            optW_bad_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=3.0_8
            optAIME_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=AIMEstate(AIMEi)
            optI_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Iopt
            optpb_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=pbopt
            optss_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=ssopt
         end do
         write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
             age,',', 1.0_8, ',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',',  3.0_8,',', 1_8,',', Aopt,',', Copt,',', 0.0_8,',', 1_8,',', AIMEstate(AIMEi),',', Iopt,',', pbopt,',', ssopt,',', valopt

         do Bi = 1, 2
            do Wi = 2, Wnum
               Vgood(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = Vgood(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optC_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optC_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optA_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optA_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optH_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optB_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optW_good_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_good_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optW_good_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_good_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optAIME_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optAIME_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optI_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optI_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optpb_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optpb_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optss_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optss_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)

               Vbad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = Vbad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optC_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optC_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optA_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optA_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optH_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optB_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optW_bad_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_bad_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optW_bad_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_bad_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optAIME_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optAIME_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optI_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optI_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optpb_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optpb_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
               optss_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optss_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
            end do
         end do

      end do ! AIME loop
   end do ! A loop
end do ! age loop

do age = etstage-1, penage, -1 !age 69-62
   !call make_AIME_2(age, AIMEstate)
   write(*,*) age
   do Wi = 1, Wnum
!      if (Wi==7_8) then
!         write(*,*) 'stop1'
!      else if (Wi==8_8) then
!         write(*,*) 'stop2'
!      else if (Wi==9_8) then
!         write(*,*) 'stop3'
!      end if
!      if (Wi==Wnum) then
!         write(*,*) 'stop4'
!      end if
      do Ai = 1, Anum
 !        if (Wi==7_8) then
 !           write(*,*) Ai
 !        end if

         do AIMEi = 1, AIMEnum
            do Bi = 0, 1
               Bidx = Bi+1_8

 !              if (Wi==7_8 .and. Ai==8_8 .and. AIMEi==10_8) then
 !                 write(*,*) 'optmum1 done'
 !                 write(*,*) AIMEi, Bidx, Bi
 !              end if

             Cinit = optC_good(age-bornage+2, AIMEi, Wi, Ai, 1_8)
             call optmum1(age, Astate(Ai), AIMEstate(AIMEi), Bi, Wstate(Wi), 0.0_8, Cinit, Vgood, Vbad, Astate, AIMEstate, Wstate, Cstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                    hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Bopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)

               Vgood(age-bornage+1_8, AIMEi, Wi, Ai, Bidx) = valopt
               optC_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Copt
               optA_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Aopt
               optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Hopt
               optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Bopt
               optW_good_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Wopt_good
               optW_good_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Wopt_bad
               optAIME_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=AIMEopt
               optI_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Iopt
               optpb_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=pbopt
               optss_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=ssopt

               write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                      age,',', 0.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',', Wstate(Wi),',', Wi,',', Aopt,',', Copt,',', Hopt,',', Bopt,',', AIMEopt,',', Iopt,',', pbopt,',', ssopt,',', valopt

               Cinit = optC_bad(age-bornage+2, AIMEi, Wi, Ai, 1_8)
               call optmum1(age, Astate(Ai), AIMEstate(AIMEi), Bi, Wstate(Wi), 1.0_8, Cinit, Vgood, Vbad, Astate, AIMEstate, Wstate, Cstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                    hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Bopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)
               Vbad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx) = valopt
               optC_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Copt
               optA_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Aopt
               optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Hopt
               optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Bopt
               optW_bad_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Wopt_good
               optW_bad_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Wopt_bad
               optAIME_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=AIMEopt
               optI_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Iopt
               optpb_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=pbopt
               optss_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=ssopt
               write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                   age,',', 1.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',', Wstate(Wi),',', Wi,',', Aopt,',', Copt,',', Hopt,',', Bopt,',', AIMEopt,',', Iopt,',', pbopt,',', ssopt,',', valopt

            end do !End B loop
         end do!End AIME loop
      end do!End A loop
   end do !End W loop
end do !End age loop

do age = penage-1, bornage, -1 !age 61-30
   !call make_AIME_2(age, AIMEstate)
   write(*,*) age
   do Wi = 1, Wnum
      do Ai = 1, Anum
         do AIMEi = 1, AIMEnum
            Cinit = optC_good(age-bornage+2, AIMEi, Wi, Ai, 1_8)
            call optmum0(age, Astate(Ai), AIMEstate(AIMEi), Wstate(Wi), Cinit, 0.0_8, Vgood, Vbad, Astate, AIMEstate, Wstate, Cstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                 hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)
            do Bi = 1, 2
               Vgood(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = valopt
               optC_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Copt
               optA_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Aopt
               optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Hopt
               optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0_8
               optW_good_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Wopt_good
               optW_good_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Wopt_bad
               optAIME_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=AIMEopt
               optI_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Iopt
               optpb_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0.0_8
               optss_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0.0_8
            end do
            write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                 age,',', 0.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',', Wstate(Wi),',', Wi,',', Aopt,',', Copt,',', Hopt,',', 0_8,',', AIMEopt,',', Iopt,',', pbopt,',', ssopt,',', valopt

            Cinit = optC_bad(age-bornage+2, AIMEi, Wi, Ai, 1_8)
            call optmum0(age, Astate(Ai), AIMEstate(AIMEi), Wstate(Wi), Cinit, 1.0_8, Vgood, Vbad, Astate, AIMEstate, Wstate, Cstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                 hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)
            do Bi = 1,2
               Vbad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = valopt
               optC_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Copt
               optA_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Aopt
               optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Hopt
               optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0_8
               optW_bad_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Wopt_good
               optW_bad_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Wopt_bad
               optAIME_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=AIMEopt
               optI_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Iopt
               optpb_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0.0_8
               optss_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0.0_8
            end do
            write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                  age,',', 1.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',', Wstate(Wi),',', Wi,',', Aopt,',', Copt,',', Hopt,',', 0_8,',', AIMEopt,',', Iopt,',', pbopt,',', ssopt,',', valopt
         end do !AIME loop
      end do ! A loop
   end do !W loop
end do !age loop

  close(15)

  !Participation choice <<< This should be incorporated into optimization routines. (Masato)
  do age = bornage, dieage
     do AIMEi = 1, AIMEnum
        do Wi = 1, Wnum
           do Ai = 1, Anum
              do Bi = 1, 2
                 if (optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==0.0_8) then
                    optP_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 0_8
                 else if (optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)>0) then
                    optP_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 1_8
                 end if
                 if (optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==0.0_8) then
                    optP_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 0_8
                 else if (optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)>0) then
                    optP_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 1_8
                 end if
              end do
           end do
        end do
     end do
  end do


  sim_length = 7000_8
  allocate(A_dist(sim_length))
  allocate(M_dist(sim_length))
  allocate(W_dist(sim_length))
  allocate(AIME_dist(sim_length))

  open(unit=87, file='sim_A_dist.csv')
  do j=1, sim_length
     read(87,*) A_dist(j)
  end do
  close(87)

  open(unit=88, file='sim_M_dist.csv')
  do j=1, sim_length
     read(88,*) M_dist(j)
  end do
  close(88)

  open(unit=89, file='sim_W_dist.csv')
  do j=1, sim_length
     read(89,*) W_dist(j)
  end do
  close(89)

  open(unit=90, file='sim_AIME_dist.csv')
  do j=1, sim_length
     read(90,*) AIME_dist(j)
  end do
  close(90)

  call simulation_mean(A_dist, M_dist, W_dist, AIME_dist, mortality_good, mortality_bad, good_to_bad, bad_to_bad, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
       optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, optP_good, optP_bad, &
       optB_good, optB_bad, optW_good_good, optW_good_bad, optW_bad_good, optW_bad_bad, optAIME_good, optAIME_bad, &
       optI_good, optI_bad, optpb_good, optpb_bad, optss_good, optss_bad, &
       Astate, Wstate, AIMEstate, prof_C_good, prof_C_bad, prof_C, prof_A_good, prof_A_bad, prof_H_good, prof_H_bad, prof_H, prof_H_work_good, &
       prof_H_work_bad, prof_H_work, prof_B_good, prof_B_bad, prof_B, prof_P_good, prof_P_bad, prof_W_good, prof_W_bad, &
       prof_I, prof_pb, prof_ss, prof_AIME)
  call itime(time_end)
 print *, 'Time spent(Min):  ', (time_end(1)*3600.0d0 + time_end(2) * 60.0d0 +time_end(3) - (time_start(1)*3600.0d0 + time_start(2) * 60.0d0 +time_start(3)))/60.0d0

end program main
