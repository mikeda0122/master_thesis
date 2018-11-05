module mod_parameter

    implicit none

    ! pension
    real(8), parameter :: pb0 = (-66.5)*.68
    real(8), parameter :: pb1 = (.3146)*.68
    real(8), parameter :: pb2 = (2.824)*.68
    real(8), parameter :: pbbk = 5000
    real(8), parameter :: penbensstart = 62.0_8

    ! utility
    integer(1) :: nonsep
    real(8) :: p_conspref
    real(8) :: p_gamc
    real(8) :: p_gamh
    real(8) :: p_leispref
    real(8) :: p_leisprefbad
    real(8) :: p_fixcost
    real(8) :: p_L
    real(8) :: p_bequest
    real(8) :: p_onemgamc
    real(8) :: p_onemgamh
    real(8) :: p_beqk

    !tiedwage
    integer(8) :: tiedwage

    real(8), parameter :: ror = 0.04d0
    !Spouse income coefficients
    real(8), parameter :: spicns = 28190.34
    real(8), parameter :: spiage1 = -5967.59
    real(8), parameter :: spiage2 = 255.6247
    real(8), parameter :: spiage3 = -4.82701
    real(8), parameter :: spiage4 = 0.0422403
    real(8), parameter :: spiage5 = -.0001402
    real(8), parameter :: spilnw = 9128.822

    ! Tax brackets and marginal tax rates
    real(8), parameter :: tbk1 = 4440.d0
    real(8), parameter :: tbk2 = 6940.d0
    real(8), parameter :: tbk3 = 27440.d0
    real(8), parameter :: tbk4 = 42440.d0
    real(8), parameter :: tbk5 = 43800.d0
    real(8), parameter :: tbk6 = 84440.d0
    !marginal taxes and ati, with SS
    real(8), parameter :: mtr1 = 0.0715d0
    real(8), parameter :: mtr2 = 0.204d0
    real(8), parameter :: mtr3 = 0.251d0
    real(8), parameter :: mtr4 = 0.3979d0
    real(8), parameter :: mtr5 = 0.4738d0
    real(8), parameter :: mtr6 = 0.4023d0
    real(8), parameter :: mtr7 = 0.4395d0
    ! After-tax income at bracket points:
    !ati(j) = ati(j-1) + [1-mtr(j)]*[tbk(j)-tbk(j-1)]
    real(8), parameter :: ati1 = 4123.d0
    real(8), parameter :: ati2 = 6113.d0
    real(8), parameter :: ati3 = 21468.d0
    real(8), parameter :: ati4 = 30500.d0
    real(8), parameter :: ati5 = 31216.d0
    real(8), parameter :: ati6 = 55506.d0

    ! marginal taxes and ati, with SS taxes reduced 20%
    real(8), parameter :: mtr1a = 0.0611d0
    real(8), parameter :: mtr2a = 0.1936d0
    real(8), parameter :: mtr3a = 0.2936d0
    real(8), parameter :: mtr4a = 0.3875d0
    real(8), parameter :: mtr5a = 0.4635d0
    real(8), parameter :: mtr6a = 0.4023d0
    real(8), parameter :: mtr7a = 0.4395d0
    !  After-tax income at bracket points:
    !  ati(j) = ati(j-1) + [1-mtr(j)]*[tbk(j)-tbk(j-1)]
    real(8), parameter :: ati1a = 4169.d0
    real(8), parameter :: ati2a = 6185.d0
    real(8), parameter :: ati3a = 21753.d0
    real(8), parameter :: ati4a = 30941.d0
    real(8), parameter :: ati5a = 31672.d0
    real(8), parameter :: ati6a = 55962.d0

    real(8), parameter :: AIMEbk1 = 3720.0_8
    real(8), parameter :: AIMEbk2 = 22392.0_8
    real(8), parameter :: mpr1 = 0.9_8
    real(8), parameter :: mpr2 = 0.32_8
    real(8), parameter :: mpr3 = 0.15_8
    real(8), parameter :: PIA1 = 3348.0_8   !  AIMEbk1*mpr1
    real(8), parameter :: PIA2 = 9323.0_8   !  PIA1 + (AIMEbk2-AIMEbk1)*mpr2
    integer, parameter :: AIMEage = 60
    integer, parameter :: startwork = 25
    integer, parameter :: AIMEyrs = 35  !  AIMEage - startwork
    real(8), parameter :: disabincometest = 3600.0_8 ! note that this is now a negative number

    !grids of W
    integer(8) :: Wnum    = 10
    integer(8) :: Wnint    = 5
    real(8) :: Wmax    = 60
    real(8) :: Wmin    = 3
    real(8) :: Wint    = 15

    !grids of H
    integer(8) :: Hnum    = 10
    integer(8) :: Hnint    = 5
    real(8) :: Hmax    = 4000
    real(8) :: Hmin    = 0
    real(8) :: Hint    = 2000

    ! grids of A
    integer(8) :: Anum =   10
    integer(8) :: Anint    = 5  
    real(8) :: Amax    = 600000
    real(8) :: Amin    = 0
    real(8) :: Aint    = 80000

    ! grids of AIME
    integer(8) :: AIMEnum  =  10
    integer(8) :: bdplace =    7
    integer(8) :: AIMEnint = 5
    real(8) :: AIMEmax  = 43800
    real(8) :: AIMEmin  =  2000
    real(8) :: AIMEint = 9000

    ! grids of Bi
    integer(8) :: Bnum = 2


    real(8) :: p_beta
    real(8) :: cfloor = 1.0_8
    integer(8) :: Cnum = 100

    ! age parameters
    real(8) :: L = 5280  ! Time endowment: hours/year
    integer(8) :: TR = 66  ! Number of periods in the life
    integer(8) :: bornage = 30
    integer(8) :: momage = 30
    integer(8) :: momage2 = 69
    integer(8) :: trdat
    integer(8) :: mashft
    integer(8) :: trdat2
    integer(8) :: retage = 70  ! Age by which everyone will exit the labor force
    ! integer(8) :: ageseq(_tr)
    integer(8) :: dieage = 95
    integer(8) :: mappage = 70  ! Age by which everyone will apply for SS benefits
    integer(8) :: penage = 62  ! Age of first pension benefits
    integer(8) :: eret = 62 !early retirement age
    integer(8) :: nret = 65 !normal retirement age
    integer(8) :: etstage = 70 !From this age, SS benefits don't face earnings test
    ! _trdat = momage2-momage+1
    ! mashft = momage-bornage+1
    ! _trdat2 = _trdat+mashft-1
    ! ageseq(_tr) = [(bornage+i, i=1,_tr)]
    ! dieage = bornage+_tr-1


!early application penalty Factors
real(8) :: eretadj64 = 0.93333
real(8) :: eretadj63 = 0.9286
real(8) :: eretadj62 = 0.92308

! Parameters for crediting back SS benefits lost to earnings test
real(8) :: bigcred64 = 0.0714
real(8) :: bigcred63 = 0.07692
real(8) :: bigcred62 = 0.08333
real(8) :: litcred65 = 0.03
real(8) :: litcred66 = 0.02913
real(8) :: litcred67 = 0.0283
real(8) :: litcred68 = 0.02752
real(8) :: litcred69 = 0.026786

!taxfrac !changes applied in mod_ass.f90
real(8) :: taxfrac = 0.5 !Used in mod_ass.f90, effective only if earnings below earnings test threshold levels
real(8) :: earnlev = 6000 !earnlev when taxfrac = 0.5. found in algs55.src line 207.

!taxtype
integer(1) :: taxtype = 0

!integral part
real(8):: wnodes(1:5) = (/ -2.02018287d0, -0.9585724646d0, 0.0d0, 0.9585724646d0, 2.02018287d0 /)
real(8):: wwgts(1:5) = (/ 0.01995624205d0, 0.3936193231d0, 0.9453087204d0, 0.3936193231d0, 0.01995624205d0 /)
real(8):: pi  = 4 * atan (1.0_8)
real(8):: rhow     = 0.977
end module
