program main_run

  use fgsl
  use mod_gmm
  use mod_gmm_fortran
  use mod_parameter
  use mod_GNU_min
  
  implicit none
  
  real(8) :: min_val
  real(8) :: min_point(7)
  !real(8) :: intval(7) = (/0.615_8, 7.96_8, 3399.0_8, 202.0_8, 240.0_8, 1.04_8, 0.037_8/) !para(4) nonscaled
  !real(fgsl_double) :: intval(7) = (/0.615_8, 7.96_8, 3399.0_8, 202.0_8, 240.0_8, 1.04_8, 0.031734_8/) !para(4) scaled
  !***French's initial values for para(4)******************************************
  !(/0.644163_8, 9.72825_8, 3580.03_8, 296.475_8, 286.139_8, 1.00974_8, 0.0368076_8/)
  !*********************************************************************************
  !0.67206167361224676        16.498844504811307        3476.4541276443719        475.00930223891373       63.564163317211642       0.96028182788239103        0.041099261528070556
  !Scale the parameter
  real(fgsl_double) :: intval(7) = (/6.44163_8, 9.72825_8, 3.58003_8, 2.96475_8, 2.86139_8, 10.0974_8, 3.68076_8/)
  
  !real(8) :: intval(7) = (/0.533_8, 3.19_8, 3900.0_8, 196.0_8, 335.0_8, 0.981_8, 1.7_8/) !para(3) nonscaled

  !real(8) :: intval(7) = (/0.602_8, 3.78_8, 4889.0_8, 191.0_8, 1292.0_8, 0.985_8, 2.58_8/) !para(2) nonscaled
  !real(8) :: intval(7) = (/0.578_8, 3.34_8, 4466.0_8, 318.0_8, 1313.0_8, 0.992_8, 1.69_8/) !para(1) nonscaled
  real(fgsl_double) :: step(7) = (/1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8, 1.0_8/)

!  real(8) :: params(7) = (/0.77673828132009937_8, 9.5918935472321927_8, 2920.8505696517195_8,&
!       & 315.65904124402346_8, 260.17418390956311_8, 1.0472725738546487_8, 0.055371049922975507_8 /)
  real(8) :: params(7) = (/0.615_8, 7.96_8, 3399.0_8, 202.0_8, 240.0_8, 1.04_8, 0.031734_8/) !para(4) scaled
  !real(8) :: params(7) = (/0.62724_8, 12.69022_8, 3438.64107_8, 597.27615_8, 473.10697_8, 0.99597_8, 0.09678_8/) !NAG estimation
  !real(8) :: params(7) = (/0.63597_8, 10.0108_8, 3571.7_8, 300.42_8, 282.29_8, 1.02145_8, 0.034415_8/) !NAG estimation work hour only
  !real(8) :: params(7) = (/0.63597_8, 9.0108_8, 3571.7_8, 300.42_8, 282.29_8, 1.02145_8, 0.034415_8/) !NAG estimation work hour only test
  !real(8) :: params(7) = (/0.61264466586931654_8, 10.616139260090080_8, 3759.1216711752331_8, &
  !     & 319.47257450681557_8, 340.44776342575203_8, 0.97389645467922641_8, 0.032998227238843705_8/) !GNU estimation obj fixed vinv
  !real(8) :: params(7) = (/0.61659_8, 11.1457_8, 3639.7_8, &
  !     & 667.91_8, 300.00_8, 0.99139_8, 0.027476_8/) !NAG estimation obj fixed vinv
  !real(8) :: params(7) = (/0.60588_8, 9.4856_8, 3794.8_8, &
  !     & 414.18_8, 378.56_8, 0.85442_8, 0.04381_8/) !NAG estimation our optimal weight
  !real(8) :: params(7) = (/0.59234_8, 7.53270_8, 3364.1923_8, &
  !     & 100.88084_8, 779.68345_8, 1.04_8, 0.07327_8/) !NAG estimation our optimal weight beta fixed
  !real(8) :: params(7) = (/0.62508_8, 11.66046_8, 3692.21938_8, &
  !     & 524.11445_8, 498.34624_8, 0.92460_8, 0.04741_8/) !NAG estimation obj fixed vinv
  !real(8) :: params(7) = (/0.59736_8, 12.16975_8, 3863.91255_8, &
  !     & 456.48423_8, 545.53936_8, 0.93422_8, 0.01613_8/) !NAG estimation optmum 2019/07/03
  
                                                  

  integer(fgsl_size_t) :: params_dimension
  
  real(8) :: y

  integer(4) :: icount, num, ifault

  call timestamp()

  params_dimension = size(intval)
  
  write(*,*) '****************Output and Jobs****************'
  write(*,*) 'Print simulated moments to a csv file', PrintSimMoments
  write(*,*) 'Job number is', job
  write(*,*) ''
  write(*,*) ''

  write(*,*) '****************Estimation**************************'
  write(*,*) 'This is a set of initial values:'
  write(*,*) intval
  write(*,*) 'Units of change in each step of Estimation procedure:'
  write(*,*) step
  write(*,*) ''
  write(*,*) ''
  
  write(*,*) '****************Model Settings****************'
  write(*,*) 'dimension of parameters', params_dimension
  write(*,*) 'Tax type:', taxtype
  write(*,*) 'Liquidity dummy is', liquid
  write(*,*) 'Tied-wage is', tiedwage
  write(*,*) 'Wage adjustment for selection bias', select_adj
  write(*,*) 'Nonseparable utility function', nonsep
  write(*,*) 'Weight matrix is computed in two step', two_step
  write(*,*) 'Existance of Earnings Test', etest
  write(*,*) 'The age when earnings test ends', etstage
  write(*,*) ''
  write(*,*) ''

  write(*,*) '****************Numbers for computation****************'
  write(*,*) 'The number of A grids is', Asnum
  write(*,*) 'The number of AIME grids is', AIMEnum
  write(*,*) 'The number of W grids is', Wnum
  write(*,*) 'The number of H grids is', Hnum
  write(*,*) 'The number of C grids is', Cnum
  write(*,*) 'The number of obs in the simulation is ', simnum
  write(*,*) 'The number of obs in the data is ', datanum
  write(*,*) 'The number of moments used is ', momnum*6
  write(*,*) ''
  write(*,*) ''

  write(*,*) 'Value should be greater than', vpanish
  write(*,*) 'Asset should be greater than', dpanish
  write(*,*) ''
  write(*,*) ''


  !!*********************************************************************
  !!***********************Start Jobs from here**************************
  !!**********************************************************************
  
  if (job==0_8) then

     y = gmm_fortran(params)

     !write(*,*) 'The value of objective function is', y
  else if(job==1_8) then     
     write(*,*) 'Start Estimation procedure!!'

     open(unit=1, file='estimation_log.csv')
     write(1, '(A)') 'p_gamh , p_gamc , p_leispref , p_leisureprefbad, p_fixcost , p_beta , p_bequest , obj'

     !call nelmin(7, intval, min_point, min_val, 0.001_8, step, 3, 120, icount, num, ifault)
     call GNU_NM_min(intval, step)
     
     close(1)
  end if
  
  call timestamp()

contains
    subroutine timestamp ( )

    !*****************************************************************************80
    !
    !! TIMESTAMP prints the current YMDHMS date as a time stamp.
    !
    !  Example:
    !
    !    31 May 2001   9:45:54.872 AM
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 May 2013
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    None
    !
    implicit none

    character ( len = 8 ) ampm
    integer ( kind = 4 ) d
    integer ( kind = 4 ) h
    integer ( kind = 4 ) m
    integer ( kind = 4 ) mm
    character ( len = 9 ), parameter, dimension(12) :: month = (/ &
         'January  ', 'February ', 'March    ', 'April    ', &
         'May      ', 'June     ', 'July     ', 'August   ', &
         'September', 'October  ', 'November ', 'December ' /)
    integer ( kind = 4 ) n
    integer ( kind = 4 ) s
    integer ( kind = 4 ) values(8)
    integer ( kind = 4 ) y

    call date_and_time ( values = values )

    y = values(1)
    m = values(2)
    d = values(3)
    h = values(5)
    n = values(6)
    s = values(7)
    mm = values(8)

    if ( h < 12 ) then
       ampm = 'AM'
    else if ( h == 12 ) then
       if ( n == 0 .and. s == 0 ) then
          ampm = 'Noon'
       else
          ampm = 'PM'
       end if
    else
       h = h - 12
       if ( h < 12 ) then
          ampm = 'PM'
       else if ( h == 12 ) then
          if ( n == 0 .and. s == 0 ) then
             ampm = 'Midnight'
          else
             ampm = 'AM'
          end if
       end if
    end if

    write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
         d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

    return
  end subroutine timestamp
end program main_run
