module mod_profwage

  use mod_parameter

  implicit none

contains

 subroutine profwage(ulogwage, hlogwage, hhgr, hugr, uhgr, uugr)

   implicit none

  integer :: age, i
  character(1) :: comma1, comma2, comma3, comma4
  real(8), intent(out) :: ulogwage(65), hlogwage(65)
  real(8), intent(out) :: hhgr(65), hugr(65), uhgr(65), uugr(65)

  integer(8) :: ios
  
  if (tiedwage==0_8) then
     if (select_adj==0_8) then
        !******************************************************************************
        !*****************wage profile for non selection adjusted**********************
        !******************************************************************************
        open(unit = 75, iostat = ios, file ='profwage_log_u_notied_noselect.csv', &
             &action = 'read', form = 'formatted', status ='old') !!Data from excel file (multiplied by 0.72 due to price adjstment)
        !!Use this data for tiedwage==0 & selection==0
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1, 60, 1
           read(75, *) ulogwage(i)
        end do
        do i = 61,65,1
           ulogwage(i) = ulogwage(60)
        end do
        close(75)

        open(unit = 76, iostat = ios, file ='profwage_log_h_notied_noselect.csv', &
             &action = 'read', form = 'formatted', status ='old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1, 60, 1
           read(76, *) hlogwage(i)
        end do
        do i = 61,65,1
           hlogwage(i) = hlogwage(60)
        end do
        !  write(*,*) 'hlogwage', hlogwage
        close(76)

        open(unit = 71, iostat = ios, file = 'wage_growth_uugr_notied_noselect.csv', &
             &action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(71, *) uugr(i)
        end do
        do i = 61,65,1
           uugr(i) = uugr(60)
        end do

        !  write(*,*) 'uugr', uugr
        close(71)

        open(unit = 72, iostat = ios, file = 'wage_growth_uhgr_notied_noselect.csv', &
             &action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(72, *) uhgr(i)
        end do
        do i = 61,65,1
           uhgr(i) = uhgr(60)
        end do
        !  write(*,*) 'uhgr', uhgr
        close(72)

        open(unit = 73, iostat = ios, file = 'wage_growth_hugr_notied_noselect.csv', &
             &action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(73, *) hugr(i)
        end do
        do i = 61,65,1
           hugr(i) = hugr(60)
        end do
        !  write(*,*) 'hugr', hugr
        close(73)
        !_
        open(unit = 74, iostat = ios, file = 'wage_growth_hhgr_notied_noselect.csv', &
             action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(74, *) hhgr(i)
        end do
        do i = 61,65,1
           hhgr(i) = hhgr(60)
        end do
        !  write(*,*) 'hhgr', hhgr
        close(74)

     else if(select_adj==1_8) then
        !**************************************************************************
        !*****************wage profile for selection adjusted**********************
        !**************************************************************************

        open(unit = 75, iostat = ios, file ='profwage_log_u_notied_select.csv', &
             & action = 'read', form = 'formatted', status ='old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1, 60, 1
           read(75, *) ulogwage(i)
        end do
        do i = 61,65,1
           ulogwage(i) = ulogwage(60)
        end do
        close(75)

        open(unit = 76, iostat = ios, file ='profwage_log_h_notied_select.csv', &
             & action = 'read', form = 'formatted', status ='old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1, 60, 1
           read(76, *) hlogwage(i)
        end do
        do i = 61,65,1
           hlogwage(i) = hlogwage(60)
        end do
        !write(*,*) 'hlogwage', hlogwage
        close(76)

        open(unit = 71, iostat = ios, file = 'wage_growth_uugr_notied_select.csv', &
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(71, *) uugr(i)
        end do
        do i = 61,65,1
           uugr(i) = uugr(60)
        end do

        !write(*,*) 'uugr', uugr
        close(71)

        open(unit = 72, iostat = ios, file = 'wage_growth_uhgr_notied_select.csv', & 
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(72, *) uhgr(i)
        end do
        do i = 61,65,1
           uhgr(i) = uhgr(60)
        end do
        !write(*,*) 'uhgr', uhgr
        close(72)

        open(unit = 73, iostat = ios, file = 'wage_growth_hugr_notied_select.csv', &
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(73, *) hugr(i)
        end do
        do i = 61,65,1
           hugr(i) = hugr(60)
        end do
        !write(*,*) 'hugr', hugr
        close(73)

        open(unit = 74, iostat = ios, file = 'wage_growth_hhgr_notied_select.csv', & 
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(74, *) hhgr(i)
        end do
        do i = 61,65,1
           hhgr(i) = hhgr(60)
        end do
        !write(*,*) 'hhgr', hhgr
        close(74)
     else
        write(*,*) 'selection adjsted wage?'
     end if

  else if (tiedwage==1_8) then
     !*****************************************************************
     !*****************wage profile for tied wage**********************
     !*****************************************************************
     if (select_adj==0_8) then
        !******************************************************************************
        !*****************wage profile for non selection adjusted**********************
        !******************************************************************************
        open(unit = 75, iostat = ios, file ='profwage_log_u_tied_noselect.csv', & 
             & action = 'read', form = 'formatted', status ='old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1, 60, 1
           read(75, *) ulogwage(i)
        end do
        do i = 61,65,1
           ulogwage(i) = ulogwage(60)
        end do
        close(75)

        open(unit = 76, iostat = ios, file ='profwage_log_h_tied_noselect.csv', & 
             & action = 'read', form = 'formatted', status ='old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1, 60, 1
           read(76, *) hlogwage(i)
        end do
        do i = 61,65,1
           hlogwage(i) = hlogwage(60)
        end do
        !write(*,*) 'hlogwage', hlogwage
        close(76)

        open(unit = 71, iostat = ios, file = 'wage_growth_uugr_tied_noselect.csv', & 
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(71, *) uugr(i)
        end do
        do i = 61,65,1
           uugr(i) = uugr(60)
        end do

        !write(*,*) 'uugr', uugr
        close(71)

        open(unit = 72, iostat = ios, file = 'wage_growth_uhgr_tied_noselect.csv', &
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(72, *) uhgr(i)
        end do
        do i = 61,65,1
           uhgr(i) = uhgr(60)
        end do
        !write(*,*) 'uhgr', uhgr
        close(72)

        open(unit = 73, iostat = ios, file = 'wage_growth_hugr_tied_noselect.csv', &
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(73, *) hugr(i)
        end do
        do i = 61,65,1
           hugr(i) = hugr(60)
        end do
        !write(*,*) 'hugr', hugr
        close(73)

        open(unit = 74, iostat = ios, file = 'wage_growth_hhgr_tied_noselect.csv', &
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(74, *) hhgr(i)
        end do
        do i = 61,65,1
           hhgr(i) = hhgr(60)
        end do
        !write(*,*) 'hhgr', hhgr
        close(74)
        
     else if(select_adj==1_8) then
        !**************************************************************************
        !*****************wage profile for selection adjusted**********************
        !**************************************************************************

        open(unit = 75, iostat = ios, file ='profwage_log_u_tied_select.csv', &
             & action = 'read', form = 'formatted', status ='old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1, 60, 1
           read(75, *) ulogwage(i)
        end do
        do i = 61,65,1
           ulogwage(i) = ulogwage(60)
        end do
        close(75)

        open(unit = 76, iostat = ios, file ='profwage_log_h_tied_select.csv', &
             &action = 'read', form = 'formatted', status ='old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1, 60, 1
           read(76, *) hlogwage(i)
        end do
        do i = 61,65,1
           hlogwage(i) = hlogwage(60)
        end do
        !write(*,*) 'hlogwage', hlogwage
        close(76)

        open(unit = 71, iostat = ios, file = 'wage_growth_uugr_tied_select.csv', & 
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(71, *) uugr(i)
        end do
        do i = 61,65,1
           uugr(i) = uugr(60)
        end do

        !write(*,*) 'uugr', uugr
        close(71)

        open(unit = 72, iostat = ios, file = 'wage_growth_uhgr_tied_select.csv', & 
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(72, *) uhgr(i)
        end do
        do i = 61,65,1
           uhgr(i) = uhgr(60)
        end do
        !write(*,*) 'uhgr', uhgr
        close(72)

        open(unit = 73, iostat = ios, file = 'wage_growth_hugr_tied_select.csv', &
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(73, *) hugr(i)
        end do
        do i = 61,65,1
           hugr(i) = hugr(60)
        end do
        !write(*,*) 'hugr', hugr
        close(73)

        open(unit = 74, iostat = ios, file = 'wage_growth_hhgr_tied_select.csv', &
             & action = 'read', form = 'formatted', status = 'old')
        if (ios /= 0) then
           write(*,*) 'Failed to open!'
           stop
        end if
        do i = 1,60,1 !!!69?
           read(74, *) hhgr(i)
        end do
        do i = 61,65,1
           hhgr(i) = hhgr(60)
        end do
        !write(*,*) 'hhgr', hhgr
        close(74)
     else
        write(*,*) 'selection adjsted wage?'
     end if     
  else
     write(*,*) 'tied wage is not defined!!'
     read*
  end if

end subroutine profwage
end module mod_profwage
