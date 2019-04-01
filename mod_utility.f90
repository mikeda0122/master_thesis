module mod_utility
    use mod_parameter
    implicit none

  contains

    function U(c, h, particip, badheal, nonsep) result(utils)
    ! Utility function (ret30.cpp: line 189-214)
    ! Arguments:
    !   intent(in)
    !       n: integer. length of input vectors c and h.
    !       c: vector of real(8). data of consumption.
    !       h: vector of real(8). data of working hours.
    !       particip: vector of integer(1). data of dummy variables indicating
    !                 whether individual participating in social security.
    !       badheal: vector of integer(1). data of dummy variables indicating
    !                whether individual is in bad health.
    !   intent(out)
    !       utils: vector of real(8). calculated utilities.

        implicit none
        real(8), intent(in) :: c
        real(8), intent(in) :: h
        integer(1), intent(in) :: particip
        real(8), intent(in) :: badheal
        integer(1) :: nonsep
        real(8), intent(out) :: utils

        real(8) :: leisure
        real(8) :: within

        
        if (nonsep == 1_1) then
           leisure = p_leispref - h - ((p_fixcost*particip) + (p_leisprefbad*badheal))
           within = (c**p_gamh) * (leisure**(1-p_gamh))
           if (p_gamc == 1.0_8) then
              utils = p_conspref * log(within)
           else
              utils = p_conspref * (within**(1-p_gamc)) * p_onemgamc
           end if
        else if (nonsep == 0_1) then
           within = c**(1-p_gamc)*p_onemgamc
           leisure = 5280-h-((p_fixcost*particip) + (p_leisprefbad*badheal))
           leisure = leisure**(1-p_gamh)*p_onemgamh
           utils = p_conspref*(within + (p_leispref*leisure))
        end if
        
    end function U

    function beq(assets, nonsep) result(val)
    ! Bequest function (ret30.cpp: line 220-239)
    ! Arguments:
    !   intent(in)
    !       assets: vector of real(8). assets held by each individual upon death.
    !   intent(out)
    !       val: vector of real(8). calculated bequest utilities.

        implicit none
        real(8), intent(in) :: assets
        integer(1) :: nonsep
        real(8), intent(out) :: val


        if (nonsep == 1_1) then
            if (assets < -(p_beqk - 1.0_8)) then
                val = p_onemgamc + (assets + p_beqk)
            else
                if (p_gamc == 1.0_8) then
                    val = log(assets + p_beqk)
                else
                    val = ((assets + p_beqk) ** ((1 - p_gamc)*p_gamh)) * p_onemgamc
                end if
            end if
        else if (nonsep == 0_1) then
            val = (assets + p_beqk) ** (1 - p_gamc) * p_onemgamc
        end if

        val = val * p_bequest * p_conspref

    end function beq

end module mod_utility
