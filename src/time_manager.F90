module  time_manager
    use nml_manager 
    implicit none
    private
    public is_leap_year
    public days_of_years
    public date_yymmdd_years
    public six_hours_of_years
    public date_yymmddhh_years_six_hours
    integer, parameter, dimension(12) :: leap_mon_days  = &
        (/31,29,31,30,31,30,31,31,30,31,30,31/)
    integer, parameter, dimension(12) :: noleap_mon_days  = &
        (/31,28,31,30,31,30,31,31,30,31,30,31/)

contains

    function is_leap_year(year)
        integer, intent(in) :: year
        logical :: is_leap_year
        is_leap_year    =   .false.
        if (.not. if_has_leap) then
            return
        end if
        if (mod(year,4) == 0 .and. mod(year,100) /=0) then
            is_leap_year    =   .true.
        else if (mod(year,400) == 0) then
            is_leap_year    =   .true.
        end if
        return
    end function is_leap_year

    function days_of_years(yr_str,yr_end)
        integer, intent(in) :: yr_str, yr_end
        integer :: days_of_years
        integer :: year
        
        days_of_years   =   0
        do year = yr_str, yr_end
            if (is_leap_year(year)) then
                days_of_years   =   days_of_years+366
            else
                days_of_years   =   days_of_years+365
            end if
        end do
        return
    end function days_of_years

    function six_hours_of_years(yr_str,yr_end)
        integer, intent(in) :: yr_str, yr_end
        integer :: six_hours_of_years
        integer :: year
        
        six_hours_of_years   =   0
        do year = yr_str, yr_end
            if (is_leap_year(year)) then
                six_hours_of_years   =   six_hours_of_years+366*4
            else
                six_hours_of_years   =   six_hours_of_years+365*4
            end if
        end do
        return
    end function six_hours_of_years


    subroutine date_yymmdd_years(yr_str,yr_end,num_days,date)
        integer, intent(in) :: yr_str, yr_end, num_days
        integer :: date(num_days) 
        integer :: year, month, day
        integer :: num_mon_days, counter
       
        counter =  1 
        do year     =   yr_str, yr_end
        do month    =   1, 12
            if (is_leap_year(year)) then
                num_mon_days    =   leap_mon_days(month)
            else
                num_mon_days    =   noleap_mon_days(month)
            end if
        do day      =   1, num_mon_days
            date(counter)    =   year*10000+month*100+day
            counter =   counter+1
        end do
        end do
        end do
        return
    end subroutine date_yymmdd_years

    subroutine date_yymmddhh_years_six_hours(yr_str,yr_end,num_six_hours,date)
        integer, intent(in) :: yr_str, yr_end, num_six_hours
        integer :: date(num_six_hours) 
        integer :: year, month, day, hour
        integer :: num_mon_days, counter
       
        counter =  1 
        do year     =   yr_str, yr_end
        do month    =   1, 12
            if (is_leap_year(year)) then
                num_mon_days    =   leap_mon_days(month)
            else
                num_mon_days    =   noleap_mon_days(month)
            end if
        do day      =   1, num_mon_days
        do hour     =   1, 4
            date(counter)    =   year*1000000+month*10000+day*100+(hour-1)*6
            counter =   counter+1
        end do
        end do
        end do
        end do
        return
    end subroutine date_yymmddhh_years_six_hours
end module time_manager
