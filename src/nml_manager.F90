module nml_manager
    implicit none
    integer :: yr_str_ref    =   1948
    integer :: yr_end_ref    =   2014
    integer :: start_date    =   19480101
    integer :: end_date      =   20141231
    integer :: num_lon       =   192
    integer :: num_lat       =   94
    character(len = 500) :: tmax_fi     = "/nfs7/Data/NCEP-NCAR/daily/tmax/tmax.2m.gauss.1948_2015.nc" 
    character(len = 500) :: out_fi      = "/nfs9/home/hwy/cal_hindex/data_out/NCEP-NCAR/hindex_1948_2014.nc" 
    logical :: if_daily      =   .true.
    logical :: if_has_leap   =   .true.
    
    character(len = 500) :: namelist_file_name 

    namelist /namelist_for_calc_hindex/ yr_str_ref, &
                                     yr_end_ref, &
                                     start_date, &
                                     end_date,   & 
                                     num_lon,    &
                                     num_lat,    &
                                     tmax_fi,    &
                                     out_fi,     &
                                     if_daily,   &
                                     if_has_leap


contains
    subroutine read_nml()
        implicit none
        call get_command_argument (1, namelist_file_name)
        open (10, file = namelist_file_name)
        read(10,nml = namelist_for_calc_hindex)
        close(10)
    end subroutine read_nml
    

end module nml_manager
