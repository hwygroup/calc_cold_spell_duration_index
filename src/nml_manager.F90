module nml_manager
    implicit none
    integer :: start_year    =   1948
    integer :: end_year      =   2015
    integer :: num_lon       =   192
    integer :: num_lat       =   94
    character(len = 500) :: ref_fi     =    "/nfs7/Data/NCEP-NCAR/daily/tmin/tmin_ref_1961_1990_10p.nc" 
    character(len = 500) :: in_fi      =    "/nfs7/Data/NCEP-NCAR/daily/tmin/tmin.2m.gauss.1948_2015.nc" 
    character(len = 500) :: out_fi     =    "csdi_djf_1948_2014.nc" 
    character(len = 500) :: lon_name   =    "lon"
    character(len = 500) :: lat_name   =    "lat"
    character(len = 500) :: tmin_name  =    "tmin"
    logical :: if_has_leap   =   .true.
    
    character(len = 500) :: namelist_file_name 

    namelist /namelist_for_calc_hindex/ start_year, &
                                        end_year, &
                                     num_lon,    &
                                     num_lat,    &
                                     ref_fi,    &
                                     in_fi,    &
                                     out_fi,     &
                                     lon_name,     &
                                     lat_name,     &
                                     tmin_name,     &
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
