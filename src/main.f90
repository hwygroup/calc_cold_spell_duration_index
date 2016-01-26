program main
    use nc_read_write_interface
    use time_manager
    use array_manager
    use area_wgt_manager
    !use nml_manager
    implicit none

    character(len = 500) :: ref_fi_10p  = "/nfs7/Data/NCEP-NCAR/daily/tmin/tmin_ref_1961_1990_10p.nc"
    !character(len = 500) :: ref_fi_90p  = "/nfs7/Data/NCEP-NCAR/daily/tmin/tmin_ref_1961_1990_90p.nc"
    character(len = 500) :: fi  = "/nfs7/Data/NCEP-NCAR/daily/tmin/tmin.2m.gauss.1948_2015.nc" 
    character(len = 500) :: out_fi  =   "csdi_djf_1948_2014.nc"

    integer, parameter :: num_lon   =   192
    integer, parameter :: num_lat   =   94
    integer, parameter :: start_year    =   1948
    integer, parameter :: end_year      =   2015
    integer :: num_days, num_days_1948
    integer, allocatable, dimension(:) :: date_all, date_1948
    real(r8), allocatable, dimension(:,:,:) :: tmin_all, tmin_1948
    integer, allocatable, dimension(:,:,:) :: ind_10p,  ind_10p_longer
    real(r8), allocatable, dimension(:,:,:) :: csdi
    integer :: i, j, k, curr_date, k_1948, ind1, ind2, indlen
    real(r8), allocatable, dimension(:) :: lon, lat
    integer,  allocatable, dimension(:) :: year


    allocate(lon(num_lon),lat(num_lat), year(end_year-start_year))
    do k = start_year, end_year-1
        year(k-start_year+1)    =   k
    end do



    num_days = days_of_years(start_year,end_year)
    num_days_1948   =   days_of_years(1948,1948)
    print *, num_days, num_days_1948 
    allocate(date_all(num_days), date_1948(num_days_1948))

    call date_yymmdd_years(start_year,end_year,num_days,date_all) 
    call date_yymmdd_years(1948,1948,num_days_1948,date_1948) 

    allocate(tmin_all(num_lon,num_lat,num_days))
    allocate(ind_10p(num_lon,num_lat,num_days))
    allocate(ind_10p_longer(num_lon,num_lat,num_days))
    allocate(tmin_1948(num_lon,num_lat,num_days_1948))
    allocate(csdi(num_lon,num_lat,end_year-start_year))
    call nc_read_write_interface_read_var (tmin_all,fi,"tmin",[1,1,1],&
                                            [num_lon,num_lat,num_days],[num_lon,num_lat,num_days],3)
    print *, minval(tmin_all)
    print *, maxval(tmin_all)
    call nc_read_write_interface_read_var (tmin_1948,ref_fi_10p,"tmin",[1,1,1],&
                                            [num_lon,num_lat,num_days_1948],[num_lon,num_lat,num_days_1948],3)
    print *, minval(tmin_1948)
    print *, maxval(tmin_1948)


    call nc_read_write_interface_read_var(lon,fi,"lon",[1],[num_lon],[num_lon],1)
    call nc_read_write_interface_read_var(lat,fi,"lat",[1],[num_lat],[num_lat],1)

    csdi=   0.0d0

    
    ind_10p =   0
    do k = 1, num_days
        print *, k
    do i = 1, num_lon
    do j = 1, num_lat
        curr_date   =   1948*10000+mod(date_all(k),10000)
        k_1948  =   index_a_in_b(curr_date,date_1948,num_days_1948)
        if (tmin_all(i,j,k) <= tmin_1948(i,j,k_1948)) then
            ind_10p(i,j,k)  =   1
        end if
    end do
    end do
    end do
    
    ind_10p_longer  =   0
    do i = 1, num_lon
    do j = 1, num_lat
        do k = 1, num_days-5
            if (minval(ind_10p(i,j,k:k+5)).gt.0.5) then
                ind_10p_longer(i,j,k:k+5) = 1
                print *, k
            end if
        end do
    end do
    end do
    
    csdi    =   0
    do i = 1, num_lon
    do j = 1, num_lat
    do k = 1, end_year-start_year 
        curr_date   =   (1948+k-1)*10000+1130
        ind1    =   index_a_in_b(curr_date,date_all,num_days)
        curr_date   =   (1948+k)*10000+301
        ind2    =   index_a_in_b(curr_date,date_all,num_days)
        csdi(i,j,k) =   sum(ind_10p_longer(i,j,ind1+1:ind2-1))
    end do
    end do
    end do

    print *, minval(csdi), maxval(csdi)



    call nc_read_write_interface_delete_file (out_fi)
    call nc_read_write_interface_create_file (out_fi)
    call nc_read_write_interface_write_dim (year, out_fi, "time", "time", "years since 0001", end_year-start_year)
    call nc_read_write_interface_write_dim (lon,  out_fi, "lon",  "lon",  "degrees_east", num_lon)
    call nc_read_write_interface_write_dim (lat,  out_fi, "lat", "lat","degrees_north",num_lat)
    call nc_read_write_interface_write_var (csdi,out_fi, &
    ["lon ", "lat ", "time"],"csdi", "csdi", "days", 1.0d+20, [num_lon,num_lat,67],3) 





end program main
