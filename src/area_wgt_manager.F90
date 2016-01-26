module area_wgt_manager
    use precision_manager
    implicit none

contains
    function create_areawgt_latlon (lat, lon, num_lat, num_lon)
        implicit none
        integer, intent(in) :: num_lon, num_lat
        real(r8), intent(in) :: lon(num_lon), lat(num_lat)
        real(r8) :: create_areawgt_latlon(num_lon,num_lat)

        integer :: i, j, k
        real(r8) :: dlon, dlat
        real(r8) :: dx(num_lat), dy(num_lat)
        real(r8) :: re, rad

        dlon = abs(lon(2)-lon(1))
        dlat = abs(lat(2)-lat(1))
        re   = 6.371d6
        rad  = 4.0d0*atan(1.0d0)/180.0d0

        do j = 1, num_lat
            dx(j)   =   re*cos(lat(j)*rad)*dlon*rad
            dy(j)   =   re*abs(dlat)*rad
        end do
        
        do j = 1, num_lat
        do i = 1, num_lon
            create_areawgt_latlon(i,j) = dx(j)*dy(j) 
        end do
        end do
        return
    end function create_areawgt_latlon
end module area_wgt_manager
