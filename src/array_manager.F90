module  array_manager
    use precision_manager
    implicit none
    interface index_a_in_b 
        module procedure index_a_in_b_integer
        module procedure index_a_in_b_float
    end interface 

    real(r8), parameter :: apv_cr   =   -1.2
    real(r8), parameter :: area_cr  =    1.8
    real(r8), parameter :: ratio_cr =    0.7
    type :: atmos_block
        integer :: lons(10000)          !lons(1:num_pts)
        integer :: lats(10000)          !lats(1:num_pts)
        integer :: ins(10000)           !ins(1:num_eve)
        integer :: ine(10000)           !ine(1:num_eve)
        integer :: hrs(10000)           !hrs(1:num_eve)
        real(r8) :: area(10000)         !area(1:num_eve)
        integer  :: ini_case(10000)     !ini_case(1:num_eve) 
        integer  :: pre_case(10000)     !pre_case(1:num_eve)
        integer  :: num_pts
        integer  :: num_eve             !number of regions with (apv_cr, area_cr) in curr_date
        integer  :: max_eve             
        integer  :: curr_date
        logical  :: is_initial
        logical  :: is_daily
    end type atmos_block 
    type :: atmos_block_1pt
        integer  :: lons(10000)        
        integer  :: lats(10000)
        real(r8) :: area
        integer  :: num_pts
    end type atmos_block_1pt

contains
    subroutine  atmos_block_ini(var_state)
        implicit none
        type (atmos_block), intent(inout) :: var_state
        
        var_state%lons  =   0
        var_state%lats  =   0
        var_state%ins   =   0
        var_state%ine   =   0
        var_state%hrs   =   0
        var_state%area  =   0.0d0
        var_state%ini_case  =   0
        var_state%pre_case  =   0
        var_state%num_pts   =   0
        var_state%num_eve   =   0
        var_state%max_eve   =   0
        var_state%curr_date =   0
        var_state%is_initial    =   .true. 
        var_state%is_daily      =   .true. 
    end subroutine atmos_block_ini
    subroutine atmos_block_1pt_ini(var_1pt_state)
        implicit none
        type (atmos_block_1pt), intent(inout) :: var_1pt_state

        var_1pt_state%lons  =   0
        var_1pt_state%lats  =   0
        var_1pt_state%num_pts   =   0
        var_1pt_state%area      =   0.0d0

    end subroutine atmos_block_1pt_ini

    subroutine local_min(var, num_lon, num_lat, lon_inds, lat_inds, num_inds)
        integer, intent(in) :: num_lon, num_lat
        real (r8), intent(in) :: var(num_lon,num_lat)

        integer, intent(out) :: num_inds
        integer, intent(out), allocatable, dimension(:) :: lon_inds, lat_inds
        
        integer :: lon0, lat0, lon_can(4), lat_can(4), can_len
        integer :: i, j, k
        integer :: lon_inds_temp(10000), lat_inds_temp(10000)
        logical :: if_min
        integer :: num_inds_s, num_inds_e
       
        
        num_inds    =   0
        num_inds_s  =   0
        num_inds_e  =   0
        do i = 1, num_lon
        do j = 1, num_lat
            lon0    =   i
            lat0    =   j
            if (lon0 == 1 .and. lat0 == 1) then
                can_len = 3
                lon_can(1:3) = (/num_lon,lon0+1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0+1/)
            else if (lon0 == 1 .and. lat0 == num_lat) then
                can_len = 3
                lon_can(1:3) = (/num_lon,lon0+1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0-1/)
            else if (lon0 == 1 .and. lat0 > 1 .and. lat0 < num_lat) then
                can_len = 4 
                lon_can(1:4) = (/num_lon,lon0+1,lon0,lon0/)
                lat_can(1:4) = (/lat0,lat0,lat0-1,lat0+1/)
            else if (lon0 == num_lon .and. lat0 == 1) then
                can_len = 3
                lon_can(1:3) = (/1,lon0-1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0+1/)
            else if (lon0 == num_lon .and. lat0 == num_lat) then
                can_len = 3 
                lon_can(1:3) = (/lon0-1,1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0-1/)
            else if (lon0 == num_lon .and. lat0 > 1 .and. lat0 < num_lat) then
                can_len = 4 
                lon_can(1:4) = (/lon0-1,1,lon0,lon0/)
                lat_can(1:4) = (/lat0,lat0,lat0-1,lat0+1/)
            else if (lon0 > 1 .and. lon0 < num_lon .and. lat0 == 1) then
                can_len = 3 
                lon_can(1:3) = (/lon0-1,lon0+1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0+1/)
            else if (lon0 > 1 .and. lon0 < num_lon .and. lat0 == num_lat) then
                can_len = 3 
                lon_can(1:3) = (/lon0-1,lon0+1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0-1/)
            else if (lon0 > 1 .and. lon0 < num_lon .and. lat0 > 1 .and. lat0 < num_lat) then
                can_len = 4 
                lon_can(1:4) = (/lon0-1,lon0+1,lon0,lon0/)
                lat_can(1:4) = (/lat0,lat0,lat0-1,lat0+1/)
            end if
            if_min = .true.
            do k = 1, can_len
                if (var(lon0,lat0) > var(lon_can(k),lat_can(k))) then
                    if_min = .false.
                end if
            end do
            if (if_min) then
                num_inds    =   num_inds+1
                lon_inds_temp(num_inds) =   lon0
                lat_inds_temp(num_inds) =   lat0
            end if
        end do
        end do
        
        allocate(lon_inds(num_inds),lat_inds(num_inds))
        lon_inds(1:num_inds)    =   lon_inds_temp(1:num_inds)
        lat_inds(1:num_inds)    =   lat_inds_temp(1:num_inds)
        return
    end subroutine local_min

    function is_2pt_close(x0,y0)
        real(r8), intent(in) :: x0(2)
        real(r8), intent(in) :: y0(2) 
        logical :: is_2pt_close

        real(r8) :: error1, error2, max_error
        error1  =   abs(x0(1)-y0(1))
        error2  =   abs(x0(2)-y0(2))
        max_error   =   max(error1,error2)

        if (max_error .lt. 1.0d-6) then
            is_2pt_close    =   .true.
        else
            is_2pt_close    =   .false.
        end if
        return
    end function is_2pt_close

    function index_a_in_b_integer(a,b,n)
        integer, intent(in) :: a, n
        integer, intent(in) :: b(n)
        integer :: index_a_in_b_integer
        integer :: k
        logical :: if_error

        if_error    =   .true.
        do k = 1, n
            if (a .eq. b(k)) then
                index_a_in_b_integer = k
                if_error = .false.
                return
            end if
        end do
        if (if_error) then
            write(*,*) "Serious Problem: index_a_in_b_integer"
            stop
        end if 
        return
    end function index_a_in_b_integer

    function index_a_in_b_float(a,b,n)
        integer, intent(in) :: n
        real(r8), intent(in) :: a
        real(r8), intent(in) :: b(n)
        integer :: index_a_in_b_float
        integer :: k
        logical :: if_error

        if_error    =   .true.
        do k = 1, n
            if (abs(a-b(k)) .lt. 1.0d-6) then
                index_a_in_b_float = k
                if_error = .false.
                return
            end if
        end do
        if (if_error) then
            write(*,*) "Serious Problem: index_a_in_b_float"
            stop
        end if 
        return
    end function index_a_in_b_float
    
    function is_a_in_b_integer_1d(a,b,n)
        implicit none
        integer, intent(in) :: a, n
        integer, intent(in) :: b(n)
        logical :: is_a_in_b_integer_1d
        integer :: k
        is_a_in_b_integer_1d    =   .false.
        do k = 1, n
            if (a .eq. b(k)) then
                is_a_in_b_integer_1d    =   .true.
                return
            end if
        end do
        return 
    end function is_a_in_b_integer_1d

    function is_a_in_b_real_2d(lon0,lat0,lons,lats,n)
        implicit none
        integer, intent(in) :: n
        real(r8), intent(in) :: lon0,lat0
        real(r8), intent(in) :: lons(n),lats(n)
        logical :: is_a_in_b_real_2d
        integer :: k
    
        real(r8) :: x0(2), y0(2)

        is_a_in_b_real_2d   =   .false.
        x0  =   (/lon0,lat0/)
        do k = 1, n
            y0 = (/lons(k),lats(k)/)
            is_a_in_b_real_2d   =  is_2pt_close(x0,y0) 
            if (is_a_in_b_real_2d) then
                return
            end if
        end do
        return
    end function is_a_in_b_real_2d

    function is_a_in_b_integer_2d(lon0,lat0,lons,lats,n)
        implicit none
        integer, intent(in) :: n
        integer, intent(in) :: lon0,lat0
        integer, intent(in) :: lons(n),lats(n)
        logical :: is_a_in_b_integer_2d
        integer :: k, diff
    

        is_a_in_b_integer_2d   =   .false.
        !print *, lon0, lat0
        !print *, lons
        do k = 1, n
            diff    =   abs(lon0-lons(k))+abs(lat0-lats(k))
            if (diff == 0) then
                is_a_in_b_integer_2d    =   .true.
                return
            end if
        end do
        return
    end function is_a_in_b_integer_2d

    
    function num_of_common_pts(lons1,lats1,lons2,lats2,n1,n2)
        implicit none
        integer, intent(in) :: n1, n2
        integer, intent(in) :: lons1(n1), lats1(n1), lons2(n2), lats2(n2)
        integer :: num_of_common_pts
        integer :: i, j, k
        integer :: lon0, lat0
        logical :: is_in
        
        num_of_common_pts   =   0
        do k = 1, n1
            lon0    =   lons1(k)
            lat0    =   lats1(k)
            is_in   =   is_a_in_b_integer_2d(lon0,lat0,lons2,lats2,n2)
            if (is_in) then
                num_of_common_pts   =   num_of_common_pts+1 
            end if
        end do
        return
    end function num_of_common_pts

    subroutine  compare_two_state(onept_state, curr_state, prev_state)
        implicit none
        type(atmos_block_1pt), intent(in) :: onept_state
        type(atmos_block), intent(inout)    ::  curr_state
        type(atmos_block), intent(in)     :: prev_state
        logical :: is_continue
        integer :: i, j, k, num_com, start1, end1, len1
        integer :: jj, n1, n2
        integer :: dh, num_com_std
        
        jj  =   curr_state%num_eve

        is_continue =   .false.
        if (prev_state%num_eve == 0) then
            is_continue = .false.    
            return
        end if

        if (curr_state%is_daily) then
            dh  =   24
        else
            dh  =   6
        end if

        do k = 1, prev_state%num_eve
            start1  =   prev_state%ins(k)
            end1    =   prev_state%ine(k)
            len1    =   end1-start1+1
            n1      =   len1
            n2      =   onept_state%num_pts 
            num_com = num_of_common_pts(onept_state%lons,onept_state%lats,&
                      prev_state%lons(start1:end1),prev_state%lats(start1:end1),&
                      onept_state%num_pts,len1)
            if (curr_state%is_daily) then
                num_com_std  =   min(n1*0.5,n2*0.5) 
            else
                num_com_std  =   min(n1*0.7,n2*0.7) 
            end if
            !if (num_com >= 0.7*len1) then
            if (num_com >= num_com_std) then
                !print *, "min(n1*0.6,n2*0.6):",min(n1*0.6,n2*0.6)
                is_continue =   .true.
                curr_state%hrs(jj) = prev_state%hrs(k) + dh 
                curr_state%ini_case(jj) = prev_state%ini_case(k)
                curr_state%pre_case(jj) = k
                curr_state%max_eve      = curr_state%max_eve
                return
            end if
        end do
        if (.not. is_continue) then
            curr_state%hrs(jj) =  dh 
            curr_state%ini_case(jj) = curr_state%max_eve+1 
            curr_state%pre_case(jj) = 0 
            curr_state%max_eve      = curr_state%max_eve+1
        end if
        return
    end subroutine compare_two_state

    function is_arr_close(lon1,lat1,n1,lon2,lat2,n2)
        implicit none
        integer, intent(in) :: n1, n2
        real(r8), intent(in) :: lon1(n1), lat1(n1)
        real(r8), intent(in) :: lon2(n2), lat2(n2)
        logical :: is_arr_close

        integer :: n_std, n_com
        integer :: i, j, k
        logical :: if_close
        real(r8) :: x0(2), y0(2)

        n_std   =   min(n1/2,n2/2)
        n_com   =   0

        do j = 1, n1
            x0 = (/lon1(j),lat1(j)/)
        do k = 1, n2
            y0 = (/lon2(k),lat2(k)/)
            if_close    =   is_2pt_close(x0,y0)     
            if (if_close) then
                n_com   = n_com+1
                exit 
            end if
        end do
        end do
        
        if (n_com .ge. n_std) then
            is_arr_close    =   .true. 
        else
            is_arr_close    =   .false.
        end if
        return
    end function is_arr_close

    subroutine onept_region(apv,areawgt,lonind,latind,num_lon,num_lat,onept_state)
        implicit none
        integer, intent(in) :: num_lon, num_lat, lonind, latind
        real(r8), intent(in)    :: areawgt(num_lon,num_lat)
        real(r8), intent(in)    :: apv(num_lon,num_lat)
        type(atmos_block_1pt), intent(inout) :: onept_state

        integer :: lon0, lat0
        integer :: apv_ind(num_lon,num_lat)
        integer :: region1(num_lon,num_lat)
        integer :: dx, dy, num_pt_s, num_pt_e
        integer :: can_len, lon_can(4), lat_can(4)

        integer :: i, j, k


        where (apv .le. apv_cr) 
            apv_ind = 1
        elsewhere 
            apv_ind = 0
        end where

        onept_state%num_pts = 0
        region1 =   0

        if (apv_ind(lonind,latind) .eq. 1) then
            onept_state%num_pts  =   onept_state%num_pts+1
            onept_state%lons(onept_state%num_pts)   =   lonind
            onept_state%lats(onept_state%num_pts)   =   latind
            region1(lonind,latind) = 1
            num_pt_s        =   1
            num_pt_e        =   1
        else
            print *, apv_ind(lonind,latind)
            print *, "error"
            stop
        end if

        dy  =   1
        k   =   0
        do while (dy >= 1 .and. k <= num_pt_e) 
            !print *, num_pt_s, num_pt_e
            k   =   k+1
            lon0    =   onept_state%lons(k) 
            lat0    =   onept_state%lats(k) 
            if (lon0 == 1 .and. lat0 == 1) then
                can_len = 3
                lon_can(1:3) = (/num_lon,lon0+1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0+1/)
            else if (lon0 == 1 .and. lat0 == num_lat) then
                can_len = 3
                lon_can(1:3) = (/num_lon,lon0+1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0-1/)
            else if (lon0 == 1 .and. lat0 > 1 .and. lat0 < num_lat) then
                can_len = 4 
                lon_can(1:4) = (/num_lon,lon0+1,lon0,lon0/)
                lat_can(1:4) = (/lat0,lat0,lat0-1,lat0+1/)
            else if (lon0 == num_lon .and. lat0 == 1) then
                can_len = 3
                lon_can(1:3) = (/1,lon0-1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0+1/)
            else if (lon0 == num_lon .and. lat0 == num_lat) then
                can_len = 3 
                lon_can(1:3) = (/lon0-1,1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0-1/)
            else if (lon0 == num_lon .and. lat0 > 1 .and. lat0 < num_lat) then
                can_len = 4 
                lon_can(1:4) = (/lon0-1,1,lon0,lon0/)
                lat_can(1:4) = (/lat0,lat0,lat0-1,lat0+1/)
            else if (lon0 > 1 .and. lon0 < num_lon .and. lat0 == 1) then
                can_len = 3 
                lon_can(1:3) = (/lon0-1,lon0+1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0+1/)
            else if (lon0 > 1 .and. lon0 < num_lon .and. lat0 == num_lat) then
                can_len = 3 
                lon_can(1:3) = (/lon0-1,lon0+1,lon0/)
                lat_can(1:3) = (/lat0,lat0,lat0-1/)
            else if (lon0 > 1 .and. lon0 < num_lon .and. lat0 > 1 .and. lat0 < num_lat) then
                can_len = 4 
                lon_can(1:4) = (/lon0-1,lon0+1,lon0,lon0/)
                lat_can(1:4) = (/lat0,lat0,lat0-1,lat0+1/)
            end if

            dx = 0
            do j = 1, can_len
                if (apv_ind(lon_can(j),lat_can(j)) == 1 .and. region1(lon_can(j), lat_can(j)) == 0) then
                    onept_state%num_pts  =   onept_state%num_pts + 1
                    onept_state%lons(onept_state%num_pts)   =   lon_can(j)
                    onept_state%lats(onept_state%num_pts)   =   lat_can(j)
                    region1(lon_can(j), lat_can(j)) = 1
                    dx = dx+1
                    dy = dy+1

                end if
            end do
            dy  =   dy - 1
            num_pt_s    =   num_pt_e+1
            num_pt_e    =   onept_state%num_pts
            if (dy == 0) then
                !print*,"check"
                onept_state%area = 0.0
                do i = 1, onept_state%num_pts
                    onept_state%area = onept_state%area + &
                                       areawgt(onept_state%lons(i),onept_state%lats(i))
                end do
                onept_state%area =  onept_state%area*1.0d-12
                return
            end if
        end do
        return
    end subroutine onept_region

    subroutine track_2d_apv(apv,areawgt,lon,lat,num_lon,num_lat, curr_state, prev_state)
        implicit none
        integer, intent(in)  :: num_lon, num_lat  
        real(r8), intent(in) :: lon(num_lon), lat(num_lat)
        real(r8), intent(in) :: apv(num_lon,num_lat)
        real(r8), intent(in) :: areawgt(num_lon,num_lat)
        type(atmos_block), intent(inout) :: curr_state
        type(atmos_block), intent(in) :: prev_state
        
        type(atmos_block_1pt) :: onept_state

        integer :: i, j, k
        integer :: num_inds
        integer, allocatable, dimension(:) :: lon_inds, lat_inds
        integer :: num_pt
        logical :: is_in

        integer :: lon0, lat0
        integer :: start1, end1
        integer :: dh

        if (curr_state%is_daily) then
            dh  =   24
        else
            dh  =   6
        end if

        
        call local_min(apv, num_lon, num_lat, lon_inds, lat_inds, num_inds)
        call atmos_block_1pt_ini(onept_state)
        loop1: do k = 1, num_inds
            lon0    =   lon(lon_inds(k))
            lat0    =   lat(lat_inds(k))
            if (lat0 < 10.0d0 .or. apv(lon_inds(k),lat_inds(k)) > apv_cr) cycle loop1 
            !if (apv(lon_inds(k),lat_inds(k)) > apv_cr) cycle loop1 
            call onept_region(apv,areawgt,lon_inds(k),lat_inds(k),num_lon,num_lat,onept_state)
            if (onept_state%area < area_cr) cycle loop1
            if (curr_state%num_pts > 0) then
                is_in   = is_a_in_b_integer_2d(lon_inds(k),lat_inds(k),&
        curr_state%lons(1:curr_state%num_pts),curr_state%lats(1:curr_state%num_pts),curr_state%num_pts)
                if (is_in) cycle loop1
            end if
            start1  =   curr_state%num_pts+1
            end1    =   curr_state%num_pts+onept_state%num_pts
            curr_state%num_eve  =   curr_state%num_eve+1
            curr_state%num_pts  =   curr_state%num_pts+onept_state%num_pts
            curr_state%ins(curr_state%num_eve) =   start1
            curr_state%ine(curr_state%num_eve) =   end1
            curr_state%lons(start1:end1) = onept_state%lons(1:onept_state%num_pts)
            curr_state%lats(start1:end1) = onept_state%lats(1:onept_state%num_pts)
            curr_state%area(curr_state%num_eve) = onept_state%area
            if (prev_state%num_eve  == 0) then
                curr_state%hrs(curr_state%num_eve) = dh 
                curr_state%ini_case(curr_state%num_eve) =   curr_state%max_eve+1
                curr_state%pre_case(curr_state%num_eve) =   0
                curr_state%max_eve  =   curr_state%max_eve+1
            else
                call  compare_two_state(onept_state, curr_state, prev_state)
            end if
            !print *, onept_state%num_pts, onept_state%area
        end do loop1
            !print *, curr_state%num_pts 
            print*,"area:", curr_state%area(1:curr_state%num_eve)
            print*,"num_eve:",curr_state%num_eve
            print*,"ins:",curr_state%ins(1:curr_state%num_eve)
            print*,"ine:",curr_state%ine(1:curr_state%num_eve)
            print*,"ini_case:",curr_state%ini_case(1:curr_state%num_eve)
            print*,"pre_case:",curr_state%pre_case(1:curr_state%num_eve)
            print*,"hrs:",curr_state%hrs(1:curr_state%num_eve)
        return
    end subroutine track_2d_apv

end module  array_manager

