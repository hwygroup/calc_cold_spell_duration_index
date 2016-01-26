load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
begin
    fi  =   addfile("tmin.2m.gauss.1948_2015.nc","r") 
    fi_date =   yyyymmdd_time(1948, 2015, "integer")
    tmin    =   fi->tmin
    tmin&time   =   fi_date

    tmin_ref    =   tmin({19610101:19901231},:,:)
   
    tmin_ref_366    =   tmin({19480101:19481231},:,:)

    lon =   tmin_ref_366&lon
    lat =   tmin_ref_366&lat
    num_lon =   dimsizes(lon)
    num_lat =   dimsizes(lat)


    start_year  =   1961
    end_year    =   1990
    time        =   yyyymmdd_time(start_year, end_year, "integer")
    mod_time    =   mod(time,10000)

    ref_time_o    =   yyyymmdd_time(1947, 1949, "integer")
    ref_time_o!0    =   "time"
    ref_time_o&time =   ref_time_o
    ref_time    =   ref_time_o({19471230:19490102})
    ref_mod_time    =   mod(ref_time,10000)
    print(ref_mod_time)


    do k = 2, dimsizes(ref_mod_time)-3
        print(k)
        ind_date    =   ind(mod_time .eq. ref_mod_time(k-2) .or. \
                            mod_time .eq. ref_mod_time(k-1) .or. \
                            mod_time .eq. ref_mod_time(k)   .or. \
                            mod_time .eq. ref_mod_time(k+1) .or. \
                            mod_time .eq. ref_mod_time(k+2))

        num_dates   =   dimsizes(ind_date)
        num_90p     =   num_dates/10
        ;print(num_90p)
        do j = 0, num_lat-1
        do i = 0, num_lon-1
            t_1d    =   tmin_ref(ind_date,j,i)
            t_1d_sort   =   dim_pqsort(t_1d, 2) 
            tmin_ref_366(k-2,j,i)   =   t_1d(num_90p-1)
            ;print(t_1d)
            ;print("tmin_ref_366: "+tmin_ref_366(k-2,j,i))
            ;exit
            delete(t_1d)
            delete(t_1d_sort)
        end do
        end do
        
        delete(ind_date)
    end do

    system("rm -rf tmin_ref_1961_1990_10p.nc")
    fi_out  =   addfile("tmin_ref_1961_1990_10p.nc","c")
    fi_out->tmin    =   tmin_ref_366


end
