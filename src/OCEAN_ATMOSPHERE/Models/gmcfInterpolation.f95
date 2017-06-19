! The bilinear interpolation code is based on "Numerical Recipes in Fortran 90"
! 3.6 Interpolation in Two or More Dimensions
! http://www.nrbook.com/a/bookfpdf.html
! the files in numerical_recipes are in the public domain: http://numerical.recipes/public-domain.html

module gmcfInterpolation

contains

! A simple helper that assummes that the grid points are equidistant
! grid_* contains d_lon_t,d_lat_t,o_lon_t,o_lat_t : the grid spacings (d_*) and offsets (o_*) for latitude and longitude
    subroutine gmcf2DInterpolation(v_t,sz_t,grid_t,v_r,sz_r,grid_r)
        use bilinear_interpolation
        integer, dimension(2), intent(In) :: sz_t,sz_r
        real, dimension(sz_t(1),sz_t(2)), intent(In) :: v_t
        real, dimension(sz_r(1),sz_r(2)), intent(InOut) :: v_r
        real, dimension(4), intent(In) :: grid_t, grid_r
        real :: d_lon_t,d_lat_t,d_lon_r,d_lat_r
        real :: o_lon_t,o_lat_t,o_lon_r,o_lat_r
        real, dimension(sz_t(1)) :: a_lon_t
        real, dimension(sz_t(2)) :: a_lat_t
        real, dimension(sz_r(1)) :: a_lon_r
        real, dimension(sz_r(2)) :: a_lat_r

        integer :: n_t,m_t,n_r,m_r
        integer :: i

        n_t = sz_t(1)
        m_t = sz_t(2)
        n_r = sz_r(1)
        m_r = sz_r(2)

        call gmcfCreatePhysCoordArrays(grid_t,n_t,m_t,a_lon_t,a_lat_t)
        call gmcfCreatePhysCoordArrays(grid_r,n_r,m_r,a_lon_r,a_lat_r)

!        d_lon_t = grid_t(1)
!        d_lat_t = grid_t(2)
!        o_lon_t = grid_t(3)
!        o_lat_t = grid_t(4)
!
!        d_lon_r = grid_r(1)
!        d_lat_r = grid_r(2)
!        o_lon_r = grid_r(3)
!        o_lat_r = grid_r(4)
!
!        do i = 1,n_t
!            a_lon_t = (i-1)*d_lon_t+o_lon_t
!        end do
!        do i = 1,m_t
!            a_lat_t = (i-1)*d_lat_t+o_lat_t
!        end do
!        do i = 1,n_r
!            a_lon_r = (i-1)*d_lon_r+o_lon_r
!        end do
!        do i = 1,m_r
!            a_lat_r = (i-1)*d_lat_r+o_lat_r
!        end do

        call bilinear_interpolation_field(v_t,a_lon_t,a_lat_t, v_r, a_lon_r, a_lat_r)
    
    end subroutine gmcf2DInterpolation


    subroutine gmcfCreatePhysCoordArrays(grid_t,sz_lon_t,sz_lat_t,a_lon_t,a_lat_t)
        real, dimension(4), intent(In) :: grid_t
        integer, intent(In) :: sz_lon_t, sz_lat_t
        real, dimension(1:sz_lon_t), intent(InOut) :: a_lon_t
        real, dimension(1:sz_lat_t), intent(InOut) :: a_lat_t
        real :: d_lon_t,d_lat_t
        real :: o_lon_t,o_lat_t

        d_lon_t = grid_t(1)
        d_lat_t = grid_t(2)
        o_lon_t = grid_t(3)
        o_lat_t = grid_t(4)

        do i = 1,sz_lon_t
            a_lon_t = (i-1)*d_lon_t+o_lon_t
        end do
        do i = 1,sz_lat_t
            a_lat_t = (i-1)*d_lat_t+o_lat_t
        end do

    end subroutine gmcfCreatePhysCoordArrays

! A point-based time interpolation function
    real function gmcfInterpolateTime(v, v_prev, delta_t, t) result(v_act)
        real, intent(In) :: v, v_prev
        !real :: v_act
        integer, intent(In) :: delta_t, t
#if INTERPOL_SCHEME == 1
        ! linear
        v_act =  ((v - v_prev)/(delta_t - 1) )*t + v_prev
#elif INTERPOL_SCHEME == 2
        ! div-by-2
        v_act =  v+(v_prev-v)/(2**(t+1))
#elif INTERPOL_SCHEME == 3
        if (t<delta_t/2) then
            v_act = (v_prev-v)/2
        else
            v_act = v
        end if
#else
        v_act = v
#endif
    end function gmcfInterpolateTime


end module gmcfInterpolation

