#define INTERPOL_SCHEME 1
! The bilinear interpolation code is based on "Numerical Recipes in Fortran 90"
! 3.6 Interpolation in Two or More Dimensions
! http://www.nrbook.com/a/bookfpdf.html
! the files in numerical_recipes are in the public domain: http://numerical.recipes/public-domain.html

module gmcfInterpolation
implicit none
contains

    ! My very own bilinear interpolation for constant-spaced grids
    ! the calculations use 0 as the starting index; but arrays with (:) start actually at 1 regardless of the original dimensioning
    subroutine gmcf2DInterpolationConstSpacing(v_o,grid_o,v_t,grid_t)
        real, dimension(:,:), intent(In) :: v_o
        real, dimension(:,:), intent(InOut) :: v_t
        real, dimension(6), intent(In) :: grid_t, grid_o
        integer :: n_x_o,n_y_o,n_x_t,n_y_t
        real :: d_x_o,x_0_o, d_y_o,y_0_o
        real :: d_x_t,x_0_t, d_y_t,y_0_t

        integer :: i_c,j_c, i_t, j_t
        real :: ax_c, ay_c

        n_x_o=int(grid_o(1))
        n_y_o=int(grid_o(4))
        n_x_t=int(grid_t(1))
        n_y_t=int(grid_t(4))

        d_x_o = grid_o(2)
        x_0_o = grid_o(3)
        d_y_o = grid_o(5)
        y_0_o = grid_o(6)

        d_x_t = grid_t(2)
        x_0_t = grid_t(3)
        d_y_t = grid_t(5)
        y_0_t = grid_t(6)

        do i_t =0,n_x_t-1
            do j_t = 0,n_y_t-1
                call calc_idx_ratio(i_t,d_x_t,x_0_t,d_x_o,x_0_o,i_c,ax_c)
                call calc_idx_ratio(j_t,d_y_t,y_0_t,d_y_o,y_0_o,j_c,ay_c)
                if (i_c>=0 .and. i_c<n_x_t .and. j_c>=0 .and. j_c<n_y_t) then
                    v_t(i_t+1,j_t+1) = bilin_interpol_point(v_o,i_c+1,ax_c,j_c+1,ay_c)
!                    print *, "Index in range: (",i_c,',',j_c,')'
!                else
!                    print *, "Index out of range: (",i_c,',',j_c,')'
                end if
            end do
        end do
    end subroutine gmcf2DInterpolationConstSpacing

! A simple helper that assummes that the grid points are equidistant
! grid_* contains d_lon_t,d_lat_t,o_lon_t,o_lat_t : the grid spacings (d_*) and offsets (o_*) for latitude and longitude
!   subroutine gmcf2DInterpolationConstSpacing(v_t,sz_t,grid_t,v_r,sz_r,grid_r)
!       use bilinear_interpolation
!       integer, dimension(2), intent(In) :: sz_t,sz_r
!       real, dimension(sz_t(1),sz_t(2)), intent(In) :: v_t
!       real, dimension(sz_r(1),sz_r(2)), intent(InOut) :: v_r
!       real, dimension(4), intent(In) :: grid_t, grid_r
!       real :: d_lon_t,d_lat_t,d_lon_r,d_lat_r
!       real :: o_lon_t,o_lat_t,o_lon_r,o_lat_r
!       real, dimension(sz_t(1)) :: a_lon_t
!       real, dimension(sz_t(2)) :: a_lat_t
!       real, dimension(sz_r(1)) :: a_lon_r
!       real, dimension(sz_r(2)) :: a_lat_r
!       integer :: n_t,m_t,n_r,m_r
!       integer :: i
!       n_t = sz_t(1)
!       m_t = sz_t(2)
!       n_r = sz_r(1)
!       m_r = sz_r(2)
!       call gmcfCreatePhysCoordArrays(grid_t,n_t,m_t,a_lon_t,a_lat_t)
!       call gmcfCreatePhysCoordArrays(grid_r,n_r,m_r,a_lon_r,a_lat_r)
!       call bilinear_interpolation_field(v_t,a_lon_t,a_lat_t, v_r, a_lon_r, a_lat_r)
!
!       call bilinear_interpolation_const_spacing(v_o,grid_o,v_t,grid_t):
!
!   end subroutine gmcf2DInterpolation

! For simplicity I will assume that the indices start at 0, see above
    real function calc_coord(idx,d_x,x_0) result(x_g)
        integer, intent(In) :: idx
        real(kind=4), intent(In) :: d_x,x_0
        x_g = idx*d_x+x_0
    end function calc_coord

    subroutine calc_idx_ratio_coord(x_g,d_x,x_0,i_c,a_c)
        real(kind=4), intent(In) :: x_g, d_x, x_0
        integer, intent(InOut) :: i_c
        real(kind=4), intent(InOut) :: a_c
        real(kind=4) :: i_a
!        print *,'<X',x_g,'-',x_0,'/',d_x,'>'
        i_a = (x_g - x_0) / d_x
        i_c = int( floor(i_a) )
        a_c = i_a - i_c
!        print *,'<I',i_a,';',i_c,',',a_c,'>'
        ! F95       <I -0.750000000     ;           0 , -0.750000000
        ! Python    <I -0.75 ; -1 , 0.25 >
    end subroutine calc_idx_ratio_coord

! For convenience we can combine both functions
    subroutine calc_idx_ratio(i_t,d_x_t,x_0_t,d_x_o,x_0_o,i_c,a_c)
        integer, intent(InOut) :: i_t
        real(kind=4), intent(In) :: x_0_t, d_x_t,x_0_o,d_x_o
        integer, intent(InOut) :: i_c
        real(kind=4), intent(InOut) :: a_c
        real(kind=4) :: x_g
        x_g = calc_coord(i_t,d_x_t,x_0_t)
!        print *,'<G',x_g,'>'
        call calc_idx_ratio_coord(x_g,d_x_o,x_0_o,i_c,a_c)
    end subroutine calc_idx_ratio

! Note that here I assume the indices start at 1
    real function bilin_interpol_point(v,i_c,ax_c,j_c,ay_c) result(v_t)
        real, dimension(:,:), intent(In) :: v
        integer, intent(In) :: i_c, j_c
        real(kind=4), intent(In) :: ax_c, ay_c
        v_t = &
            v(i_c,j_c)*(1-ax_c)*(1-ay_c) + &
            v(i_c+1,j_c)*ax_c*(1-ay_c) + &
            v(i_c,j_c+1)*(1-ax_c)*ay_c + &
            v(i_c+1,j_c+1)*ax_c*ay_c
    end function bilin_interpol_point

    subroutine gmcfCreatePhysCoordArrays(grid_t,sz_lon_t,sz_lat_t,a_lon_t,a_lat_t)
        real, dimension(4), intent(In) :: grid_t
        integer, intent(In) :: sz_lon_t, sz_lat_t
        real, dimension(1:sz_lon_t), intent(InOut) :: a_lon_t
        real, dimension(1:sz_lat_t), intent(InOut) :: a_lat_t
        integer :: i
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
        real(kind=4), intent(In) :: v, v_prev
        !real :: v_act
        integer, intent(In) :: delta_t, t
        real(kind=4) :: delta_t_r,t_r
        delta_t_r=real(delta_t)
        t_r=real(t)
#if INTERPOL_SCHEME == 1
        ! linear
        v_act =  (v - v_prev)*t_r/(delta_t_r - 1.0) + v_prev
#elif INTERPOL_SCHEME == 2
        ! div-by-2, so drops of as 2^-t
        ! The idea is that the first point is v_prev, the last is v
        ! e^-8 = 0.000335, close enough
        v_act =  v + (v_prev-v)*exp(- (t_r*8/delta_t_r ) ) ! (2**(-10*(t/delta_t)))
#elif INTERPOL_SCHEME == 3
        ! stepwise
        if (t<real(delta_t)/2) then
            v_act = (v_prev+v)/2
        else
            v_act = v
        end if
#else
        v_act = v
#endif
    end function gmcfInterpolateTime

    subroutine gmcfExtract2DSubdomain(corner_indices, domain, subdomain)
        integer, dimension(4), intent(In) :: corner_indices
        real(kind=4), dimension(:,:) , intent(In) :: domain
        real(kind=4), dimension(:,:) , intent(InOut) :: subdomain
        subdomain = domain(corner_indices(1):corner_indices(2),corner_indices(3):corner_indices(4))
    end subroutine gmcfExtract2DSubdomain

    subroutine gmcfExtract3DSubdomain(corner_indices, domain, subdomain)
        integer, dimension(6), intent(In) :: corner_indices
        real(kind=4), dimension(:,:,:) , intent(In) :: domain
        real(kind=4), dimension(:,:,:) , intent(InOut) :: subdomain
        subdomain = domain(corner_indices(1):corner_indices(2),corner_indices(3):corner_indices(4),corner_indices(5):corner_indices(6))
    end subroutine gmcfExtract3DSubdomain

    subroutine gmcfInsert2DSubdomain(corner_indices, domain, subdomain)
        integer, dimension(4), intent(In) :: corner_indices
        real(kind=4), dimension(:,:) , intent(InOut) :: domain
        real(kind=4), dimension(:,:) , intent(In) :: subdomain
        domain(corner_indices(1):corner_indices(2),corner_indices(3):corner_indices(4))=subdomain
!        print *,'gmcfInsert2DSubdomain:', sum(subdomain), sum(domain(corner_indices(1):corner_indices(2),corner_indices(3):corner_indices(4)))
!        print *,'gmcfInsert2DSubdomain:', domain(corner_indices(1),corner_indices(3))
    end subroutine gmcfInsert2DSubdomain


    subroutine gmcfInsert3DSubdomain(corner_indices, domain, subdomain)
        integer, dimension(6), intent(In) :: corner_indices
        real(kind=4), dimension(:,:,:) , intent(InOut) :: domain
        real(kind=4), dimension(:,:,:) , intent(In) :: subdomain
        domain(corner_indices(1):corner_indices(2),corner_indices(3):corner_indices(4),corner_indices(5):corner_indices(6))=subdomain
    end subroutine gmcfInsert3DSubdomain

end module gmcfInterpolation

