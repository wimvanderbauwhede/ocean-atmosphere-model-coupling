! Test program for bilinear interpolation code
program main
    use bilinear_interpolation

! If this is the ocean model, n_t = nox1 = 85+1; m_t = noy1 = 60+1
! The longitude array is x01_G, the latitude array is y01_G
! starts.f95 contains the lat and lon settings hardcoded
!      xwest  = 123.
!      xeast  = 291.
!      ysouth = -29.5
!      ynorth = 29.5
    
    integer, parameter :: n_t = 86
    real, parameter :: B_lon_t = 123. ! This should be [part of] the Indian Ocean
    real, parameter :: E_lon_t = 291. ! This should be [part of] the Indian Ocean
    real, parameter :: S_lon_t = (E_lon_t-B_lon_t)/n_t

    integer, parameter :: m_t = 61
    real, parameter :: B_lat_t =  -29.5 ! This should be [part of] the Indian Ocean
    real, parameter :: E_lat_t = 29.5 ! This should be [part of] the Indian Ocean
    real, parameter :: S_lat_t = (E_lat_t-B_lat_t)/m_t

    ! If this is the atmosphere model, then n_r = 128+1, m_r = 64+0
    ! But the points are not actually linear!
    ! The LBM uses alat and alon, both are 2D arrays of size 129*64
    ! They contain redundant information, a row of alon and a col of alat are repeated
    ! 

    integer, parameter :: n_r = 129
    real, parameter :: B_lon_r = 0.
    real, parameter :: E_lon_r = 360.
    real, parameter :: S_lon_r = (E_lon_r-B_lon_r)/n_r

    integer, parameter :: m_r = 64
    real, parameter :: B_lat_r = -87.8638
    real, parameter :: E_lat_r = 87.8638
    real, parameter :: S_lat_r = (E_lat_r-B_lat_r)/m_r

    integer :: i
    real, dimension(1:n_t) :: a_lon_t = (/ ( B_lon_t+S_lon_t*(i-1), i=1,n_t ) /)
    real, dimension(1:m_t) :: a_lat_t = (/ ( B_lat_t+S_lat_t*(i-1), i=1,m_t ) /)

    real, dimension(1:n_r) :: a_lon_r = (/ ( B_lon_r+S_lon_r*(i-1), i=1,n_r ) /)
    real, dimension(1:m_r) :: a_lat_r = (/ ( B_lat_r+S_lat_r*(i-1), i=1,m_r ) /)

    real, dimension(1:n_t,1:m_t) :: v_t 
    real, dimension(1:n_r,1:m_r) :: v_r 

    do i = 1, n_t
        do j = 1, m_t
            v_t(i,j) = 1.0 ! real(i*j)/real(n_t*m_t)
        end do
    end do

    do i = 1, n_r
        do j = 1, m_r
            v_r(i,j) = 2.0 ! real(i*j)/real(n_r*m_r)
        end do
    end do

    print *,'# v_t'
    
    do i = 1, n_t
        do j = 1, m_t
            print *, a_lon_t(i), a_lat_t(j), v_t(i,j) 
        end do
        print *, ' '
    end do

    print *,'# v_r'

    do i = 1, n_r
        do j = 1, m_r
            print *, a_lon_r(i), a_lat_r(j), v_r(i,j) 
        end do
        print *, ' '
    end do

    !print *, v_t
    !print *, v_r
    ! Main question is, how do we test this?
    call bilinear_interpolation_field(v_t,a_lon_t,a_lat_t, v_r, a_lon_r, a_lat_r)

    print *,'# v_res_1'
    do i = 1, n_r
        do j = 1, m_r
            print *, a_lon_r(i), a_lat_r(j), v_r(i,j) 
        end do
        print *, ' '
    end do

     do i = 1, n_t
        do j = 1, m_t
            v_t(i,j) = 1.0 ! real(i*j)/real(n_t*m_t)
        end do
    end do

    do i = 1, n_r
        do j = 1, m_r
            v_r(i,j) = 2.0 ! real(i*j)/real(n_r*m_r)
        end do
    end do    

    call bilinear_interpolation_field(v_r,a_lon_r,a_lat_r, v_t,a_lon_t, a_lat_t)

    print *,'# v_res_2'
    do i = 1, n_t
        do j = 1, m_t
            print *, a_lon_t(i), a_lat_t(j), v_t(i,j) 
        end do
        print *, ' '
    end do



end program main

