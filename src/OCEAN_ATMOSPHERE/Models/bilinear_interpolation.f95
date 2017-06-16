! This code is taken from "Numerical Recipes in Fortran 90"
! 3.6 Interpolation in Two or More Dimensions
! http://www.nrbook.com/a/bookfpdf.html
!
module bilinear_interpolation
    contains
    ! v_t : input array
    ! x1a_t : array of coords of points in the x direction
    ! x2a_t : array of coords of points in the y direction
    ! v_r : interpolated array
    ! x1a_r : array of coords of points in the x direction
    ! x2a_r : array of coords of points in the y direction
    subroutine bilinear_interpolation_field(v_t,x1a_t,x2a_t,v_r, x1a_r, x2a_r)
        use nrtype
        use nrutil, only : iminloc
        implicit none
        
        real(kind=SP), dimension(:,:), intent(In) :: v_t
        real(kind=SP), dimension(:,:), intent(InOut) :: v_r
        real(kind=SP), dimension(:), intent(In) :: x1a_r,x2a_r, x1a_t, x2a_t
        integer :: j,k,j_b_r,j_e_r,k_b_r,k_e_r
        real(kind=SP) :: x1, x2, y
        ! This is looking for the _r range corresponding to _t
        ! Now, if the _t would be larger than the _r, this should cover the whole area, right?
        j_b_r = iminloc(abs(x1a_t(1)-x1a_r))
        j_e_r = iminloc(abs(x1a_t(size(x1a_t))-x1a_r))
        k_b_r = iminloc(abs(x2a_t(1)-x2a_r))
        k_e_r = iminloc(abs(x2a_t(size(x2a_t))-x2a_r))
        do j=j_b_r,j_e_r
            x1=x1a_r(j)
            do k=k_b_r,k_e_r
                x2=x2a_r(k)
                call bilinear_interpolation_point(x1a_t,x2a_t,v_t,y,x1,x2)
                v_r(j,k)=y
            end do
        end do        
end subroutine bilinear_interpolation_field

! y is the interpolated value
subroutine bilinear_interpolation_point(x1a,x2a,ya,y,x1,x2)
    use nrtype
    use nrutil, only : iminloc
    implicit none

    real(kind=SP), dimension(:), intent(In) :: x1a,x2a
    real(kind=SP), dimension(:,:), intent(In) :: ya
    real(kind=SP), intent(In) :: x1,x2
    real(kind=SP), intent(Out) :: y
    integer :: j,k
    real(kind=SP) :: y1,y2,y3,y4,t,u

! ya(j,k) = y(x1a(j), x2a(k)) (3.6.1)
! We want to estimate, by interpolation, the function y at some untabulated point (x1, x2).
! An important concept is that of the grid square in which the point (x1, x2)
! falls, that is, the four tabulated points that surround the desired interior point. For
! convenience, we will number these points from 1 to 4, counterclockwise starting from the lower left. More precisely, if
! x1a(j) <= x1 <= x1a(j+1)
! So I need to compute j from x1a and x1 
    j=iminloc(abs(x1-x1a))
! and k from x2a and x2
    k=iminloc(abs(x2-x2a))
! x2a(k) <= x2 <= x2a(k+1) (3.6.2)
! then

    y1 = ya(j,k)
    y2 = ya(j+1,k)
    y3 = ya(j+1,k+1)
    y4 = ya(j,k+1)

    t = (x1-x1a(j))/(x1a(j+1)-x1a(j))
    u = (x2-x2a(k))/(x2a(k+1)-x2a(k))
! (3.6.4)
! (so that t and u each lie between 0 and 1), and
! y(x1, x2) 
    y = (1-t)*(1-u)*y1 + t*(1-u)*y2 + t*u*y3 + (1-t)*u*y4

end subroutine bilinear_interpolation_point

end module bilinear_interpolation

