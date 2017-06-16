! Given arrays x1a(1:m) and x2a(1:n) of independent variables, and an m by n array of
! function values ya(1:m,1:n), tabulated at the grid points dened by x1a and x2a; and
! given values x1 and x2 of the independent variables; this routine returns an interpolated
! function value y, and an accuracy indication dy (based only on the interpolation in the x1
! direction, however).
     SUBROUTINE polin2(x1a,x2a,ya,x1,x2,y,dy)
        USE nrtype; USE nrutil, ONLY : assert_eq
        USE nr, ONLY : polint
        IMPLICIT NONE
        REAL(SP), DIMENSION(:), INTENT(IN) :: x1a,x2a
        REAL(SP), DIMENSION(:,:), INTENT(IN) :: ya
        REAL(SP), INTENT(IN) :: x1,x2
        REAL(SP), INTENT(OUT) :: y,dy
        INTEGER(I4B) :: j,m,ndum
        REAL(SP), DIMENSION(size(x1a)) :: ymtmp
        REAL(SP), DIMENSION(size(x2a)) :: yntmp
        m=assert_eq(size(x1a),size(ya,1),'polin2: m')
        ndum=assert_eq(size(x2a),size(ya,2),'polin2: ndum')
        do j=1,m
            yntmp=ya(j,:)
            call polint(x2a,yntmp,x2,ymtmp(j),dy)
        end do
        call polint(x1a,ymtmp,x1,y,dy)
     END SUBROUTINE polin2
