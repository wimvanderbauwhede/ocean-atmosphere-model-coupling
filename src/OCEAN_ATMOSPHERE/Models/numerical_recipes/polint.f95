! Given arrays xa and ya, each of length n, and given a value x, this routine returns a
! value y, and an error estimate dy. If P(x) is the polynomial of degree N − 1 such that
! P(xai) = yai; i = 1; : : : ;n, then the returned value y = P(x).
      SUBROUTINE polint(xa,ya,x,y,dy)
          USE nrtype; USE nrutil, ONLY : assert_eq,iminloc,nrerror
          IMPLICIT NONE
          REAL(SP), DIMENSION(:), INTENT(IN) :: xa,ya
          REAL(SP), INTENT(IN) :: x
          REAL(SP), INTENT(OUT) :: y,dy
          INTEGER(I4B) :: m,n,ns
          REAL(SP), DIMENSION(size(xa)) :: c,d,den,ho
          n=assert_eq(size(xa),size(ya),'polint')
          c=ya ! because c is modified
          d=ya ! ditto
          ho=xa-x
          ns=iminloc(abs(x-xa))
          y=ya(ns)
          ns=ns-1
          do m=1,n-1
            den(1:n-m)=ho(1:n-m)-ho(1+m:n)
            if (any(den(1:n-m) == 0.0)) &
                  call nrerror('polint: calculation failure')
            den(1:n-m)=(c(2:n-m+1)-d(1:n-m))/den(1:n-m)
            d(1:n-m)=ho(1+m:n)*den(1:n-m)
            c(1:n-m)=ho(1:n-m)*den(1:n-m)
            if (2*ns < n-m) then
                  dy=c(ns+1)
            else
                  dy=d(ns)
                  ns=ns-1
            end if
            y=y+dy
        end do
      END SUBROUTINE polint
