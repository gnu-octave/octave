      subroutine xzdotc(n,zx,incx,zy,incy,retval)
      double complex zdotc, zx(*), zy(*), retval
      integer n, incx, incy
      external zdotc
      retval = zdotc (n, dx, incx, dy, incy)
      return
      end
