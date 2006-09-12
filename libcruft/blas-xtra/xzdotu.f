      subroutine xzdotu (n, zx, incx, zy, incy, retval)
      double complex zdotu, zx(*), zy(*), retval
      integer incx, incy
      retval = zdotu (n, zx, incx, zy, incy)
      return
      end
