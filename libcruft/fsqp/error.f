c
      subroutine error(string,inform,io)
c     implicit real*8 (a-h,o-z)
      integer inform,io
      character*40 string
c
      write(io,9900) string
 9900 format(1x,a40)
      inform=7
      return
      end
