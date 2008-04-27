      real function r1mach (i)
      integer i
      logical init
      real rmach(5)
      save init, rmach
      data init /.false./
      if (.not. init) then
        call smachar (rmach(1), rmach(2), rmach(3), rmach(4), rmach(5))
        init = .true.
      endif
      if (i .lt. 1  .or.  i .gt. 5) goto 999
      r1mach = rmach(i)
      return
  999 write(*,1999) i
 1999 format(' s1mach - i out of bounds', i10)
      call xstopx (' ')
      r1mach = 0
      end
