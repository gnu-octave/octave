      double precision function d1mach (i)
      integer i
      logical init
      double precision dmach(5)
      save init, dmach
      data init /.false./
      if (.not. init) then
        call machar (dmach(1), dmach(2), dmach(3), dmach(4), dmach(5))
        init = .true.
      endif
      if (i .lt. 1  .or.  i .gt. 5) goto 999
      d1mach = dmach(i)
      return
  999 write(*,1999) i
 1999 format(' d1mach - i out of bounds', i10)
      call xstopx (' ')
      d1mach = 0
      end
