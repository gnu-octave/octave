c Copyright (C) 1993-2023 The Octave Project Developers
c
c See the file COPYRIGHT.md in the top-level directory of this
c distribution or <https://octave.org/copyright/>.
c
c This file is part of Octave.
c
c Octave is free software: you can redistribute it and/or modify it
c under the terms of the GNU General Public License as published by
c the Free Software Foundation, either version 3 of the License, or
c (at your option) any later version.
c
c Octave is distributed in the hope that it will be useful, but
c WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c
c You should have received a copy of the GNU General Public License
c along with Octave; see the file COPYING.  If not, see
c <https://www.gnu.org/licenses/>.
c
      real function r1mach (i)
      integer i
      logical init
      real rmach(5)
      real slamch
      external slamch
      save init, rmach
      data init /.false./
      if (.not. init) then
        rmach(1) = slamch ('u')
        rmach(2) = slamch ('o')
        rmach(3) = slamch ('e')
        rmach(4) = slamch ('p')
        rmach(5) = log10 (slamch ('b'))
        init = .true.
      endif
      if (i .lt. 1 .or. i .gt. 5) goto 999
      r1mach = rmach(i)
      return
  999 write (*, 1999) i
 1999 format (' r1mach - i out of bounds', i10)
      call xstopx (' ')
      r1mach = 0
      end
