c Copyright (C) 1996-2023 The Octave Project Developers
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
      integer function i1mach (i)
      integer i, imach(16)
      logical init
      double precision dlamch
      real slamch
      external dlamch, slamch
      save imach, init
      data imach / 5, 6, 0, 6, 32, 4, 2, 31, 2147483647,
     $     2, 0, 0, 0, 0, 0, 0 /
      data init /.false./
      if (.not. init) then
        imach(11) = slamch ('n')
        imach(12) = slamch ('m')
        imach(13) = slamch ('l')
        imach(14) = dlamch ('n')
        imach(15) = dlamch ('m')
        imach(16) = dlamch ('l')
        init = .true.
      endif
      if (i .lt. 1 .or. i .gt. 16) goto 999
      i1mach = imach(i)
      return
  999 write (*, 1999) i
 1999 format (' i1mach - i out of bounds', i10)
      call xstopx (' ')
      i1mach = 0
      end
