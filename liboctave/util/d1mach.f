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
      double precision function d1mach (i)
      integer i
      logical init
      double precision dmach(5)
      double precision dlamch
      external dlamch
      save init, dmach
      data init /.false./
      if (.not. init) then
        dmach(1) = dlamch ('u')
        dmach(2) = dlamch ('o')
        dmach(3) = dlamch ('e')
        dmach(4) = dlamch ('p')
        dmach(5) = log10 (dlamch ('b'))
        init = .true.
      endif
      if (i .lt. 1 .or. i .gt. 5) goto 999
      d1mach = dmach(i)
      return
  999 write (*, 1999) i
 1999 format (' d1mach - i out of bounds', i10)
      call xstopx (' ')
      d1mach = 0
      end
