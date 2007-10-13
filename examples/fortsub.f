c Copyright (C) 2007 John W. Eaton
c 
c This file is part of Octave.
c 
c Octave is free software; you can redistribute it and/or modify it
c under the terms of the GNU General Public License as published by the
c Free Software Foundation; either version 3 of the License, or (at your
c option) any later version.
c 
c Octave is distributed in the hope that it will be useful, but WITHOUT
c ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
c FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
c for more details.
c 
c You should have received a copy of the GNU General Public License
c along with Octave; see the file COPYING.  If not, see
c <http://www.gnu.org/licenses/>.

      subroutine fortsub (n, a, s)
      implicit none
      character*(*) s
      real*8 a(*)
      integer*4 i, n, ioerr
      do i = 1, n
        if (a(i) .eq. 0d0) then
          call xstopx ('fortsub: divide by zero')
        else
          a(i) = 1d0 / a(i)
        endif
      enddo
      write (unit = s, fmt = '(a,i3,a,a)', iostat = ioerr)
     $       'There are ', n, ' values in the input vector',
     $       char(0)
      if (ioerr .ne. 0) then
        call xstopx ('fortsub: error writing string')
      endif
      return
      end

