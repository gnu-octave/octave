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
      program main
      integer i
      double precision d1mach
      double precision t1, t2
      do 10 i = 1, 5
        print *, d1mach (i)
   10 continue
      end
