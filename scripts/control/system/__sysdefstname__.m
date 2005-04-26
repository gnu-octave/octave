## Copyright (C) 1996 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __sysdefstname__ (@var{n}, @var{nz})
## return default state names given @var{n}, @var{nz}
##
## used internally, minimal argument checking
## @end deftypefn

function stname = __sysdefstname__ (n, nz)

  stname = {};
  if (n > 0)
    for ii = 1:n
      stname{ii} = sprintf ("x_%d", ii);
    endfor
  endif

  ## Set default names for discrete states
  if (nz > 0)
    for ii = (n+1):(n+nz)
      stname{ii} = sprintf ("xd_%d", ii);
    endfor
  endif

endfunction
