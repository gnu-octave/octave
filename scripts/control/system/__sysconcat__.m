## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Undocumented internal function.

function c = __sysconcat__ (a, b)

  ## c = __sysconcat__ (a, b)
  ## cell array replacement for append, used by control systems toolbox

  if (ischar (a))
    a = {a};
  endif
  if (ischar (b))
    b = {b};
  endif

  if (! (is_signal_list (a) && is_signal_list (b)))
    error("need cell arrays of strings");
  endif

  c = a;
  la = length (a);
  for ii = 1:length (b)
    c{la+ii} = b{ii};
  endfor

endfunction
