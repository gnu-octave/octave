## Copyright (C) 1996 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage:  m = split (s, t)
##
## Divides the string S into pieces separated by T, and stores the
## pieces as the rows of M (padded with blanks to form a valid
## matrix).

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function m = split (s, t)

  if (nargin != 2)
    usage ("split (s, t)");
  endif

  if (isstr (s) && isstr (t))

  l_s = length (s);
  l_t = length (t);

  if (l_s < l_t)
    error ("split: s must not be shorter than t");
  endif

  if (l_t == 0)
    ind = 1 : (l_s + 1);
  else
    ind = findstr (s, t, 0);
    if (length (ind) == 0)
      m = s;
      return;
    endif
    ind = [1 - l_t, ind, l_s + 1];
  endif

  cmd = "";

  limit = length (ind) - 1;

  for k = 1 : limit

    range = (ind (k) + l_t) : ind (k + 1) - 1;

    if (k != limit)
      cmd = sprintf ("%s\"%s\", ", cmd, s (range));
    else
      cmd = sprintf ("%s\"%s\"", cmd, s (range));
    endif

  endfor

  m = eval (sprintf ("str2mat (%s);", cmd));


  else
    error ("split:  both s and t must be strings");
  endif

endfunction
