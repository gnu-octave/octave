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

## -*- texinfo -*-
## @deftypefn {Function File} {} split (@var{s}, @var{t})
## Divides the string @var{s} into pieces separated by @var{t}, returning
## the result in a string array (padded with blanks to form a valid
## matrix).  For example,
##
## @example
## split ("Test string", "t")
##      @result{} "Tes "
##         " s  "
##         "ring"
## @end example
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function m = split (s, t)

  if (nargin == 2)
    if (isstr (s) && isstr (t))

      l_s = length (s);
      l_t = length (t);

      if (l_s == 0)
	m = "";
	return;
      elseif (l_t == 0)
	m = s';
	return;
      elseif (l_s < l_t)
	error ("split: s must not be shorter than t");
      endif

      if (min (size (s)) != 1 || min (size (t)) != 1)
	error("split: multi-line strings are not supported");
      endif

      ind = findstr (s, t, 0);
      if (length (ind) == 0)
	m = s;
	return;
      endif
      ind2 = [1, ind+l_t];
      ind  = [ind, l_s+1];

      ind_diff = ind-ind2;

      ## Create a matrix of the correct size that's filled with spaces.
      m_rows = length (ind);
      m_cols = max (ind_diff);
      m = repmat (" ", m_rows, m_cols);

      ## Copy the strings to the matrix.
      for i = 1:length (ind)
	tmp = ind2(i):(ind(i)-1);
	m(i,1:length(tmp)) = s(tmp);
      endfor
    else
      error ("split: both s and t must be strings");
    endif
  else
    usage ("split (s, t)");
  endif

endfunction
