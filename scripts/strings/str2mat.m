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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} str2mat (@var{s_1}, @dots{}, @var{s_n})
## Return a matrix containing the strings @var{s_1}, @dots{}, @var{s_n} as
## its rows.  Each string is padded with blanks in order to form a valid
## matrix.
##
## This function is modelled after @sc{Matlab}.  In Octave, you can create
## a matrix of strings by @code{[@var{s_1}; @dots{}; @var{s_n}]} even if
## the strings are not all the same length.
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function retval = str2mat (varargin)

  if (nargin == 0)
    usage ("str2mat (s1, ...)");
  endif

  nc = 0;
  nr = 0;

  nr = zeros (nargin, 1);
  nc = zeros (nargin, 1);
  for k = 1 : nargin
    s = varargin{k};
    if (! isstr (s))
      s = setstr (s);
    endif
    [nr(k), nc(k)] = size (s);
  endfor

  tmp = find (nr == 0);

  if (! isempty (tmp))
    nr(tmp) = 1;
  endif

  retval_nr = sum (nr);
  retval_nc = max (nc);

  retval = setstr (ones (retval_nr, retval_nc) * toascii (" "));

  row_offset = 0;
  for k = 1 : nargin
    s = varargin{k};
    if (! isstr (s))
      s = setstr (s);
    endif
    if (nc(k) > 0)
      retval ((row_offset + 1) : (row_offset + nr(k)), 1:nc(k)) = s;
    endif
    row_offset = row_offset + nr(k);
  endfor

endfunction
