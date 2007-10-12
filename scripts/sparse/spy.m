## Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
##               2007 Andy Adler
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

## -*- texinfo -*-
## @deftypefn {Function File} {} spy (@var{x})
## @deftypefnx {Function File} {} spy (@dots{}, @var{markersize})
## @deftypefnx {Function File} {} spy (@dots{}, @var{LineSpec})
## Plot the sparsity pattern of the sparse matrix @var{x}. If the argument
## @var{markersize} is given as an scalar value, it is used to determine the
## point size in the plot. If the string @var{LineSpec} is given it is
## passed to @code{plot} and determines the appearance of the plot.
## @seealso{plot}
## @end deftypefn

function spy (S, varargin) 

  markersize = NaN;
  if (numel (i) < 1000)
    LineSpec = "*";
  else
    LineSpec = ".";
  endif
  for i = 1:length(varargin)
    if (ischar(varargin{i}))
      LineSpec = varargin{i};
    elseif (isscalar (varargin{i}))
      markersize = varargin{i};
    else
      error ("spy: expected markersize or linespec");
    endif
  endfor

  if (issparse (S))
    [i, j, s, m, n] = spfind (S);
  else
    [i, j, s] = find (S);
    [m, n] = size (S);
  endif

  if (isnan (markersize))
    plot (j, i, LineSpec);
  else
    plot (j, i, LineSpec, "MarkerSize", markersize);
  endif

  axis ([0, n+1, 0, m+1], "ij");

endfunction
