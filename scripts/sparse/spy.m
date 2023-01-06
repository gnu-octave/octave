########################################################################
##
## Copyright (C) 1998-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} spy (@var{x})
## @deftypefnx {} {} spy (@dots{}, @var{markersize})
## @deftypefnx {} {} spy (@dots{}, @var{line_spec})
## Plot the sparsity pattern of the sparse matrix @var{x}.
##
## If the optional numeric argument @var{markersize} is given, it determines
## the size of the markers used in the plot.
##
## If the optional string @var{line_spec} is given it is passed to @code{plot}
## and determines the appearance of the plot.
## @seealso{plot, gplot}
## @end deftypefn

function spy (x, varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  markersize = NaN;
  if (nnz (x) < 1000)
    line_spec = "*";
  else
    line_spec = ".";
  endif
  for arg = varargin
    arg = arg{1};
    if (ischar (arg))
      if (numel (arg) == 1)
        line_spec = [line_spec, arg];
      else
        line_spec = arg;
      endif
    elseif (isreal (arg) && isscalar (arg))
      markersize = arg;
    else
      error ("spy: expected markersize or linespec");
    endif
  endfor

  [i, j] = find (x);
  [m, n] = size (x);

  if (isnan (markersize))
    plot (j, i, line_spec);
  else
    plot (j, i, line_spec, "markersize", markersize);
  endif

  axis ([0, n+1, 0, m+1], "ij");
  xlabel (sprintf ("nnz = %d", nnz (x)));

endfunction


%!demo
%! clf;
%! spy (sprand (10,10, 0.2));

## Test input validation
%!error <Invalid call> spy ()
%!error <Invalid call> spy (1,2,3,4)
