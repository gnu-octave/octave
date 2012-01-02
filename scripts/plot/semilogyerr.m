## Copyright (C) 2000-2012 Teemu Ikonen
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
## @deftypefn {Function File} {} semilogyerr (@var{args})
## Produce two-dimensional plots using a logarithmic scale for the @var{y}
## axis and errorbars at each data point.  Many different combinations of
## arguments are possible.  The most used form is
##
## @example
## semilogyerr (@var{x}, @var{y}, @var{ey}, @var{fmt})
## @end example
##
## @noindent
## which produces a semi-logarithmic plot of @var{y} versus @var{x}
## with errors in the @var{y}-scale defined by @var{ey} and the plot
## format defined by @var{fmt}.  See @code{errorbar} for available formats and
## additional information.
## @seealso{errorbar, loglogerr, semilogxerr}
## @end deftypefn

## Created: 20.2.2001
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function retval = semilogyerr (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("semilogyerr", varargin{:});

  oldh = gca ();
  unwind_protect
    axes (h);
    newplot ();

    set (h, "yscale", "log");

    tmp = __errcomm__ ("semilogyerr", h, varargin{:});

    if (nargout > 0)
      retval = tmp;
    endif
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

endfunction

%!demo
%! clf
%! x = 0.25:0.25:10;
%! y = wblpdf (x, 4, 2);
%! eyu = rand (size (y));
%! eyl = 1.0 - 1./(1+eyu);
%! semilogyerr (x, y, eyl.*y, eyu.*y, "~-d")
%! xlim ([0 10])

