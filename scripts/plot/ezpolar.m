## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {} ezpolar (@var{f})
## @deftypefnx {Function File} {} ezpolar (@dots{}, @var{dom})
## @deftypefnx {Function File} {} ezpolar (@dots{}, @var{n})
## @deftypefnx {Function File} {} ezpolar (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} ezpolar (@dots{})
##
## Plot a function in polar coordinates.  The function @var{f} is either
## a string, inline function or function handle with one arguments defining
## the function.  By default the plot is over the domain @code{0 < @var{x} <
## 2*pi} with 60 points.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## value of both @var{t}.  @var{n} is a scalar defining the number of points to
## use.
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## @example
## ezpolar (@@(t) 1 + sin (t));
## @end example
##
## @seealso{polar, ezplot, ezsurf, ezmesh}
## @end deftypefn

function retval = ezpolar (varargin)

  [h, needusage] = __ezplot__ ("polar", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    retval = h;
  endif
endfunction


%!demo
%! clf
%! ezpolar (@(t) 1 + sin (t));

