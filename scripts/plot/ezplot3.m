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
## @deftypefn  {Function File} {} ezplot3 (@var{fx}, @var{fy}, @var{fz})
## @deftypefnx {Function File} {} ezplot3 (@dots{}, @var{dom})
## @deftypefnx {Function File} {} ezplot3 (@dots{}, @var{n})
## @deftypefnx {Function File} {} ezplot3 (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} ezplot3 (@dots{})
##
## Plot a parametrically defined curve in three dimensions.
## @var{fx}, @var{fy}, and @var{fz} are strings, inline functions
## or function handles with one arguments defining the function.  By
## default the plot is over the domain @code{-2*pi < @var{x} < 2*pi}
## with 60 points.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## value of @var{t}.  @var{n} is a scalar defining the number of points to use.
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## @example
## @group
## fx = @@(t) cos (t);
## fy = @@(t) sin (t);
## fz = @@(t) t;
## ezplot3 (fx, fy, fz, [0, 10*pi], 100);
## @end group
## @end example
##
## @seealso{plot3, ezplot, ezsurf, ezmesh}
## @end deftypefn

function retval = ezplot3 (varargin)

  [h, needusage] = __ezplot__ ("plot3", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    retval = h;
  endif
endfunction


%!demo
%! clf
%! fx = @(t) cos (t);
%! fy = @(t) sin (t);
%! fz = @(t) t;
%! ezplot3 (fx, fy, fz, [0, 10*pi], 100);

