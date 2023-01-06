########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {} ezplot3 (@var{fx}, @var{fy}, @var{fz})
## @deftypefnx {} {} ezplot3 (@dots{}, @var{dom})
## @deftypefnx {} {} ezplot3 (@dots{}, @var{n})
## @deftypefnx {} {} ezplot3 (@dots{}, "animate")
## @deftypefnx {} {} ezplot3 (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} ezplot3 (@dots{})
##
## Plot a parametrically defined curve in three dimensions.
##
## @var{fx}, @var{fy}, and @var{fz} are strings, inline functions,
## or function handles with one argument defining the function.  By
## default the plot is over the domain @code{0 <= @var{t} <= 2*pi}
## with 500 points.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## values of @var{t}.
##
## @var{n} is a scalar defining the number of points to use in plotting the
## function.
##
## If the @qcode{"animate"} option is given then the plotting is animated
## in the style of @code{comet3}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
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
## @seealso{plot3, comet3, ezplot, ezmesh, ezsurf}
## @end deftypefn

function h = ezplot3 (varargin)

  [htmp, needusage] = __ezplot__ ("plot3", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! fx = @(t) cos (t);
%! fy = @(t) sin (t);
%! fz = @(t) t;
%! ezplot3 (fx, fy, fz, [0, 10*pi]);

%!demo
%! clf;
%! fx = @(t) cos (t);
%! fy = @(t) sin (t);
%! fz = @(t) t;
%! ezplot3 (fx, fy, fz, [0, 5*pi], "animate");
