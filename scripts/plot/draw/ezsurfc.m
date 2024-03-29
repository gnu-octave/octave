########################################################################
##
## Copyright (C) 2007-2024 The Octave Project Developers
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
## @deftypefn  {} {} ezsurfc (@var{f})
## @deftypefnx {} {} ezsurfc (@var{fx}, @var{fy}, @var{fz})
## @deftypefnx {} {} ezsurfc (@dots{}, @var{dom})
## @deftypefnx {} {} ezsurfc (@dots{}, @var{n})
## @deftypefnx {} {} ezsurfc (@dots{}, "circ")
## @deftypefnx {} {} ezsurfc (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} ezsurfc (@dots{})
##
## Plot the surface and contour lines defined by a function.
##
## @var{f} is a string, inline function, or function handle with two arguments
## defining the function.  By default the plot is over the meshed domain
## @code{-2*pi <= @var{x} | @var{y} <= 2*pi} with 60 points in each dimension.
##
## If three functions are passed, then plot the parametrically defined
## function @code{[@var{fx}(@var{s}, @var{t}), @var{fy}(@var{s}, @var{t}),
## @var{fz}(@var{s}, @var{t})]}.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## values of both @var{x} and @var{y}.  If @var{dom} is a four element vector,
## then the minimum and maximum values are @code{[xmin xmax ymin ymax]}.
##
## @var{n} is a scalar defining the number of points to use in each dimension.
##
## If the argument @qcode{"circ"} is given, then the function is plotted over
## a disk centered on the middle of the domain @var{dom}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a 2-element vector with a graphics
## handle for the created surface plot and a second handle for the created
## contour plot.
##
## Example:
##
## @example
## @group
## f = @@(x,y) sqrt (abs (x .* y)) ./ (1 + x.^2 + y.^2);
## ezsurfc (f, [-3, 3]);
## @end group
## @end example
##
## @seealso{surfc, ezsurf, ezplot, ezmesh, ezmeshc, shading}
## @end deftypefn

function h = ezsurfc (varargin)

  [htmp, needusage] = __ezplot__ ("surfc", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ("default");
%! f = @(x,y) sqrt (abs (x .* y)) ./ (1 + x.^2 + y.^2);
%! ezsurfc (f, [-3, 3]);
