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
## @deftypefn  {Function File} {} ezmeshc (@var{f})
## @deftypefnx {Function File} {} ezmeshc (@var{fx}, @var{fy}, @var{fz})
## @deftypefnx {Function File} {} ezmeshc (@dots{}, @var{dom})
## @deftypefnx {Function File} {} ezmeshc (@dots{}, @var{n})
## @deftypefnx {Function File} {} ezmeshc (@dots{}, 'circ')
## @deftypefnx {Function File} {} ezmeshc (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} ezmeshc (@dots{})
##
## Plot the mesh and contour lines defined by a function.  @var{f} is a string,
## inline function or function handle with two arguments defining the function.
## By default the plot is over the domain @code{-2*pi < @var{x} < 2*pi} and
## @code{-2*pi < @var{y} < 2*pi} with 60 points in each dimension.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## value of both @var{x} and @var{y}.  If @var{dom} is a four element vector,
## then the minimum and maximum value of @var{x} and @var{y} are specify
## separately.
##
## @var{n} is a scalar defining the number of points to use in each dimension.
##
## If three functions are passed, then plot the parametrically defined
## function @code{[@var{fx} (@var{s}, @var{t}), @var{fy} (@var{s}, @var{t}),
## @var{fz} (@var{s}, @var{t})]}.
##
## If the argument 'circ' is given, then the function is plotted over a disk
## centered on the middle of the domain @var{dom}.
##
## The optional return value @var{h} is a 2-element vector with a graphics
## handle for the created mesh plot and a second handle for the created contour
## plot.
##
## @example
## @group
## f = @@(x,y) sqrt (abs (x .* y)) ./ (1 + x.^2 + y.^2);
## ezmeshc (f, [-3, 3]);
## @end group
## @end example
##
## @seealso{ezplot, ezsurfc, ezsurf, ezmesh}
## @end deftypefn

function retval = ezmeshc (varargin)

  [h, needusage] = __ezplot__ ("meshc", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    retval = h;
  endif
endfunction


%!demo
%! clf
%! f = @(x,y) sqrt(abs(x .* y)) ./ (1 + x.^2 + y.^2);
%! ezmeshc (f, [-3, 3]);

