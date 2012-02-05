## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn  {Function File} {} ezplot (@var{f})
## @deftypefnx {Function File} {} ezplot (@var{fx}, @var{fy})
## @deftypefnx {Function File} {} ezplot (@dots{}, @var{dom})
## @deftypefnx {Function File} {} ezplot (@dots{}, @var{n})
## @deftypefnx {Function File} {} ezplot (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} ezplot (@dots{})
##
## Plot the curve defined by @var{f} in two dimensions.  The function
## @var{f} may be a string, inline function or function handle and can
## have either one or two variables.  If @var{f} has one variable, then
## the function is plotted over the domain @code{-2*pi < @var{x} < 2*pi}
## with 500 points.
##
## If @var{f} has two variables then @code{@var{f}(@var{x},@var{y}) = 0}
## is calculated over the meshed domain @code{-2*pi < @var{x} | @var{y}
## < 2*pi} with 60 by 60 in the mesh.  For example:
##
## @example
## ezplot (@@(@var{x}, @var{y}) @var{x}.^2 - @var{y}.^2 - 1)
## @end example
##
## If two functions are passed as strings, inline functions or function
## handles, then the parametric function
##
## @example
## @group
## @var{x} = @var{fx} (@var{t})
## @var{y} = @var{fy} (@var{t})
## @end group
## @end example
##
## @noindent
## is plotted over the domain @code{-2*pi < @var{t} < 2*pi} with 500
## points.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## value of @var{x}, @var{y} and @var{t}.  If it is a four element
## vector, then the minimum and maximum values of @var{x} and @var{t}
## are determined by the first two elements and the minimum and maximum
## of @var{y} by the second pair of elements.
##
## @var{n} is a scalar defining the number of points to use in plotting
## the function.
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## @seealso{plot, ezplot3}
## @end deftypefn

function retval = ezplot (varargin)

  [h, needusage] = __ezplot__ ("plot", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    retval = h;
  endif
endfunction


%!demo
%! clf
%! ezplot (@cos, @sin)

%!demo
%! clf
%! ezplot ("1/x")

%!demo
%! clf
%! ezplot (inline ("x^2 - y^2 = 1"))

