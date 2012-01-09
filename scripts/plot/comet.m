## Copyright (C) 2008-2012 Ben Abbott
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
## @deftypefn  {Function File} {} comet (@var{y})
## @deftypefnx {Function File} {} comet (@var{x}, @var{y})
## @deftypefnx {Function File} {} comet (@var{x}, @var{y}, @var{p})
## @deftypefnx {Function File} {} comet (@var{ax}, @dots{})
## Produce a simple comet style animation along the trajectory provided by
## the input coordinate vectors (@var{x}, @var{y}), where @var{x} will default
## to the indices of @var{y}.
##
## The speed of the comet may be controlled by @var{p}, which represents the
## time which passes as the animation passes from one point to the next.  The
## default for @var{p} is 0.1 seconds.
##
## If @var{ax} is specified the animation is produced in that axis rather than
## the @code{gca}.
## @end deftypefn

## Author: Ben Abbott bpabbott@mac.com
## Created: 2008-09-21

function comet (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ("comet", varargin{:});

  if (nargin == 0)
    print_usage ();
  elseif (nargin == 1)
    y = varargin{1};
    x = 1:numel(y);
    p = 0.1;
  elseif (nargin == 2)
    x = varargin{1};
    y = varargin{2};
    p = 0.1;
  elseif (nargin == 3)
    x = varargin{1};
    y = varargin{2};
    p = varargin{3};
  endif

  oldh = gca ();
  unwind_protect
    axes (h);
    newplot ();
    theaxis = [min(x), max(x), min(y), max(y)];
    num = numel (y);
    dn = round (num/10);
    for n = 1:(num+dn);
      m = n - dn;
      m = max ([m, 1]);
      k = min ([n, num]);
      h = plot (x(1:m), y(1:m), "r", x(m:k), y(m:k), "g", x(k), y(k), "ob");
      axis (theaxis);
      drawnow ();
      pause (p);
    endfor
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

endfunction

%!demo
%! clf
%! t = 0:.1:2*pi;
%! x = cos(2*t).*(cos(t).^2);
%! y = sin(2*t).*(sin(t).^2);
%! comet(x,y)


