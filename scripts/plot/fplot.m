## Copyright (C) 2005-2012 Paul Kienzle
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
## @deftypefn  {Function File} {} fplot (@var{fn}, @var{limits})
## @deftypefnx {Function File} {} fplot (@var{fn}, @var{limits}, @var{tol})
## @deftypefnx {Function File} {} fplot (@var{fn}, @var{limits}, @var{n})
## @deftypefnx {Function File} {} fplot (@dots{}, @var{fmt})
## Plot a function @var{fn} within defined limits.
## @var{fn} is a function handle, inline function, or string
## containing the name of the function to evaluate.
## The limits of the plot are given by @var{limits} of the form
## @code{[@var{xlo}, @var{xhi}]} or @code{[@var{xlo}, @var{xhi},
## @var{ylo}, @var{yhi}]}.  @var{tol} is the default tolerance to use for the
## plot, and if @var{tol} is an integer it is assumed that it defines the
## number points to use in the plot.  The @var{fmt} argument is passed
## to the plot command.
##
## @example
## @group
## fplot ("cos", [0, 2*pi])
## fplot ("[cos(x), sin(x)]", [0, 2*pi])
## @end group
## @end example
## @seealso{plot}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function fplot (fn, limits, n, fmt)
  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (!isreal (limits) || (numel (limits) != 2 && numel (limits) != 4))
    error ("fplot: second input argument must be a real vector with 2 or 4 elements");
  endif

  if (nargin < 3)
    n = 0.002;
  endif

  have_linespec = true;
  if (nargin < 4)
    have_linespec = false;
  endif

  if (ischar (n))
    have_linespec = true;
    fmt = n;
    n = 0.002;
  endif

  if (strcmp (typeinfo (fn), "inline function"))
    fn = vectorize (fn);
    nam = formula (fn);
  elseif (isa (fn, "function_handle"))
    nam = func2str (fn);
  elseif (all (isalnum (fn)))
    nam = fn;
  elseif (ischar (fn))
     fn = vectorize (inline (fn));
     nam = formula (fn);
  else
    error ("fplot: first input argument must be a function handle, inline function or string");
  endif

  if (floor(n) != n)
    tol = n;
    x0 = linspace (limits(1), limits(2), 5)';
    y0 = feval (fn, x0);
    err0 = Inf;
    n = 8;
    x = linspace (limits(1), limits(2), n)';
    y = feval (fn, x);

    while (n < 2 .^ 20)
      y00 = interp1 (x0, y0, x, "linear");
      err = 0.5 * max (abs ((y00 - y) ./ (y00 + y))(:));
      if (err == err0 || 0.5 * max (abs ((y00 - y) ./ (y00 + y))(:)) < tol)
        break;
      endif
      x0 = x;
      y0 = y;
      err0 = err;
      n = 2 * (n - 1) + 1;
      x = linspace (limits(1), limits(2), n)';
      y = feval (fn, x);
    endwhile
  else
    x = linspace (limits(1), limits(2), n)';
    y = feval (fn, x);
  endif

  if (have_linespec)
    plot (x, y, fmt);
  else
    plot (x, y);
  endif

  if (length (limits) > 2)
    axis (limits);
  endif

  if (isvector (y))
    legend (nam);
  else
    for i = 1:columns (y)
      nams{i} = sprintf ("%s(:,%i)", nam, i);
    endfor
    legend (nams{:});
  endif
endfunction

%!demo
%! clf
%! fplot ("cos", [0, 2*pi])

%!demo
%! clf
%! fplot ("[cos(x), sin(x)]", [0, 2*pi])
