## Copyright (C) 2005 Paul Kienzle <pkienzle@users.sf.net>
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} fplot (@var{fn}, @var{limits})
## @deftypefnx {Function File} {} fplot (@var{fn}, @var{limits}, @var{tol})
## @deftypefnx {Function File} {} fplot (@var{fn}, @var{limits}, @var{n})
## @deftypefnx {Function File} {} fplot (@dots{}, @var{LineSpec})
## Plot a function @var{fn}, within the defined limits.  @var{fn}
## an be either a string, a function handle or an inline function.
## The limits of the plot are given by @var{limits} of the form
## @code{[@var{xlo}, @var{xhi}]} or @code{[@var{xlo}, @var{xhi},
## @var{ylo}, @var{yhi}]}. @var{tol} is the default tolerance to use for the
## plot, and if @var{tol} is an integer it is assumed that it defines the 
## number points to use in the plot. The @var{LineSpec} is passed to the plot
## command.
##
## @example
##    fplot ("cos", [0, 2*pi])
##    fplot ("[cos(x), sin(x)]", [0, 2*pi])
## @end example
## @seealso{plot}
## @end deftypefn

function fplot (fn, limits, n, linespec)
  if (nargin < 2 || nargin > 3)
    print_usage ();
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
    linespec = n;
    n = 0.002;
  endif

  if (strcmp (typeinfo (fn), "inline function"))
    fn = vectorize (fn);
    nam = formula (fn);
  elseif (isa (fn, "function_handle"))
    nam = func2str (fn);
  elseif (all (isalnum (fn)))
    nam = fn;
  else
    fn = vectorize (inline (fn));
    nam = formula (fn);
  endif

  if (floor(n) != n)
    tol = n;
    x0 = linspace (limits(1), limits(2), 3)';
    y0 = feval (fn, x0);
    err0 = Inf;
    n = 5;
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

  if (length (limits) > 2) 
    axis (limits);
  endif

  if (have_linespec)
    plot (x, y, linespec);
  else
    plot (x, y);
  endif
  if (isvector(y))
    legend (nam);
  else
    for i=1:columns(y)
      nams{i} = sprintf ("%s(:,%i)", nam, i);
    endfor
    legend (nams{:});
  endif
endfunction
