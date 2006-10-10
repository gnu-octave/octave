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
## @deftypefnx {Function File} {} fplot (@var{fn}, @var{limits}, @var{n})
## Plots a function @var{fn}, within the defined limits. @var{fn}
## an be either a string, a function handle or an inline function.
## The limits of the plot are given by @var{limits} of the form
## @code{[@var{xlo}, @var{xhi}]} or @code{[@var{xlo}, @var{xhi},
## @var{ylo}, @var{yhi}]}. @var{n} is the number of points to use and
## defaults to 100. 
##
## @example
##    fplot('cos',[0,2*pi])
##    fplot('[cos(x),sin(x)]',[0,2*pi])
## @end example
## @end deftypefn

function fplot (fn, limits, n)
  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (nargin < 3) 
    n = 100; 
  endif

  x = linspace (limits(1), limits(2), n)';

  if (strcmp (class (fn), "inline function")
      || strcmp (class (fn), "function handle"))
    y = fn (x);
  elseif (all (isalnum (fn)))
    y = feval (fn, x);
  else
    finl = inline (fn);
    y = finl (x);
  endif

  if (length (limits) > 2) 
    axis (limits);
  endif

  plot (x, y, [";", fn, ";"]);

endfunction
