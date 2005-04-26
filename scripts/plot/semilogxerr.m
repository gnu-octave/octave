## Copyright (C) 2000, 2001, 2002 Teemu Ikonen
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
## @deftypefn {Function File} {} semilogxerr (@var{args})
## This function produces two-dimensional plots on a semilogarithm axis 
## with errorbars. Many different combinations of arguments are possible.  
## The most used form is
##
## @example
## semilogxerr (@var{x}, @var{y}, @var{ey}, @var{fmt})
## @end example
##
## @noindent
## which produces a semi-logarithm plot of @var{y} versus @var{x}
## with errors in the @var{y}-scale defined by @var{ey} and the plot
## format defined by @var{fmt}. See errorbar for available formats and 
## additional information.
##
## @end deftypefn
##
## @seealso{errorbar, loglogerr semilogyerr, polar, mesh, contour, __pltopt__, 
## bar, stairs, replot, xlabel, ylabel, and title}

## Created: 20.2.2001
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function semilogxerr (varargin)

  if (nargin < 2)
    usage ("semilogxerr (...)");
  endif

  __gnuplot_raw__ ("set logscale x;\n");
  __gnuplot_raw__ ("set nologscale y;\n");
  __gnuplot_raw__ ("set nopolar;\n");

  __errcomm__ ("semilogxerr", varargin{:});

endfunction
