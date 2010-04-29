## Copyright (C) 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2008,
##               2009 Teemu Ikonen
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
## @deftypefn {Function File} {} errorbar (@var{args})
## This function produces two-dimensional plots with errorbars.  Many
## different combinations of arguments are possible.  The simplest form is
##
## @example
## errorbar (@var{y}, @var{ey})
## @end example
##
## @noindent
## where the first argument is taken as the set of @var{y} coordinates
## and the second argument @var{ey} is taken as the errors of the
## @var{y} values.  @var{x} coordinates are taken to be the indices
## of the elements, starting with 1.
##
## If more than two arguments are given, they are interpreted as
##
## @example
## errorbar (@var{x}, @var{y}, @dots{}, @var{fmt}, @dots{})
## @end example
##
## @noindent
## where after @var{x} and @var{y} there can be up to four error
## parameters such as @var{ey}, @var{ex}, @var{ly}, @var{uy}, etc.,
## depending on the plot type.  Any number of argument sets may appear,
## as long as they are separated with a format string @var{fmt}.
##
## If @var{y} is a matrix, @var{x} and error parameters must also be matrices
## having same dimensions.  The columns of @var{y} are plotted versus the
## corresponding columns of @var{x} and errorbars are drawn from
## the corresponding columns of error parameters.
##
## If @var{fmt} is missing, yerrorbars ("~") plot style is assumed.
##
## If the @var{fmt} argument is supplied, it is interpreted as in
## normal plots.  In addition the following plot styles are supported by
## errorbar:
##
## @table @samp
## @item ~
## Set yerrorbars plot style (default).
##
## @item >
## Set xerrorbars plot style.
##
## @item ~>
## Set xyerrorbars plot style.
##
## @item #
## Set boxes plot style.
##
## @item #~
## Set boxerrorbars plot style.
##
## @item #~>
## Set boxxyerrorbars plot style.
## @end table
##
## Examples:
##
## @example
## errorbar (@var{x}, @var{y}, @var{ex}, ">")
## @end example
##
## produces an xerrorbar plot of @var{y} versus @var{x} with @var{x}
## errorbars drawn from @var{x}-@var{ex} to @var{x}+@var{ex}.
##
## @example
## @group
## errorbar (@var{x}, @var{y1}, @var{ey}, "~",
##           @var{x}, @var{y2}, @var{ly}, @var{uy})
## @end group
## @end example
##
## produces yerrorbar plots with @var{y1} and @var{y2} versus @var{x}.
## Errorbars for @var{y1} are drawn from @var{y1}-@var{ey} to
## @var{y1}+@var{ey}, errorbars for @var{y2} from @var{y2}-@var{ly} to
## @var{y2}+@var{uy}.
##
## @example
## @group
## errorbar (@var{x}, @var{y}, @var{lx}, @var{ux},
##           @var{ly}, @var{uy}, "~>")
## @end group
## @end example
##
## produces an xyerrorbar plot of @var{y} versus @var{x} in which
## @var{x} errorbars are drawn from @var{x}-@var{lx} to @var{x}+@var{ux}
## and @var{y} errorbars from @var{y}-@var{ly} to @var{y}+@var{uy}.
## @seealso{semilogxerr, semilogyerr, loglogerr}
## @end deftypefn

## Created: 18.7.2000
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function retval = errorbar (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("errorbar", varargin{:});

  oldh = gca ();
  unwind_protect
    axes (h);
    newplot ();

    tmp = __errcomm__ ("errorbar", h, varargin{:});

    if (nargout > 0)
      retval = tmp;
    endif
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

endfunction

%!demo
%! errorbar(0:10,rand(1,11),0.25*rand(1,11))

%!demo
%! errorbar(0:10,rand(1,11),rand(1,11), ">")

%!demo
%! x = 0:0.5:2*pi; 
%! err = x/100; 
%! y1 = sin (x); 
%! y2 = cos (x); 
%! hg = errorbar (x, y1, err, "~", x, y2, err, ">");

%!demo
%! x = 0:0.5:2*pi; 
%! err = x/100; 
%! y1 = sin (x); 
%! y2 = cos (x); 
%! hg = errorbar (x, y1, err, err, "#", x, y2, err, err, "#~");

%!demo
%! x = 0:0.5:2*pi; 
%! err = x/100; 
%! y1 = sin (x); 
%! y2 = cos (x); 
%! hg = errorbar (x, y1, err, err, err, err, "~>", ...
%!                x, y2, err, err, err, err, "#~>");

