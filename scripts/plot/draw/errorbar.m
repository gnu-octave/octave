## Copyright (C) 2000-2013 Teemu Ikonen
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
## @deftypefn  {Function File} {} errorbar (@var{args})
## @deftypefnx {Function File} {} errorbar (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} errorbar (@dots{})
## Create a 2-D with errorbars.
##
## Many different combinations of arguments are possible.  The simplest
## form is
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
## normal plots.  In addition, @var{fmt} may include an errorbar style
## which must precede the line and marker format.  The following plot
## styles are supported by errorbar:
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
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a handle to the hggroup object
## representing the data plot and errorbars.
##
## Examples:
##
## @example
## errorbar (@var{x}, @var{y}, @var{ex}, ">")
## @end example
##
## @noindent
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
## @noindent
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
## @noindent
## produces an xyerrorbar plot of @var{y} versus @var{x} in which
## @var{x} errorbars are drawn from @var{x}-@var{lx} to @var{x}+@var{ux}
## and @var{y} errorbars from @var{y}-@var{ly} to @var{y}+@var{uy}.
## @seealso{semilogxerr, semilogyerr, loglogerr, plot}
## @end deftypefn

## Created: 18.7.2000
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function h = errorbar (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("errorbar", varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    htmp = __errcomm__ ("errorbar", hax, varargin{:});
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! rand_1x11_data1 = [0.82712, 0.50325, 0.35613, 0.77089, 0.20474, 0.69160, 0.30858, 0.88225, 0.35187, 0.14168, 0.54270];
%! rand_1x11_data2 = [0.506375, 0.330106, 0.017982, 0.859270, 0.140641, 0.327839, 0.275886, 0.162453, 0.807592, 0.318509, 0.921112];
%! errorbar (0:10, rand_1x11_data1, 0.25*rand_1x11_data2);
%! title ('errorbar() with Y errorbars');

%!demo
%! clf;
%! rand_1x11_data3 = [0.423650, 0.142331, 0.213195, 0.129301, 0.975891, 0.012872, 0.635327, 0.338829, 0.764997, 0.401798, 0.551850];
%! rand_1x11_data4 = [0.682566, 0.456342, 0.132390, 0.341292, 0.108633, 0.601553, 0.040455, 0.146665, 0.309187, 0.586291, 0.540149];
%! errorbar (0:10, rand_1x11_data3, rand_1x11_data4, '>');
%! title ('errorbar() with X errorbars');

%!demo
%! clf;
%! x = 0:0.5:2*pi;
%! err = x/30;
%! y1 = sin (x);
%! y2 = cos (x);
%! errorbar (x, y1, err, '~', x, y2, err, '>');
%! legend ("Y errbar", "X errbar");
%! title ('errorbar() with 2 datasets');


%!demo
%! clf;
%! x = 0:0.5:2*pi;
%! err = x/30;
%! y1 = sin (x);
%! y2 = cos (x);
%! errorbar (x, y1, err, err, '#r', x, y2, err, err, '#~');
%! legend ("X errbox", "Y errbox");
%! title ('errorbar() with error boxes');

%!demo
%! clf;
%! x = 0:0.5:2*pi;
%! err = x/30;
%! y1 = sin (x);
%! y2 = cos (x);
%! errorbar (x, y1, err, err, err, err, '~>', ...
%!           x, y2, err, err, err, err, '#~>-*');
%! legend ("X-Y errbars", "X-Y errboxes");
%! title ('errorbar() with X-Y errorbars and error boxes');

## Invisible figure used for tests
%!shared hf, hax
%! hf = figure ("visible", "off");
%! hax = axes;

%!error errorbar ()
%!error errorbar (1)
%!error <data argument 1 must be numeric> errorbar (hax, {1}, 2)
%!error <data argument 2 must be numeric> errorbar (hax, 1, {2})
%!error <size of argument 2 does not match others> errorbar (hax, 1, 1:2)
%!error <size of argument 3 does not match others> errorbar (hax, 1, 2, 3:4)
%!error <too many arguments to plot> errorbar (1,2,3,4,5,6,7)

%!error <2 column errorplot is only valid for xerr> errorbar (1,2, "~>")
%!error <6 columns only valid for xyerr and boxxy> errorbar (1,2,3,4,5,6, "~")
%!error <error plot requires 2, 3, 4, or 6 arguments> errorbar (1,2,3,4,5)

## Close figure used for testing
%!test
%! close (hf);

