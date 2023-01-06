########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
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
## @deftypefn  {} {} loglog (@var{y})
## @deftypefnx {} {} loglog (@var{x}, @var{y})
## @deftypefnx {} {} loglog (@var{x}, @var{y}, @var{prop}, @var{value}, @dots{})
## @deftypefnx {} {} loglog (@var{x}, @var{y}, @var{fmt})
## @deftypefnx {} {} loglog (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} loglog (@dots{})
## Produce a 2-D plot using logarithmic scales for both axes.
##
## See the documentation of @code{plot} for a description of the arguments
## that @code{loglog} will accept.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created plot.
## @seealso{plot, semilogx, semilogy}
## @end deftypefn

function h = loglog (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("loglog", varargin{:});

  if (nargs < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    set (hax, "xscale", "log", "yscale", "log");
    if (! ishold ())
      set (hax, "xminortick", "on", "yminortick", "on", "box", "on");
    endif

    htmp = __plt__ ("loglog", hax, varargin{:});

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
%! t = 1:0.01:10;
%! x = sort ((t .* (1 + rand (size (t)))) .^ 2);
%! y = (t .* (1 + rand (size (t)))) .^ 2;
%! loglog (x, y);
%! title ({"loglog() plot", "Both axes are logarithmic"});

%!demo
%! clf;
%! a = logspace (-5, 1, 10);
%! b =-logspace (-5, 1, 10);
%!
%! subplot (1,2,1);
%!  loglog (a, b);
%!  title ("loglog (a, b)");
%!
%! subplot (1,2,2);
%!  loglog (a, abs (b));
%!  set (gca, "ydir", "reverse");
%!  title ("loglog (a, abs (b))");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a = logspace (-5, 1, 10);
%!   b = logspace (-5, 1, 10);
%!   loglog (a, b);
%!   assert (get (gca, "yscale"), "log");
%!   assert (get (gca, "xscale"), "log");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a = logspace (-5, 1, 10);
%!   b =-logspace (-5, 1, 10);
%!   loglog (a, b);
%!   axis tight;
%!   assert (all (get (gca, "ytick") < 0));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
