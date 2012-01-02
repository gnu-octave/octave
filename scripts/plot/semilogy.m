## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn  {Function File} {} semilogy (@var{y})
## @deftypefnx {Function File} {} semilogy (@var{x}, @var{y})
## @deftypefnx {Function File} {} semilogy (@var{x}, @var{y}, @var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} semilogy (@var{x}, @var{y}, @var{fmt})
## @deftypefnx {Function File} {} semilogy (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} semilogy (@dots{})
## Produce a two-dimensional plot using a logarithmic scale for the @var{y}
## axis.  See the documentation of @code{plot} for a description of the
## arguments that @code{semilogy} will accept.
##
## The optional return value @var{h} is a graphics handle to the created plot.
## @seealso{plot, semilogx, loglog}
## @end deftypefn

## Author: jwe

function retval = semilogy (varargin)

  [h, varargin, nargs] = __plt_get_axis_arg__ ("semilogy", varargin{:});

  if (nargs < 1)
    print_usage();
  endif

  oldh = gca ();
  unwind_protect
    axes (h);
    newplot ();

    set (h, "yscale", "log");
    if (any( strcmp (get (gca, "nextplot"), {"new", "replace"})))
      set (h, "yminortick", "on");
    endif

    tmp = __plt__ ("semilogy", h, varargin{:});

    if (nargout > 0)
      retval = tmp;
    endif

  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

endfunction

%!demo
%! clf ();
%! x = 1:0.01:10;
%! y = (x .* (1 + rand (size (x)))) .^ 2;
%! semilogy (x, y);

%!demo
%! clf ();
%! x = logspace (-5, 1, 10);
%! y = logspace (-5, 1, 10);
%!
%! subplot (2, 1, 1);
%! semilogy (x, y);
%! ylabel ("semilogy (x, y)");
%!
%! subplot (2, 1, 2);
%! semilogy (x, -y);
%! ylabel ("semilogy (x, -y)");

%!demo
%! clf ();
%! x = logspace (-5, 1, 10);
%! y = logspace (-5, 1, 10);
%!
%! subplot (2, 1, 1);
%! semilogy (x, y);
%! set (gca, "ydir", "reverse", "activepositionproperty", "outerposition");
%! ylabel ({"semilogy (x, y)", "ydir = reversed"});
%!
%! subplot (2, 1, 2);
%! semilogy (x, -y);
%! set (gca, "ydir", "reverse", "activepositionproperty", "outerposition");
%! ylabel ({"semilogy (x, -y)", "ydir = reversed"});

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a = logspace (-5, 1, 10);
%!   b = logspace (-5, 1, 10);
%!   semilogy (a, b);
%!   assert (get (gca, "yscale"), "log");
%!   assert (get (gca, "xscale"), "linear");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   a = logspace (-5, 1, 10);
%!   b =-logspace (-5, 1, 10);
%!   semilogy (a, b);
%!   axis tight;
%!   assert (all (get (gca, "ytick") < 0));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

