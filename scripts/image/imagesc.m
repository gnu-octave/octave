########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn  {} {} imagesc (@var{img})
## @deftypefnx {} {} imagesc (@var{x}, @var{y}, @var{img})
## @deftypefnx {} {} imagesc (@dots{}, @var{climits})
## @deftypefnx {} {} imagesc (@dots{}, "@var{prop}", @var{val}, @dots{})
## @deftypefnx {} {} imagesc ("@var{prop1}", @var{val1}, @dots{})
## @deftypefnx {} {} imagesc (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} imagesc (@dots{})
## Display a scaled version of the matrix @var{img} as a color image.
##
## The colormap is scaled so that the entries of the matrix occupy the entire
## colormap.  If @code{@var{climits} = [@var{lo}, @var{hi}]} is given, then
## that range is set to the @qcode{"clim"} of the current axes.
##
## @var{x} and @var{y} are optional 2-element vectors, @w{@code{[min, max]}},
## which specify the coordinates of the centers of the corner pixels.
## If a range is specified as @w{@code{[max, min]}} then the image will be
## reversed along that axis.  For convenience, @var{x} and @var{y} may be
## specified as N-element vectors matching the length of the data in @var{img}.
## However, only the first and last elements will be used to determine
## the image limits.
##
## The optional return value @var{h} is a graphics handle to the image.
##
## Calling Forms: The @code{imagesc} function can be called in two forms:
## High-Level and Low-Level.  When invoked with normal options, the High-Level
## form is used which first calls @code{newplot} to prepare the graphic figure
## and axes.  When the only inputs to @code{image} are property/value pairs
## the Low-Level form is used which creates a new instance of an image object
## and inserts it in the current axes.  The full list of properties is
## documented at @ref{Image Properties}.
##
## @seealso{image, imshow, caxis}
## @end deftypefn

function h = imagesc (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("imagesc", varargin{:});

  chararg = find (cellfun ("isclass", varargin, "char"), 1, "first");

  do_new = true;
  if (nargin == 0)
    print_usage ();
  elseif (chararg == 1)
    ## Low-Level syntax
    do_new = false;
    img = x = y = climits = [];
  elseif (nargin == 1 || chararg == 2)
    img = varargin{1};
    x = y = climits = [];
  elseif (nargin == 2 || chararg == 3)
    img = varargin{1};
    climits = varargin{2};
    x = y = [];
  elseif (nargin == 3 || chararg == 4)
    x = varargin{1};
    y = varargin{2};
    img = varargin{3};
    climits = [];
  elseif (nargin == 4 || chararg == 5)
    x = varargin{1};
    y = varargin{2};
    img = varargin{3};
    climits = varargin{4};
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    if (do_new)
      hax = newplot (hax);
    elseif (isempty (hax))
      hax = gca ();
    endif

    if (do_new)
      htmp = image (x, y, img, "cdatamapping", "scaled", varargin{chararg:end});
    else
      htmp = image ("cdatamapping", "scaled", varargin{:});
    endif

    if (do_new && ! ishold (hax))
      ## use given climits or guess them from the matrix
      if (numel (climits) == 2 && climits(1) <= climits(2))
        set (hax, "clim", double (climits));
      elseif (! isempty (climits))
        error ("imagesc: CLIMITS must be in form [lo, hi]");
      endif
    endif
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
%! colormap ("default");
%! img = 1 ./ hilb (11);
%! x = y = -5:5;
%! subplot (2,2,1);
%!  h = imagesc (x, y, img);
%!  ylabel ("limits = [-5.5, 5.5]");
%!  title ("imagesc (x, y, img)");
%! subplot (2,2,2);
%!  h = imagesc (-x, y, img);
%!  title ("imagesc (-x, y, img)");
%! subplot (2,2,3);
%!  h = imagesc (x, -y, img);
%!  title ("imagesc (x, -y, img)");
%!  ylabel ("limits = [-5.5, 5.5]");
%! subplot (2,2,4);
%!  h = imagesc (-x, -y, img);
%!  title ("imagesc (-x, -y, img)");

%!demo
%! clf;
%! colormap ("default");
%! g = 0.1:0.1:10;
%! h = g'*g;
%! imagesc (g, g, sin (h));
%! hold on;
%! imagesc (g, g+12, cos (h/2));
%! axis ([0 10 0 22]);
%! hold off;
%! title ("two consecutive images w/hold()");

%!demo
%! clf;
%! colormap ("default");
%! g = 0.1:0.1:10;
%! h = g'*g;
%! imagesc (g, g, sin (h));
%! hold on;
%! plot (g, 11.0 * ones (size (g)));
%! imagesc (g, g+12, cos (h/2));
%! axis ([0 10 0 22]);
%! hold off;
%! title ("image, line, image w/hold()");

%!demo
%! clf;
%! colormap ("default");
%! g = 0.1:0.1:10;
%! h = g'*g;
%! plot (g, 10.5 * ones (size (g)));
%! hold on;
%! imagesc (g, g, sin (h));
%! plot (g, 11.0 * ones (size (g)));
%! imagesc (g, g+12, cos (h/2));
%! plot (g, 11.5 * ones (size (g)));
%! axis ([0 10 0 22]);
%! hold off;
%! title ("line, image, line, image, line w/hold()");

%!demo  # bug #48879
%! clf;
%! img = reshape (1:100, 10, 10);
%! imagesc (img);
%! colormap (prism (10));
%! title ("10 vertical color bars");
