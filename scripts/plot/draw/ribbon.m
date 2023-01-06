########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {} ribbon (@var{y})
## @deftypefnx {} {} ribbon (@var{x}, @var{y})
## @deftypefnx {} {} ribbon (@var{x}, @var{y}, @var{width})
## @deftypefnx {} {} ribbon (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} ribbon (@dots{})
## Draw a ribbon plot for the columns of @var{y} vs. @var{x}.
##
## If @var{x} is omitted, a vector containing the row numbers is assumed
## (@code{1:rows (Y)}).  Alternatively, @var{x} can also be a vector with
## same number of elements as rows of @var{y} in which case the same
## @var{x} is used for each column of @var{y}.
##
## The optional parameter @var{width} specifies the width of a single ribbon
## (default is 0.75).
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to
## the surface objects representing each ribbon.
## @seealso{surface, waterfall}
## @end deftypefn

function h = ribbon (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("ribbon", varargin{:});

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 1)
    y = varargin{1};
    if (isvector (y))
      y = y(:);
    endif
    x = 1:rows (y);
    width = 0.75;
  elseif (nargin == 2)
    x = varargin{1};
    y = varargin{2};
    width = 0.75;
  else
    x = varargin{1};
    y = varargin{2};
    width = varargin{3};
  endif

  if (! size_equal (x, y))
    if (! isvector (x) || rows (y) != numel (x))
      error (["ribbon: X must have the same dimensions as Y or be a "  ...
              "vector with the same number of rows as Y"]);
    endif
    x = repmat (x(:), 1, columns (y));
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    [nr, nc] = size (y);
    htmp = zeros (nc, 1);

    for c = nc:-1:1
      zz = [y(:,c), y(:,c)];
      yy = x(:,c);
      xx = [c - width / 2, c + width / 2];
      [xx, yy] = meshgrid (xx, yy);
      cc = repmat (c, size (zz));
      htmp(c) = surface (xx, yy, zz, cc);
    endfor

    if (! ishold ())
      set (hax, "view", [-37.5, 30],
                "xgrid", "on", "ygrid", "on", "zgrid", "on");
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
%! [x, y, z] = sombrero ();
%! ribbon (y, z);
%! title ("ribbon() plot of sombrero ()");

%!FIXME: Could have some input validation tests here
