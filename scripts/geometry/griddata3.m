## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn {Function File} {@var{vi} =} griddata3 (@var{x}, @var{y}, @var{z}, @var{v}, @var{xi}, @var{yi}, @var{zi}, @var{method}, @var{options})
##
## Generate a regular mesh from irregular data using interpolation.
## The function is defined by @code{@var{v} = f (@var{x}, @var{y}, @var{z})}.
## The interpolation points are specified by @var{xi}, @var{yi}, @var{zi}.
##
## The interpolation method can be @code{"nearest"} or @code{"linear"}.
## If method is omitted it defaults to @code{"linear"}.
## @seealso{griddata, griddatan, delaunayn}
## @end deftypefn

## Author: David Bateman <dbateman@free.fr>

function vi = griddata3 (x, y, z, v, xi, yi, zi, method, varargin)

  if (nargin < 7)
    print_usage ();
  endif

  if (!all (size (x) == size (y) & size (x) == size(z) & size(x) == size (v)))
    error ("griddata3: X, Y, Z, and V must be vectors of same length");
  endif

  ## meshgrid xi, yi and zi if they are vectors unless they
  ## are vectors of the same length
  if (isvector (xi) && isvector (yi) && isvector (zi)
      && (numel (xi) != numel (yi) || numel (xi) != numel (zi)))
    [xi, yi, zi] = meshgrid (xi, yi, zi);
  endif

  if (any (size(xi) != size(yi)) || any (size(xi) != size(zi)))
    error ("griddata3: XI, YI and ZI must be vectors or matrices of same size");
  endif

  vi = griddatan ([x(:), y(:), z(:)], v(:), [xi(:), yi(:), zi(:)], varargin{:});
  vi = reshape (vi, size (xi));

endfunction


%!testif HAVE_QHULL
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 0);
%! x = 2 * rand (1000, 1) - 1;
%! y = 2 * rand (1000, 1) - 1;
%! z = 2 * rand (1000, 1) - 1;
%! v = x.^2 + y.^2 + z.^2;
%! [xi, yi, zi] = meshgrid (-0.8:0.2:0.8);
%! vi = griddata3 (x, y, z, v, xi, yi, zi, 'linear');
%! vv = vi - xi.^2 - yi.^2 - zi.^2;
%! assert (max (abs (vv(:))), 0, 0.1);

%!testif HAVE_QHULL
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 0);
%! x = 2 * rand (1000, 1) - 1;
%! y = 2 * rand (1000, 1) - 1;
%! z = 2 * rand (1000, 1) - 1;
%! v = x.^2 + y.^2 + z.^2;
%! [xi, yi, zi] = meshgrid (-0.8:0.2:0.8);
%! vi = griddata3 (x, y, z, v, xi, yi, zi, 'nearest');
%! vv = vi - xi.^2 - yi.^2 - zi.^2;
%! assert (max (abs (vv(:))), 0, 0.1)
