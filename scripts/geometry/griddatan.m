########################################################################
##
## Copyright (C) 2007-2020 The Octave Project Developers
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
## @deftypefn  {} {@var{yi} =} griddatan (@var{x}, @var{y}, @var{xi})
## @deftypefnx {} {@var{yi} =} griddatan (@var{x}, @var{y}, @var{xi}, @var{method})
## @deftypefnx {} {@var{yi} =} griddatan (@var{x}, @var{y}, @var{xi}, @var{method}, @var{options})
##
## Generate a regular mesh from irregular data using interpolation.
##
## The function is defined by @code{@var{y} = f (@var{x})}.
## The interpolation points are all @var{xi}.
##
## The interpolation method can be @qcode{"nearest"} or @qcode{"linear"}.
## If method is omitted it defaults to @qcode{"linear"}.
##
## The optional argument @var{options} is passed directly to Qhull when
## computing the Delaunay triangulation used for interpolation.  See
## @code{delaunayn} for information on the defaults and how to pass different
## values.
## @seealso{griddata, griddata3, delaunayn}
## @end deftypefn

function yi = griddatan (x, y, xi, method = "linear", varargin)

  if (nargin < 3)
    print_usage ();
  endif

  if (ischar (method))
    method = tolower (method);
  endif

  [m, n] = size (x);
  [mi, ni] = size (xi);

  if (n != ni || rows (y) != m || columns (y) != 1)
    error ("griddatan: dimensional mismatch");
  endif

  ## triangulate data
  tri = delaunayn (x, varargin{:});

  yi = NaN (mi, 1);

  if (strcmp (method, "nearest"))
    ## search index of nearest point
    idx = dsearchn (x, tri, xi);
    valid = ! isnan (idx);
    yi(valid) = y(idx(valid));

  elseif (strcmp (method, "linear"))
    ## search for every point the enclosing triangle
    [tri_list, bary_list] = tsearchn (x, tri, xi);

    ## only keep the points within triangles.
    valid = ! isnan (tri_list);
    tri_list = tri_list(valid);
    bary_list = bary_list(valid, :);
    nr_t = rows (tri_list);

    ## Use barycentric coordinate of point to calculate yi
    if (isscalar (tri_list))
      ## Special case required by orientation rules for vector/vector index.
      yi(valid) = sum (y(tri(tri_list,:)).' .* bary_list, 2);
    else
      yi(valid) = sum (y(tri(tri_list,:)) .* bary_list, 2);
    endif

  elseif (any (strcmp (method, {"cubic", "v4"})))
    error ('griddata: "%s" METHOD only valid for 2-D inputs using "griddata"', method);

  elseif (strcmp (method, "natural"))
    ## FIXME: implement missing interpolation method 'natural' for 3-D inputs.
    error ('griddatan: "natural" interpolation METHOD not yet implemented');

  else
    error ('griddatan: unknown interpolation METHOD: "%s"', method);
  endif

endfunction


%!testif HAVE_QHULL
%! [xx,yy] = meshgrid (linspace (-1,1,32));
%! xi = [xx(:), yy(:)];
%! x = 2*rand (100,2) - 1;
%! x = [x;1,1;1,-1;-1,-1;-1,1];
%! y = sin (2 * sum (x.^2,2));
%! zz = griddatan (x,y,xi,"linear");
%! zz2 = griddata (x(:,1),x(:,2),y,xi(:,1),xi(:,2),"linear");
%! assert (zz, zz2, 1e-10);

%!testif HAVE_QHULL
%! [xx,yy] = meshgrid (linspace (-1,1,32));
%! xi = [xx(:), yy(:)];
%! x = 2*rand (100,2) - 1;
%! x = [x;1,1;1,-1;-1,-1;-1,1];
%! y = sin (2*sum (x.^2,2));
%! zz = griddatan (x,y,xi,"nearest");
%! zz2 = griddata (x(:,1),x(:,2),y,xi(:,1),xi(:,2),"nearest");
%! assert (zz, zz2, 1e-10);

%!testif HAVE_QHULL <*56515>
%! x = [ 0, 0; 1, 1; 0, 1; 1, 0 ];
%! y = [ 1; 2; 3; 4 ];
%! xi = [ .5, .5 ];
%! yi = griddatan (x, y, xi);

## Test input validation
%!error griddatan ()
%!error griddatan (1)
%!error griddatan (1,2)
%!error griddatan (1,2,3)
%!error <OPTIONS argument must be a string> griddatan (1,2,3,4,5)
%!error <unknown interpolation METHOD> griddatan (1,2,3,4)
%!#error <"v4" METHOD only valid for 2-D inputs>
%! griddatan (ones(2,2,2), 2*ones(2,2,2), 3, "v4")
%!error <"cubic" METHOD only valid for> griddatan (1, 2, 3, "cubic")
%!error <"natural" .* not yet implemented> griddatan (1, 2, 3, "natural")
%!error <unknown interpolation METHOD: "foobar"> griddatan (1, 2, 3, "foobar")
