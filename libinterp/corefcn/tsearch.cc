////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2002-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>

#include "lo-ieee.h"

#include "defun.h"
#include "error.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

inline double max (double a, double b, double c)
{
  return (a > b) ? (a > c ? a : c) : (b > c ? b : c);
}

inline double min (double a, double b, double c)
{
  return (a < b) ? (a < c ? a : c) : (b < c ? b : c);
}

#define REF(x,k,i) x(static_cast<octave_idx_type> (elem((k), (i))) - 1)

// for large data set the algorithm is very slow
// one should presort (how?) either the elements of the points of evaluation
// to cut down the time needed to decide which triangle contains the
// given point

// e.g., build up a neighbouring triangle structure and use a simplex-like
// method to traverse it

DEFUN (tsearch, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{idx} =} tsearch (@var{x}, @var{y}, @var{t}, @var{xi}, @var{yi})
Search for the enclosing Delaunay convex hull.

For @code{@var{t} = delaunay (@var{x}, @var{y})}, finds the index in @var{t}
containing the points @code{(@var{xi}, @var{yi})}.  For points outside the
convex hull, @var{idx} is NaN.
@seealso{delaunay, delaunayn}
@end deftypefn */)
{
  if (args.length () != 5)
    print_usage ();

  const double eps = 1.0e-12;

  const ColumnVector x (args(0).vector_value ());
  const ColumnVector y (args(1).vector_value ());
  const Matrix elem (args(2).matrix_value ());
  const ColumnVector xi (args(3).vector_value ());
  const ColumnVector yi (args(4).vector_value ());

  const octave_idx_type nelem = elem.rows ();

  ColumnVector minx (nelem);
  ColumnVector maxx (nelem);
  ColumnVector miny (nelem);
  ColumnVector maxy (nelem);
  for (octave_idx_type k = 0; k < nelem; k++)
    {
      minx(k) = min (REF (x, k, 0), REF (x, k, 1), REF (x, k, 2)) - eps;
      maxx(k) = max (REF (x, k, 0), REF (x, k, 1), REF (x, k, 2)) + eps;
      miny(k) = min (REF (y, k, 0), REF (y, k, 1), REF (y, k, 2)) - eps;
      maxy(k) = max (REF (y, k, 0), REF (y, k, 1), REF (y, k, 2)) + eps;
    }

  const octave_idx_type np = xi.numel ();
  ColumnVector values (np);

  double x0 = 0.0, y0 = 0.0;
  double a11 = 0.0, a12 = 0.0, a21 = 0.0, a22 = 0.0, det = 0.0;
  double xt = 0.0, yt = 0.0;
  double dx1 = 0.0, dx2 = 0.0, c1 = 0.0, c2 = 0.0;

  octave_idx_type k = nelem;   // k is more than just an index variable.

  for (octave_idx_type kp = 0; kp < np; kp++)   // for each point
    {
      xt = xi (kp);
      yt = yi (kp);

      // Check if point (xt,yt) is in the triangle that was last examined.
      // This is for inputs where points are in contiguous order,
      // like when the points are sampled from a continuous path.
      if (k < nelem)  // This check will be false for the very first point.
        {
          // If we are here, then x0, y0, det all exist from before.
          dx1 = xt - x0;
          dx2 = yt - y0;
          c1 = (a22 * dx1 - a21 * dx2) / det;
          c2 = (-a12 * dx1 + a11 * dx2) / det;
          if (c1 >= -eps && c2 >= -eps && (c1 + c2) <= 1 + eps)
            {
              values (kp) = k+1;
              continue;
            }
        }

      // The point is not in the same triangle, so go through all triangles.
      for (k = 0; k < nelem; k++)
        {
          if (xt >= minx(k) && xt <= maxx(k) && yt >= miny(k) && yt <= maxy(k))
            {
              // Point is inside the triangle's bounding rectangle:
              // See if it's inside the triangle itself.
              x0  = REF (x, k, 0);
              y0  = REF (y, k, 0);
              a11 = REF (x, k, 1) - x0;
              a12 = REF (y, k, 1) - y0;
              a21 = REF (x, k, 2) - x0;
              a22 = REF (y, k, 2) - y0;
              det = a11 * a22 - a21 * a12;

              // solve the system
              dx1 = xt - x0;
              dx2 = yt - y0;
              c1 = (a22 * dx1 - a21 * dx2) / det;
              c2 = (-a12 * dx1 + a11 * dx2) / det;
              if (c1 >= -eps && c2 >= -eps && (c1 + c2) <= 1 + eps)
                {
                  values (kp) = k+1;
                  break;
                }
            } //end see if it's inside the triangle itself
        } //end for each triangle

      if (k == nelem)
        values (kp) = lo_ieee_nan_value ();

    } //end for each point

  return ovl (values);
}

/*
%!shared x, y, tri
%! x = [-1;-1;1];
%! y = [-1;1;-1];
%! tri = [1, 2, 3];
%!assert (tsearch (x,y,tri,-1,-1), 1)
%!assert (tsearch (x,y,tri, 1,-1), 1)
%!assert (tsearch (x,y,tri,-1, 1), 1)
%!assert (tsearch (x,y,tri,-1/3, -1/3), 1)
%!assert (tsearch (x,y,tri, 1, 1), NaN)

%!error tsearch ()
*/

OCTAVE_END_NAMESPACE(octave)
