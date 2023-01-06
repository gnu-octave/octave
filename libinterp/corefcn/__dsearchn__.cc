////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#include <string>

#include "defun.h"
#include "error.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (__dsearchn__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{idx}, @var{d}] =} dsearch (@var{x}, @var{xi})
Undocumented internal function.
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  Matrix x = args(0).matrix_value ().transpose ();
  Matrix xi = args(1).matrix_value ().transpose ();

  if (x.rows () != xi.rows () || x.columns () < 1)
    error ("__dsearchn__: number of rows of X and XI must match");

  octave_idx_type n = x.rows ();
  octave_idx_type nx = x.columns ();
  octave_idx_type nxi = xi.columns ();

  ColumnVector idx (nxi);
  double *pidx = idx.fortran_vec ();
  ColumnVector dist (nxi);
  double *pdist = dist.fortran_vec ();

#define DIST(dd, y, yi, m)                      \
  dd = 0.0;                                     \
  for (octave_idx_type k = 0; k < m; k++)       \
    {                                           \
      double yd = y[k] - yi[k];                 \
      dd += yd * yd;                            \
    }                                           \
  dd = sqrt (dd)

  const double *pxi = xi.data ();
  for (octave_idx_type i = 0; i < nxi; i++)
    {
      double d0;
      const double *px = x.data ();
      DIST(d0, px, pxi, n);
      *pidx = 1.;
      for (octave_idx_type j = 1; j < nx; j++)
        {
          px += n;
          double d;
          DIST (d, px, pxi, n);
          if (d < d0)
            {
              d0 = d;
              *pidx = static_cast<double> (j + 1);
            }
          octave_quit ();
        }

      *pdist++ = d0;
      pidx++;
      pxi += n;
    }

  return ovl (idx, dist);
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

OCTAVE_END_NAMESPACE(octave)
