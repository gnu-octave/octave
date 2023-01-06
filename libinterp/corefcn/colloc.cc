////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <algorithm>
#include <string>

#include "CollocWt.h"
#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "ovl.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (colloc, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{r}, @var{amat}, @var{bmat}, @var{q}] =} colloc (@var{n}, "left", "right")
Compute derivative and integral weight matrices for orthogonal collocation.

Reference: @nospell{J. Villadsen}, @nospell{M. L. Michelsen},
@cite{Solution of Differential Equation Models by Polynomial Approximation}.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  if (! args(0).is_scalar_type ())
    error ("colloc: N must be a scalar");

  double tmp = args(0).double_value ();
  if (math::isnan (tmp))
    error ("colloc: N cannot be NaN");

  octave_idx_type ncol = math::nint_big (tmp);
  if (ncol < 0)
    error ("colloc: N must be positive");

  octave_idx_type ntot = ncol;
  octave_idx_type left = 0;
  octave_idx_type right = 0;

  for (int i = 1; i < nargin; i++)
    {
      std::string s = args(i).xstring_value ("colloc: optional arguments must be strings");

      std::transform (s.begin (), s.end (), s.begin (), ::tolower);

      if (s == "r" || s == "right")
        right = 1;
      else if (s == "l" || s == "left")
        left = 1;
      else
        error (R"(colloc: string argument must be "left" or "right")");
    }

  ntot += left + right;
  if (ntot < 1)
    error (R"("colloc: the total number of roots (N + "left" + "right") must be >= 1)");

  CollocWt wts (ncol, left, right);

  ColumnVector r = wts.roots ();
  Matrix A = wts.first ();
  Matrix B = wts.second ();
  ColumnVector q = wts.quad_weights ();

  return ovl (r, A, B, q);
}

/*

%!assert (colloc (1), 0.5)
%!assert (colloc (1, "left"), [0; 0.5])
%!assert (colloc (1, "right"), [0.5; 1])
%!assert (colloc (1, "left", "right"), [0; 0.5; 1])

## Test input validation
%!error colloc ()
%!error colloc (1,2,3,4)
%!error <N must be a scalar> colloc (ones (2,2))
%!error <N cannot be NaN> colloc (NaN)
%!error <N must be positive> colloc (-1)
%!error <optional arguments must be strings> colloc (1, 1)
%!error <string argument must be "left" or "right"> colloc (1, "foobar")
%!error <total number of roots .* must be .= 1> colloc (0)

*/

OCTAVE_END_NAMESPACE(octave)
