////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022-2023 The Octave Project Developers
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

#include "lo-array-errwarn.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"

// FIXME: According to cppreference.com the implementation of `ldexp (f, e)`
// might be less efficient that the corresponding `f * exp2 (e)`.  Consider
// replacing our implementation with the latter.

template <typename T>
void
map_2_xldexp (Array<T>& y, const Array<T>& f, const Array<T>& e)
{
  if (f.numel () == e.numel () || e.numel () == 1)
    y = Array<T> (f.dims ());
  else if (f.numel () == 1)
    y = Array<T> (e.dims ());
  else
    octave::err_nonconformant ("pow2", f.dims (), e.dims ());

  octave_idx_type f_inc = (f.numel () == 1) ? 0 : 1;
  octave_idx_type e_inc = (e.numel () == 1) ? 0 : 1;

  for (octave_idx_type i = 0; i < y.numel (); i++)
    y.xelem (i) = std::ldexp (f.xelem (i * f_inc),
                              static_cast<int> (e.xelem (i * e_inc)));
}

void
map_2_xldexp_sparse (SparseMatrix& y, const SparseMatrix& f,
                     const SparseMatrix& e)
{
  if (e.numel () == 1)
    {
      int ee = static_cast<int> (e.data (0));
      for (octave_idx_type i = 0; i < y.nnz (); i++)
        y.data (i) = std::ldexp (f.data (i), ee);
    }
  else if (f.numel () == e.numel ())
    {
      octave_idx_type col = 1;
      for (octave_idx_type i = 0; i < y.nnz (); i++)
        {
          // Determine current column.
          while (i >= f.cidx (col))
            col++;
          int ee = static_cast<int> (e.xelem (f.ridx (i), col - 1));
          y.data (i) = std::ldexp (f.data (i), ee);
        }
    }
  else
    octave::err_nonconformant ("pow2", f.dims (), e.dims ());
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (pow2, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} pow2 (@var{x})
@deftypefnx {} {@var{y} =} pow2 (@var{f}, @var{e})
With one input argument, compute
@tex
$y = 2^x$
@end tex
@ifnottex
y = 2 .^ x
@end ifnottex
for each element of @var{x}.

With two input arguments, return
@tex
$y = f \cdot 2^e$,
@end tex
@ifnottex
y = f .* (2 .^ e).
@end ifnottex
where for complex inputs only the real part of both inputs is regarded
and from @var{e} only the real integer part.  This calling form corresponds
to C/C++ standard function @code{ldexp()}.
@seealso{log2, nextpow2, power}
@end deftypefn */)
{
  if (args.length () < 1 || args.length () > 2)
    print_usage ();

  if (! args(0).isfloat ())
    err_wrong_type_arg ("pow2", args(0));

  // Call exp2(f) where possible for numerical more accurate results.
  if (args.length () == 1)
    {
      if (args(0).iscomplex ())
        {
          // The C++ standard does not define exp2 for complex arguments.
          // Therefore call `2.^x`.
          octave_value retval = octave::binary_op (octave_value::op_el_pow,
                                2, args(0));

          // Preserve sparse datatype, but even for sparse input fill-up
          // is unavoidable `2^0 == 1` thus cast only.
          if (args(0).issparse ())
            retval = octave_value (retval.sparse_complex_matrix_value ());

          return ovl (retval);
        }
      else if (args(0).is_single_type ())
        {
          FloatNDArray x = args(0).float_array_value ();
          FloatNDArray y (x.dims ());
          for (octave_idx_type i = 0; i < y.numel (); i++)
            y.xelem (i) = std::exp2 (x.xelem (i));
          return ovl (y);
        }
      else
        {
          NDArray x = args(0).array_value ();
          NDArray y (x.dims ());
          for (octave_idx_type i = 0; i < y.numel (); i++)
            y.xelem (i) = std::exp2 (x.xelem (i));

          // Preserve sparse datatype, but even for sparse input fill-up
          // is unavoidable `2^0 == 1` thus cast only.
          if (args(0).issparse ())
            return ovl (SparseMatrix (y));
          else
            return ovl (y);
        }
    }

  // For Matlab compatibility, the two argument call `y = pow2 (f, e)`
  // corresponds to std::ldexp() (see bug #61968).  The resulting y is
  // computed quickly by adding the integer part of e to the floating-point
  // exponent of f.

  if (! args(1).isfloat ())
    err_wrong_type_arg ("pow2", args(1));

  if (args(0).iscomplex () || args(1).iscomplex ())
    warning_with_id ("Octave:pow2:imaginary-ignored",
                     "pow2: imaginary part is ignored");

  // Note: Matlab R2021a errors on `pow2 (sparse (f), single (e))`,
  //       but sparsity in f determines output and can significantly
  //       reduce computation, e.g. `N=1e5; pow2(speye(N),sparse(N,N))`.
  if (args(0).issparse ())
    {
      SparseMatrix f = args(0).sparse_matrix_value ();

      // Special case: return a sparse zero matrix in size of e.
      if ((f.numel () == 1) && (f.nnz () == 0))
        return ovl (SparseMatrix (args(1).rows (), args(1).columns ()));

      // Only do sparse computation, if it pays off.  For scalar f fill-up
      // is unavoidable even for sparse e because `f * 2^0 == f`.  Use dense
      // code below in this case.
      if (f.numel () > 1)
        {
          SparseMatrix e = args(1).sparse_matrix_value ();
          SparseMatrix y = SparseMatrix (f);
          map_2_xldexp_sparse (y, f, e);
          return ovl (y);
        }
    }

  if (args(0).is_single_type () || args(1).is_single_type ())
    {
      FloatNDArray f = args(0).float_array_value ();
      FloatNDArray e = args(1).float_array_value ();
      FloatNDArray y;
      map_2_xldexp (y, f, e);
      return ovl (y);
    }
  else
    {
      NDArray f = args(0).array_value ();
      NDArray e = args(1).array_value ();
      NDArray y;
      map_2_xldexp (y, f, e);

      // Preserve sparse datatype.
      // Cases for efficient use of sparsity were treated above already.
      if (args(0).issparse ())
        return ovl (SparseMatrix (y));
      else
        return ovl (y);
    }
}

/*
## Call `y = pow2 (x)`

%!test
%! fcns = {@double, @single, @complex};
%! x = [3, 0, -3];
%! v = [8, 1, .125];
%! for i = 1:numel (fcns)
%!   fcn = fcns{i};
%!   assert (pow2 (fcn (x)), fcn (v), sqrt (eps));
%! endfor

%!test
%! fcns = {@double, @single, @complex, @sparse};
%! x = [3, 1, -3];
%! v = [8, 2, .125];
%! for i = 1:numel (fcns)
%!   fcn = fcns{i};
%!   assert (pow2 (fcn (x)), fcn (v), sqrt (eps));
%! endfor

%!test
%! fcns = {@double, @single, @complex, @sparse};
%! x = [1, 1+1i, 1i];
%! for i = 1:numel (fcns)
%!   fcn = fcns{i};
%!   assert (pow2 (fcn (x)), fcn (2) .^ fcn (x), sqrt (eps));
%! endfor

## Call `y = pow2 (f, e)`

%!test
%! fcns = {@double, @single, @complex, @sparse};
%! f = [2 2];
%! e = [2 2];
%! z = [8 8];
%! warning ("off", "Octave:pow2:imaginary-ignored", "local");
%! for i = 1:numel (fcns)
%!   fcn = fcns{i};
%!   assert (pow2 (fcn (f), fcn (e)), real (fcn (z)));
%! endfor

## Only integer part is taken into account.
%!test
%! f = 2;
%! e = [2, 2.1, 2.2, 2.4, 2.5, 2.8];
%! z = 8 .* ones (1, length (e));
%! assert (pow2 (f, e), z);

## Only real part is taken into account.
%!test
%! f = [1+1i, 1];
%! e = 2;
%! z = [4, 4];
%! warning ("off", "Octave:pow2:imaginary-ignored", "local");
%! assert (pow2 (f, e), z);

%!test
%! f = 1;
%! e = [1+1i, 1];
%! z = [2, 2];
%! warning ("off", "Octave:pow2:imaginary-ignored", "local");
%! assert (pow2 (f, e), z);

%!test
%! f = [1/2, pi/4, -3/4, 1/2, 1-eps()/2, 1/2];
%! e = [1, 2, 2, -51, 1024, -1021];
%! z = [1, pi, -3, eps(), realmax(), realmin()];
%! assert (pow2 (f, e), z);

## Tests for sparsity.
%!assert (pow2 (sparse (0), ones  (3)), sparse (3, 3));
%!assert (pow2 (sparse (1), ones  (3)), 2 .* sparse (ones (3)));
%!assert (pow2 (sparse (1), speye (3)), sparse (ones (3) + eye (3)));
%!assert (pow2 (sparse (3, 3), ones (3)), sparse (3, 3));
%!assert (pow2 (speye (3), ones (3)), 2 .* speye (3));
%!assert (pow2 (speye (3), 1),        2 .* speye (3));

%!test
%! f = speye (3);
%! e = sparse (3, 3);
%! e(1,1) = 1;
%! e(1,3) = 1;
%! z = f;
%! z(1,1) = 2;
%! assert (pow2 (f, e), z);

## Large sparse matrix (only few real elements).
%!test
%! ## FIXME: `N = 1e5` would be a better test, but `assert` fills-up somehow.
%! N = 1e3;
%! assert (pow2 (speye  (N), sparse (N,N)), speye (N));
%! assert (pow2 (sparse (0), speye  (N)),   sparse(N,N));

%!error <Invalid call> pow2 ()
%!error <Invalid call> pow2 (1,2,3)
%!error <wrong type argument> pow2 (int8 (1))
%!error <wrong type argument> pow2 (2, int8 (1))
%!warning <imaginary part is ignored> pow2 (i, 2);
%!warning <imaginary part is ignored> pow2 (2, i);
%!error <pow2: nonconformant arguments> pow2 ([1,2], [3,4,5])
%!error <pow2: nonconformant arguments> pow2 (sparse ([1,2]), sparse ([3,4,5]))
*/

OCTAVE_END_NAMESPACE(octave)
