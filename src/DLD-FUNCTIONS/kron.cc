/*

Copyright (C) 2002, 2005, 2006, 2007, 2008 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Author: Paul Kienzle <pkienzle@users.sf.net>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dMatrix.h"
#include "CMatrix.h"
#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern void
kron (const Array<double>&, const Array<double>&, Array<double>&);

extern void
kron (const Array<Complex>&, const Array<Complex>&, Array<Complex>&);

extern void
kron (const Array<float>&, const Array<float>&, Array<float>&);

extern void
kron (const Array<FlaotComplex>&, const Array<FloatComplex>&, 
      Array<FloatComplex>&);
#endif

template <class T>
void
kron (const Array<T>& A, const Array<T>& B, Array<T>& C)
{
  C.resize (A.rows () * B.rows (), A.columns () * B.columns ());

  octave_idx_type Ac, Ar, Cc, Cr;

  for (Ac = Cc = 0; Ac < A.columns (); Ac++, Cc += B.columns ())
    for (Ar = Cr = 0; Ar < A.rows (); Ar++, Cr += B.rows ())
      {
        const T v = A (Ar, Ac);
        for (octave_idx_type Bc = 0; Bc < B.columns (); Bc++)
          for (octave_idx_type Br = 0; Br < B.rows (); Br++)
            {
              OCTAVE_QUIT;
              C.xelem (Cr+Br, Cc+Bc) = v * B.elem (Br, Bc);
            }
      }
}

template void
kron (const Array<double>&, const Array<double>&, Array<double>&);

template void
kron (const Array<Complex>&, const Array<Complex>&, Array<Complex>&);

template void
kron (const Array<float>&, const Array<float>&, Array<float>&);

template void
kron (const Array<FloatComplex>&, const Array<FloatComplex>&, 
      Array<FloatComplex>&);

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern void
kron (const Sparse<double>&, const Sparse<double>&, Sparse<double>&);

extern void
kron (const Sparse<Complex>&, const Sparse<Complex>&, Sparse<Complex>&);
#endif

template <class T>
void
kron (const Sparse<T>& A, const Sparse<T>& B, Sparse<T>& C)
{
  octave_idx_type idx = 0;
  C = Sparse<T> (A.rows () * B.rows (), A.columns () * B.columns (), 
                 A.nzmax () * B.nzmax ());

  C.cidx (0) = 0;

  for (octave_idx_type Aj = 0; Aj < A.columns (); Aj++)
    for (octave_idx_type Bj = 0; Bj < B.columns (); Bj++)
      {
        for (octave_idx_type Ai = A.cidx (Aj); Ai < A.cidx (Aj+1); Ai++)
          {
            octave_idx_type Ci = A.ridx(Ai) * B.rows ();
            const T v = A.data (Ai);

            for (octave_idx_type Bi = B.cidx (Bj); Bi < B.cidx (Bj+1); Bi++)
              {
                OCTAVE_QUIT;
                C.data (idx) = v * B.data (Bi);
                C.ridx (idx++) = Ci + B.ridx (Bi);
              }
          }
        C.cidx (Aj * B.columns () + Bj + 1) = idx;
      }
}

template void
kron (const Sparse<double>&, const Sparse<double>&, Sparse<double>&);

template void
kron (const Sparse<Complex>&, const Sparse<Complex>&, Sparse<Complex>&);


DEFUN_DLD (kron, args, nargout, "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} kron (@var{a}, @var{b})\n\
Form the kronecker product of two matrices, defined block by block as\n\
\n\
@example\n\
x = [a(i, j) b]\n\
@end example\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
kron (1:4, ones (3, 1))\n\
      @result{}  1  2  3  4\n\
          1  2  3  4\n\
          1  2  3  4\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 2 || nargout > 1)
    {
      print_usage ();
    }
  else if (args(0).is_sparse_type () || args(1).is_sparse_type ())
    {
      if (args(0).is_complex_type () || args(1).is_complex_type ())
        {
          SparseComplexMatrix a (args(0).sparse_complex_matrix_value());
          SparseComplexMatrix b (args(1).sparse_complex_matrix_value());

          if (! error_state)
            {
              SparseComplexMatrix c;
              kron (a, b, c);
              retval(0) = c;
            }
        }
      else
        {
          SparseMatrix a (args(0).sparse_matrix_value ());
          SparseMatrix b (args(1).sparse_matrix_value ());

          if (! error_state)
            {
              SparseMatrix c;
              kron (a, b, c);
              retval (0) = c;
            }
        }
    }
  else 
    {
      if (args(0).is_single_type () || args(1).is_single_type ())
        {
          if (args(0).is_complex_type () || args(1).is_complex_type ())
            {
              FloatComplexMatrix a (args(0).float_complex_matrix_value());
              FloatComplexMatrix b (args(1).float_complex_matrix_value());

              if (! error_state)
                {
                  FloatComplexMatrix c;
                  kron (a, b, c);
                  retval(0) = c;
                }
            }
          else
            {
              FloatMatrix a (args(0).float_matrix_value ());
              FloatMatrix b (args(1).float_matrix_value ());

              if (! error_state)
                {
                  FloatMatrix c;
                  kron (a, b, c);
                  retval (0) = c;
                }
            }
        }
      else
        {
          if (args(0).is_complex_type () || args(1).is_complex_type ())
            {
              ComplexMatrix a (args(0).complex_matrix_value());
              ComplexMatrix b (args(1).complex_matrix_value());

              if (! error_state)
                {
                  ComplexMatrix c;
                  kron (a, b, c);
                  retval(0) = c;
                }
            }
          else
            {
              Matrix a (args(0).matrix_value ());
              Matrix b (args(1).matrix_value ());

              if (! error_state)
                {
                  Matrix c;
                  kron (a, b, c);
                  retval (0) = c;
                }
            }
        }
    }

  return retval;
}
