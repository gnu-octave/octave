/*

Copyright (C) 2016 John W. Eaton
Copyright (C) 2004-2016 David Bateman
Copyright (C) 1998-2004 Andy Adler

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_sparse_lu_h)
#define octave_sparse_lu_h 1

#include "octave-config.h"

#include "MArray.h"
#include "dSparse.h"

namespace octave
{
  namespace math
  {
    // If the sparse matrix classes become templated on the element type
    // (i.e., sparse_matrix<double>), then it might be best to make the
    // template parameter of this class also be the element type instead
    // of the matrix type.

    template <typename lu_type>
    class
    sparse_lu
    {
    public:

      typedef typename lu_type::element_type lu_elt_type;

      sparse_lu (void)
        : Lfact (), Ufact (), Rfact (), cond (0), P (), Q () { }

      sparse_lu (const lu_type& a, const Matrix& piv_thres = Matrix (),
                 bool scale = false);

      sparse_lu (const lu_type& a, const ColumnVector& Qinit,
                 const Matrix& piv_thres, bool scale = false,
                 bool FixedQ = false, double droptol = -1.0,
                 bool milu = false, bool udiag = false);

      sparse_lu (const sparse_lu& a)
        : Lfact (a.Lfact), Ufact (a.Ufact), Rfact (), cond (a.cond),
          P (a.P), Q (a.Q)
      { }

      sparse_lu& operator = (const sparse_lu& a)
      {
        if (this != &a)
          {
            Lfact = a.Lfact;
            Ufact = a.Ufact;
            cond = a.cond;
            P = a.P;
            Q = a.Q;
          }

        return *this;
      }

      virtual ~sparse_lu (void) = default;

      lu_type L (void) const { return Lfact; }

      lu_type U (void) const { return Ufact; }

      SparseMatrix R (void) const { return Rfact; }

      lu_type Y (void) const;

      SparseMatrix Pc (void) const;

      SparseMatrix Pr (void) const;

      ColumnVector Pc_vec (void) const;

      ColumnVector Pr_vec (void) const;

      PermMatrix Pc_mat (void) const;

      PermMatrix Pr_mat (void) const;

      const octave_idx_type * row_perm (void) const { return P.fortran_vec (); }

      const octave_idx_type * col_perm (void) const { return Q.fortran_vec (); }

      double rcond (void) const { return cond; }

    protected:

      lu_type Lfact;
      lu_type Ufact;
      SparseMatrix Rfact;

      double cond;

      MArray<octave_idx_type> P;
      MArray<octave_idx_type> Q;
    };
  }
}

#endif

