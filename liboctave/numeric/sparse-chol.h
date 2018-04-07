/*

Copyright (C) 2016-2018 John W. Eaton
Copyright (C) 2005-2018 David Bateman
Copyright (C) 1998-2005 Andy Adler

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_sparse_chol_h)
#define octave_sparse_chol_h 1

#include "octave-config.h"

#include "CSparse.h"

class RowVector;
class SparseMatrix;
class SparseComplexMatrix;

namespace octave
{
  namespace math
  {
    // If the sparse matrix classes become templated on the element type
    // (i.e., sparse_matrix<double>), then it might be best to make the
    // template parameter of this class also be the element type instead
    // of the matrix type.

    template <typename chol_type>
    class
    sparse_chol
    {
    public:

      sparse_chol (void);

      sparse_chol (const chol_type& a, bool natural, bool force);

      sparse_chol (const chol_type& a, octave_idx_type& info,
                   bool natural, bool force);

      sparse_chol (const chol_type& a, octave_idx_type& info, bool natural);

      sparse_chol (const chol_type& a, octave_idx_type& info);

      sparse_chol (const sparse_chol<chol_type>& a);

      virtual ~sparse_chol (void);

      sparse_chol& operator = (const sparse_chol& a);

      chol_type L (void) const;

      chol_type R (void) const { return L ().hermitian (); }

      octave_idx_type P (void) const;

      RowVector perm (void) const;

      SparseMatrix Q (void) const;

      bool is_positive_definite (void) const;

      double rcond (void) const;

      chol_type inverse (void) const;

    protected:

      typedef typename chol_type::element_type chol_elt;

      class sparse_chol_rep;

    private:

      sparse_chol_rep *rep;
    };

    template <typename chol_type>
    chol_type
    chol2inv (const chol_type& r);

    // SparseComplexMatrix specialization.

    template <>
    sparse_chol<SparseComplexMatrix>::sparse_chol
      (const SparseComplexMatrix& a, octave_idx_type& info);
  }
}

#endif
