////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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

#include <cstddef>

#include "CSparse.h"
#include "MatrixType.h"
#include "dRowVector.h"
#include "dSparse.h"
#include "lo-error.h"
#include "oct-cmplx.h"
#include "oct-sparse.h"
#include "oct-spparms.h"
#include "quit.h"
#include "sparse-chol.h"
#include "sparse-util.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

template <typename chol_type>
class sparse_chol<chol_type>::sparse_chol_rep
{
public:

  sparse_chol_rep (void)
    : m_is_pd (false), m_minor_p (0), m_perm (), m_rcond (0)
#if defined (HAVE_CHOLMOD)
    , m_L (nullptr), m_common ()
#endif
  { }

  sparse_chol_rep (const chol_type& a, bool natural, bool force)
    : m_is_pd (false), m_minor_p (0), m_perm (), m_rcond (0)
#if defined (HAVE_CHOLMOD)
    , m_L (nullptr), m_common ()
#endif
  {
    init (a, natural, force);
  }

  sparse_chol_rep (const chol_type& a, octave_idx_type& info,
                   bool natural, bool force)
    : m_is_pd (false), m_minor_p (0), m_perm (), m_rcond (0)
#if defined (HAVE_CHOLMOD)
    , m_L (nullptr), m_common ()
#endif
  {
    info = init (a, natural, force);
  }

  // No copying!

  sparse_chol_rep (const sparse_chol_rep&) = delete;

  sparse_chol_rep& operator = (const sparse_chol_rep&) = delete;

  ~sparse_chol_rep (void)
  {
#if defined (HAVE_CHOLMOD)
    if (m_L)
      CHOLMOD_NAME (free_sparse) (&m_L, &m_common);

    CHOLMOD_NAME(finish) (&m_common);
#endif
  }

#if defined (HAVE_CHOLMOD)
  cholmod_sparse * L (void) const
  {
    return m_L;
  }
#endif

  octave_idx_type P (void) const
  {
#if defined (HAVE_CHOLMOD)
    return (m_minor_p == static_cast<octave_idx_type> (m_L->ncol) ?
            0 : m_minor_p + 1);
#else
    return 0;
#endif
  }

  RowVector perm (void) const { return m_perm + 1; }

  SparseMatrix Q (void) const;

  bool is_positive_definite (void) const { return m_is_pd; }

  double rcond (void) const { return m_rcond; }

private:

  bool m_is_pd;

  octave_idx_type m_minor_p;

  RowVector m_perm;

  double m_rcond;

#if defined (HAVE_CHOLMOD)
  cholmod_sparse *m_L;

  cholmod_common m_common;

  void drop_zeros (const cholmod_sparse *S);
#endif

  octave_idx_type init (const chol_type& a, bool natural, bool force);
};

#if defined (HAVE_CHOLMOD)

// Can't use CHOLMOD_NAME(drop)(0.0, S, cm) because it doesn't treat
// complex matrices.

template <typename chol_type>
void
sparse_chol<chol_type>::sparse_chol_rep::drop_zeros (const cholmod_sparse *S)
{
  if (! S)
    return;

  octave_idx_type *Sp = static_cast<octave_idx_type *>(S->p);
  octave_idx_type *Si = static_cast<octave_idx_type *>(S->i);
  chol_elt *Sx = static_cast<chol_elt *>(S->x);

  octave_idx_type pdest = 0;
  octave_idx_type ncol = S->ncol;

  for (octave_idx_type k = 0; k < ncol; k++)
    {
      octave_idx_type p = Sp[k];
      octave_idx_type pend = Sp[k+1];
      Sp[k] = pdest;

      for (; p < pend; p++)
        {
          chol_elt sik = Sx[p];

          if (CHOLMOD_IS_NONZERO (sik))
            {
              if (p != pdest)
                {
                  Si[pdest] = Si[p];
                  Sx[pdest] = sik;
                }

              pdest++;
            }
        }
    }

  Sp[ncol] = pdest;
}

// Must provide a specialization for this function.
template <typename T>
int
get_xtype (void);

template <>
inline int
get_xtype<double> (void)
{
  return CHOLMOD_REAL;
}

template <>
inline int
get_xtype<Complex> (void)
{
  return CHOLMOD_COMPLEX;
}

#endif

template <typename chol_type>
octave_idx_type
sparse_chol<chol_type>::sparse_chol_rep::init (const chol_type& a,
    bool natural, bool force)
{
  volatile octave_idx_type info = 0;

#if defined (HAVE_CHOLMOD)

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    (*current_liboctave_error_handler)
      ("sparse_chol requires square matrix");

  cholmod_common *cm = &m_common;

  // Setup initial parameters

  CHOLMOD_NAME(start) (cm);
  cm->prefer_zomplex = false;

  double spu = sparse_params::get_key ("spumoni");

  if (spu == 0.)
    {
      cm->print = -1;
      SUITESPARSE_ASSIGN_FPTR (printf_func, cm->print_function, nullptr);
    }
  else
    {
      cm->print = static_cast<int> (spu) + 2;
      SUITESPARSE_ASSIGN_FPTR (printf_func, cm->print_function,
                               &SparseCholPrint);
    }

  cm->error_handler = &SparseCholError;

  SUITESPARSE_ASSIGN_FPTR2 (divcomplex_func, cm->complex_divide,
                            divcomplex);

  SUITESPARSE_ASSIGN_FPTR2 (hypot_func, cm->hypotenuse, hypot);

  cm->final_asis = false;
  cm->final_super = false;
  cm->final_ll = true;
  cm->final_pack = true;
  cm->final_monotonic = true;
  cm->final_resymbol = false;

  cholmod_sparse A;
  cholmod_sparse *ac = &A;
  double dummy;

  ac->nrow = a_nr;
  ac->ncol = a_nc;

  ac->p = a.cidx ();
  ac->i = a.ridx ();
  ac->nzmax = a.nnz ();
  ac->packed = true;
  ac->sorted = true;
  ac->nz = nullptr;
#if defined (OCTAVE_ENABLE_64)
  ac->itype = CHOLMOD_LONG;
#else
  ac->itype = CHOLMOD_INT;
#endif
  ac->dtype = CHOLMOD_DOUBLE;
  ac->stype = 1;
  ac->xtype = get_xtype<chol_elt> ();

  if (a_nr < 1)
    ac->x = &dummy;
  else
    ac->x = a.data ();

  // use natural ordering if no q output parameter
  if (natural)
    {
      cm->nmethods = 1;
      cm->method[0].ordering = CHOLMOD_NATURAL;
      cm->postorder = false;
    }

  cholmod_factor *Lfactor = CHOLMOD_NAME(analyze) (ac, cm);
  CHOLMOD_NAME(factorize) (ac, Lfactor, cm);

  m_is_pd = cm->status == CHOLMOD_OK;
  info = (m_is_pd ? 0 : cm->status);

  if (m_is_pd || force)
    {
      m_rcond = CHOLMOD_NAME(rcond) (Lfactor, cm);

      m_minor_p = Lfactor->minor;

      m_L = CHOLMOD_NAME(factor_to_sparse) (Lfactor, cm);

      if (m_minor_p > 0 && m_minor_p < a_nr)
        {
          std::size_t n1 = a_nr + 1;
          m_L->p = CHOLMOD_NAME(realloc) (m_minor_p+1,
                                          sizeof(octave_idx_type),
                                          m_L->p, &n1, cm);

          CHOLMOD_NAME(reallocate_sparse)
          (static_cast<octave_idx_type *>(m_L->p)[m_minor_p],
           m_L, cm);

          m_L->ncol = m_minor_p;
        }

      drop_zeros (m_L);

      if (! natural)
        {
          m_perm.resize (a_nr);
          for (octave_idx_type i = 0; i < a_nr; i++)
            m_perm(i) = static_cast<octave_idx_type *>(Lfactor->Perm)[i];
        }
    }

  // NAME used to prefix statistics report from print_common
  static char blank_name[] = " ";

  CHOLMOD_NAME(print_common) (blank_name, cm);
  CHOLMOD_NAME(free_factor) (&Lfactor, cm);

  return info;

#else

  octave_unused_parameter (a);
  octave_unused_parameter (natural);
  octave_unused_parameter (force);

  (*current_liboctave_error_handler)
    ("support for CHOLMOD was unavailable or disabled when liboctave was built");

  return info;

#endif
}

template <typename chol_type>
SparseMatrix
sparse_chol<chol_type>::sparse_chol_rep::Q (void) const
{
#if defined (HAVE_CHOLMOD)

  octave_idx_type n = m_L->nrow;
  SparseMatrix p (n, n, n);

  for (octave_idx_type i = 0; i < n; i++)
    {
      p.xcidx (i) = i;
      p.xridx (i) = static_cast<octave_idx_type> (m_perm (i));
      p.xdata (i) = 1;
    }

  p.xcidx (n) = n;

  return p;

#else

  return SparseMatrix ();

#endif
}

template <typename chol_type>
sparse_chol<chol_type>::sparse_chol (void)
  : m_rep (new typename sparse_chol<chol_type>::sparse_chol_rep ())
{ }

template <typename chol_type>
sparse_chol<chol_type>::sparse_chol (const chol_type& a, bool natural,
                                     bool force)
  : m_rep (new typename
           sparse_chol<chol_type>::sparse_chol_rep (a, natural, force))
{ }

template <typename chol_type>
sparse_chol<chol_type>::sparse_chol (const chol_type& a,
                                     octave_idx_type& info,
                                     bool natural, bool force)
  : m_rep (new typename
           sparse_chol<chol_type>::sparse_chol_rep (a, info, natural, force))
{ }

template <typename chol_type>
sparse_chol<chol_type>::sparse_chol (const chol_type& a,
                                     octave_idx_type& info,
                                     bool natural)
  : m_rep (new typename
           sparse_chol<chol_type>::sparse_chol_rep (a, info, natural, false))
{ }

template <typename chol_type>
sparse_chol<chol_type>::sparse_chol (const chol_type& a,
                                     octave_idx_type& info)
  : m_rep (new typename
           sparse_chol<chol_type>::sparse_chol_rep (a, info, false, false))
{ }

template <typename chol_type>
chol_type
sparse_chol<chol_type>::L (void) const
{
#if defined (HAVE_CHOLMOD)

  cholmod_sparse *m = m_rep->L ();

  octave_idx_type nc = m->ncol;
  octave_idx_type nnz = m->nzmax;

  chol_type ret (m->nrow, nc, nnz);

  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = static_cast<octave_idx_type *>(m->p)[j];

  for (octave_idx_type i = 0; i < nnz; i++)
    {
      ret.xridx (i) = static_cast<octave_idx_type *>(m->i)[i];
      ret.xdata (i) = static_cast<chol_elt *>(m->x)[i];
    }

  return ret;

#else

  return chol_type ();

#endif
}

template <typename chol_type>
octave_idx_type
sparse_chol<chol_type>::P (void) const
{
  return m_rep->P ();
}

template <typename chol_type>
RowVector
sparse_chol<chol_type>::perm (void) const
{
  return m_rep->perm ();
}

template <typename chol_type>
SparseMatrix
sparse_chol<chol_type>::Q (void) const
{
  return m_rep->Q ();
}

template <typename chol_type>
bool
sparse_chol<chol_type>::is_positive_definite (void) const
{
  return m_rep->is_positive_definite ();
}

template <typename chol_type>
double
sparse_chol<chol_type>::rcond (void) const
{
  return m_rep->rcond ();
}

template <typename chol_type>
chol_type
sparse_chol<chol_type>::inverse (void) const
{
  chol_type retval;

#if defined (HAVE_CHOLMOD)

  cholmod_sparse *m = m_rep->L ();
  octave_idx_type n = m->ncol;
  RowVector m_perm = m_rep->perm ();
  double rcond2;
  octave_idx_type info;
  MatrixType mattype (MatrixType::Upper);
  chol_type linv = L ().hermitian ().inverse (mattype, info, rcond2, 1, 0);

  if (m_perm.numel () == n)
    {
      SparseMatrix Qc = Q ();

      retval = Qc * linv * linv.hermitian () * Qc.transpose ();
    }
  else
    retval = linv * linv.hermitian ();

#endif

  return retval;
}

template <typename chol_type>
chol_type
chol2inv (const chol_type& r)
{
  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();
  chol_type retval;

  if (r_nr != r_nc)
    (*current_liboctave_error_handler) ("U must be a square matrix");

  MatrixType mattype (r);
  int typ = mattype.type (false);
  double rcond;
  octave_idx_type info;
  chol_type rtra, multip;

  if (typ == MatrixType::Upper)
    {
      rtra = r.transpose ();
      multip = (rtra*r);
    }
  else if (typ == MatrixType::Lower)
    {
      rtra = r.transpose ();
      multip = (r*rtra);
    }
  else
    (*current_liboctave_error_handler) ("U must be a triangular matrix");

  MatrixType mattypenew (multip);
  retval = multip.inverse (mattypenew, info, rcond, true, false);
  return retval;
}

// SparseComplexMatrix specialization (the value for the NATURAL
// parameter in the sparse_chol<T>::sparse_chol_rep constructor is
// different from the default).

template <>
OCTAVE_API
sparse_chol<SparseComplexMatrix>::sparse_chol (const SparseComplexMatrix& a,
    octave_idx_type& info)
  : m_rep (new sparse_chol<SparseComplexMatrix>::sparse_chol_rep (a, info,
           true,
           false))
{ }

// Instantiations we need.

template class OCTAVE_API sparse_chol<SparseMatrix>;

template class sparse_chol<SparseComplexMatrix>;

template OCTAVE_API SparseMatrix
chol2inv<SparseMatrix> (const SparseMatrix& r);

template OCTAVE_API SparseComplexMatrix
chol2inv<SparseComplexMatrix> (const SparseComplexMatrix& r);

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
