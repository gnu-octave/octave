////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2023 The Octave Project Developers
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

#include <cinttypes>
#include <vector>

#include "MatrixType.h"
#include "dMatrix.h"
#include "fMatrix.h"
#include "CMatrix.h"
#include "fCMatrix.h"
#include "dSparse.h"
#include "CSparse.h"
#include "oct-spparms.h"
#include "oct-locbuf.h"

static void
warn_cached (void)
{
  (*current_liboctave_warning_with_id_handler)
    ("Octave:matrix-type-info", "using cached matrix type");
}

static void
warn_invalid (void)
{
  (*current_liboctave_warning_with_id_handler)
    ("Octave:matrix-type-info", "invalid matrix type");
}

static void
warn_calculating_sparse_type (void)
{
  (*current_liboctave_warning_with_id_handler)
    ("Octave:matrix-type-info", "calculating sparse matrix type");
}

// FIXME: There is a large code duplication here

MatrixType::MatrixType (void)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (octave::sparse_params::get_bandden ()),
    m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (false), m_nperm (0), m_perm (nullptr) { }

MatrixType::MatrixType (const MatrixType& a)
  : m_type (a.m_type), m_sp_bandden (a.m_sp_bandden), m_bandden (a.m_bandden),
    m_upper_band (a.m_upper_band), m_lower_band (a.m_lower_band),
    m_dense (a.m_dense), m_full (a.m_full),
    m_nperm (a.m_nperm), m_perm (nullptr)
{
  if (m_nperm != 0)
    {
      m_perm = new octave_idx_type [m_nperm];
      for (octave_idx_type i = 0; i < m_nperm; i++)
        m_perm[i] = a.m_perm[i];
    }
}

template <typename T>
MatrixType::matrix_type
matrix_real_probe (const MArray<T>& a)
{
  MatrixType::matrix_type m_type;
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();

  const T zero = 0;

  if (ncols == nrows)
    {
      bool upper = true;
      bool lower = true;
      bool hermitian = true;

      // do the checks for lower/upper/hermitian all in one pass.
      OCTAVE_LOCAL_BUFFER (T, diag, ncols);

      for (octave_idx_type j = 0;
           j < ncols && upper; j++)
        {
          T d = a.elem (j, j);
          upper = upper && (d != zero);
          lower = lower && (d != zero);
          hermitian = hermitian && (d > zero);
          diag[j] = d;
        }

      for (octave_idx_type j = 0;
           j < ncols && (upper || lower || hermitian); j++)
        {
          for (octave_idx_type i = 0; i < j; i++)
            {
              T aij = a.elem (i, j);
              T aji = a.elem (j, i);
              lower = lower && (aij == zero);
              upper = upper && (aji == zero);
              hermitian = hermitian && (aij == aji
                                        && aij*aij < diag[i]*diag[j]);
            }
        }

      if (upper)
        m_type = MatrixType::Upper;
      else if (lower)
        m_type = MatrixType::Lower;
      else if (hermitian)
        m_type = MatrixType::Hermitian;
      else
        m_type = MatrixType::Full;
    }
  else
    m_type = MatrixType::Rectangular;

  return m_type;
}

template <typename T>
MatrixType::matrix_type
matrix_complex_probe (const MArray<std::complex<T>>& a)
{
  MatrixType::matrix_type m_type = MatrixType::Unknown;
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();

  const T zero = 0;
  // get the real type

  if (ncols == nrows)
    {
      bool upper = true;
      bool lower = true;
      bool hermitian = true;

      // do the checks for lower/upper/hermitian all in one pass.
      OCTAVE_LOCAL_BUFFER (T, diag, ncols);

      for (octave_idx_type j = 0;
           j < ncols && upper; j++)
        {
          std::complex<T> d = a.elem (j, j);
          upper = upper && (d != zero);
          lower = lower && (d != zero);
          hermitian = hermitian && (d.real () > zero && d.imag () == zero);
          diag[j] = d.real ();
        }

      for (octave_idx_type j = 0;
           j < ncols && (upper || lower || hermitian); j++)
        {
          for (octave_idx_type i = 0; i < j; i++)
            {
              std::complex<T> aij = a.elem (i, j);
              std::complex<T> aji = a.elem (j, i);
              lower = lower && (aij == zero);
              upper = upper && (aji == zero);
              hermitian = hermitian && (aij == octave::math::conj (aji)
                                        && std::norm (aij) < diag[i]*diag[j]);
            }
        }

      if (upper)
        m_type = MatrixType::Upper;
      else if (lower)
        m_type = MatrixType::Lower;
      else if (hermitian)
        m_type = MatrixType::Hermitian;
      else
        m_type = MatrixType::Full;
    }
  else
    m_type = MatrixType::Rectangular;

  return m_type;
}

MatrixType::MatrixType (const Matrix& a)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (0), m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (true), m_nperm (0), m_perm (nullptr)
{
  m_type = matrix_real_probe (a);
}

MatrixType::MatrixType (const ComplexMatrix& a)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (0), m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (true), m_nperm (0), m_perm (nullptr)
{
  m_type = matrix_complex_probe (a);
}

MatrixType::MatrixType (const FloatMatrix& a)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (0), m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (true), m_nperm (0), m_perm (nullptr)
{
  m_type = matrix_real_probe (a);
}

MatrixType::MatrixType (const FloatComplexMatrix& a)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (0), m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (true), m_nperm (0), m_perm (nullptr)
{
  m_type = matrix_complex_probe (a);
}


template <typename T>
MatrixType::MatrixType (const MSparse<T>& a)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (0), m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (false), m_nperm (0), m_perm (nullptr)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  octave_idx_type nm = (ncols < nrows ? ncols : nrows);
  octave_idx_type nnz = a.nnz ();

  if (octave::sparse_params::get_key ("spumoni") != 0.)
    warn_calculating_sparse_type ();

  m_sp_bandden = octave::sparse_params::get_bandden ();
  bool maybe_hermitian = false;
  m_type = MatrixType::Full;

  if (nnz == nm)
    {
      matrix_type tmp_typ = MatrixType::Diagonal;
      octave_idx_type i;
      // Maybe the matrix is diagonal
      for (i = 0; i < nm; i++)
        {
          if (a.cidx (i+1) != a.cidx (i) + 1)
            {
              tmp_typ = MatrixType::Full;
              break;
            }
          if (a.ridx (i) != i)
            {
              tmp_typ = MatrixType::Permuted_Diagonal;
              break;
            }
        }

      if (tmp_typ == MatrixType::Permuted_Diagonal)
        {
          std::vector<bool> found (nrows);

          for (octave_idx_type j = 0; j < i; j++)
            found[j] = true;
          for (octave_idx_type j = i; j < nrows; j++)
            found[j] = false;

          for (octave_idx_type j = i; j < nm; j++)
            {
              if ((a.cidx (j+1) > a.cidx (j) + 1)
                  || ((a.cidx (j+1) == a.cidx (j) + 1) && found[a.ridx (j)]))
                {
                  tmp_typ = MatrixType::Full;
                  break;
                }
              found[a.ridx (j)] = true;
            }
        }
      m_type = tmp_typ;
    }

  if (m_type == MatrixType::Full)
    {
      // Search for banded, upper and lower triangular matrices
      bool singular = false;
      m_upper_band = 0;
      m_lower_band = 0;
      for (octave_idx_type j = 0; j < ncols; j++)
        {
          bool zero_on_diagonal = false;
          if (j < nrows)
            {
              zero_on_diagonal = true;
              for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                if (a.ridx (i) == j)
                  {
                    zero_on_diagonal = false;
                    break;
                  }
            }

          if (zero_on_diagonal)
            {
              singular = true;
              break;
            }

          if (a.cidx (j+1) != a.cidx (j))
            {
              octave_idx_type ru = a.ridx (a.cidx (j));
              octave_idx_type rl = a.ridx (a.cidx (j+1)-1);

              if (j - ru > m_upper_band)
                m_upper_band = j - ru;

              if (rl - j > m_lower_band)
                m_lower_band = rl - j;
            }
        }

      if (! singular)
        {
          m_bandden = double (nnz) /
                      (double (ncols) * (double (m_lower_band) +
                                         double (m_upper_band)) -
                       0.5 * double (m_upper_band + 1) * double (m_upper_band) -
                       0.5 * double (m_lower_band + 1) * double (m_lower_band));

          if (nrows == ncols && m_sp_bandden != 1. && m_bandden > m_sp_bandden)
            {
              if (m_upper_band == 1 && m_lower_band == 1)
                m_type = MatrixType::Tridiagonal;
              else
                m_type = MatrixType::Banded;

              octave_idx_type nnz_in_band
                = ((m_upper_band + m_lower_band + 1) * nrows
                   - (1 + m_upper_band) * m_upper_band / 2
                   - (1 + m_lower_band) * m_lower_band / 2);

              if (nnz_in_band == nnz)
                m_dense = true;
              else
                m_dense = false;
            }

          // If a matrix is Banded but also Upper/Lower, set to the latter.
          if (m_upper_band == 0)
            m_type = MatrixType::Lower;
          else if (m_lower_band == 0)
            m_type = MatrixType::Upper;

          if (m_upper_band == m_lower_band && nrows == ncols)
            maybe_hermitian = true;
        }

      if (m_type == MatrixType::Full)
        {
          // Search for a permuted triangular matrix, and test if
          // permutation is singular

          // FIXME: Perhaps this should be based on a dmperm algorithm?
          bool found = false;

          m_nperm = ncols;
          m_perm = new octave_idx_type [ncols];

          for (octave_idx_type i = 0; i < ncols; i++)
            m_perm[i] = -1;

          for (octave_idx_type i = 0; i < nm; i++)
            {
              found = false;

              for (octave_idx_type j = 0; j < ncols; j++)
                {
                  if ((a.cidx (j+1) - a.cidx (j)) > 0
                      && (a.ridx (a.cidx (j+1)-1) == i))
                    {
                      m_perm[i] = j;
                      found = true;
                      break;
                    }
                }

              if (! found)
                break;
            }

          if (found)
            {
              m_type = MatrixType::Permuted_Upper;
              if (ncols > nrows)
                {
                  octave_idx_type k = nrows;
                  for (octave_idx_type i = 0; i < ncols; i++)
                    if (m_perm[i] == -1)
                      m_perm[i] = k++;
                }
            }
          else if (a.cidx (nm) == a.cidx (ncols))
            {
              m_nperm = nrows;
              delete [] m_perm;
              m_perm = new octave_idx_type [nrows];
              OCTAVE_LOCAL_BUFFER (octave_idx_type, tmp, nrows);

              for (octave_idx_type i = 0; i < nrows; i++)
                {
                  m_perm[i] = -1;
                  tmp[i] = -1;
                }

              for (octave_idx_type j = 0; j < ncols; j++)
                for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                  m_perm[a.ridx (i)] = j;

              found = true;
              for (octave_idx_type i = 0; i < nm; i++)
                if (m_perm[i] == -1)
                  {
                    found = false;
                    break;
                  }
                else
                  {
                    tmp[m_perm[i]] = 1;
                  }

              if (found)
                {
                  octave_idx_type k = ncols;
                  for (octave_idx_type i = 0; i < nrows; i++)
                    {
                      if (tmp[i] == -1)
                        {
                          if (k < nrows)
                            {
                              m_perm[k++] = i;
                            }
                          else
                            {
                              found = false;
                              break;
                            }
                        }
                    }
                }

              if (found)
                m_type = MatrixType::Permuted_Lower;
              else
                {
                  delete [] m_perm;
                  m_nperm = 0;
                }
            }
          else
            {
              delete [] m_perm;
              m_nperm = 0;
            }
        }

      // FIXME: Disable lower under-determined and upper over-determined
      //        problems as being detected, and force to treat as singular
      //        as this seems to cause issues.
      if (((m_type == MatrixType::Lower
            || m_type == MatrixType::Permuted_Lower)
           && nrows > ncols)
          || ((m_type == MatrixType::Upper
               || m_type == MatrixType::Permuted_Upper)
              && nrows < ncols))
        {
          if (m_type == MatrixType::Permuted_Upper
              || m_type == MatrixType::Permuted_Lower)
            delete [] m_perm;
          m_nperm = 0;
          m_type = MatrixType::Rectangular;
        }

      if (m_type == MatrixType::Full && ncols != nrows)
        m_type = MatrixType::Rectangular;

      if (maybe_hermitian && (m_type == MatrixType::Full
                              || m_type == MatrixType::Tridiagonal
                              || m_type == MatrixType::Banded))
        {
          bool is_herm = true;

          // first, check whether the diagonal is positive & extract it
          ColumnVector diag (ncols);

          for (octave_idx_type j = 0; is_herm && j < ncols; j++)
            {
              is_herm = false;
              for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                {
                  if (a.ridx (i) == j)
                    {
                      T d = a.data (i);
                      is_herm = (std::real (d) > 0.0
                                 && std::imag (d) == 0.0);
                      diag(j) = std::real (d);
                      break;
                    }
                }
            }

          // next, check symmetry and 2x2 positiveness

          for (octave_idx_type j = 0; is_herm && j < ncols; j++)
            for (octave_idx_type i = a.cidx (j); is_herm && i < a.cidx (j+1); i++)
              {
                octave_idx_type k = a.ridx (i);
                is_herm = k == j;
                if (is_herm)
                  continue;

                T d = a.data (i);
                if (std::norm (d) < diag(j)*diag(k))
                  {
                    d = octave::math::conj (d);
                    for (octave_idx_type l = a.cidx (k); l < a.cidx (k+1); l++)
                      {
                        if (a.ridx (l) == j)
                          {
                            is_herm = a.data (l) == d;
                            break;
                          }
                      }
                  }
              }

          if (is_herm)
            {
              if (m_type == MatrixType::Full)
                m_type = MatrixType::Hermitian;
              else if (m_type == MatrixType::Banded)
                m_type = MatrixType::Banded_Hermitian;
              else
                m_type = MatrixType::Tridiagonal_Hermitian;
            }
        }
    }
}


MatrixType::MatrixType (const matrix_type t, bool _full)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (octave::sparse_params::get_bandden ()),
    m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (_full), m_nperm (0), m_perm (nullptr)
{
  if (t == MatrixType::Unknown || t == MatrixType::Full
      || t == MatrixType::Diagonal || t == MatrixType::Permuted_Diagonal
      || t == MatrixType::Upper || t == MatrixType::Lower
      || t == MatrixType::Tridiagonal || t == MatrixType::Tridiagonal_Hermitian
      || t == MatrixType::Rectangular)
    m_type = t;
  else
    warn_invalid ();
}

MatrixType::MatrixType (const matrix_type t, const octave_idx_type np,
                        const octave_idx_type *p, bool _full)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (octave::sparse_params::get_bandden ()),
    m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (_full), m_nperm (0), m_perm (nullptr)
{
  if ((t == MatrixType::Permuted_Upper || t == MatrixType::Permuted_Lower)
      && np > 0 && p != nullptr)
    {
      m_type = t;
      m_nperm = np;
      m_perm = new octave_idx_type [m_nperm];
      for (octave_idx_type i = 0; i < m_nperm; i++)
        m_perm[i] = p[i];
    }
  else
    warn_invalid ();
}

MatrixType::MatrixType (const matrix_type t, const octave_idx_type ku,
                        const octave_idx_type kl, bool _full)
  : m_type (MatrixType::Unknown),
    m_sp_bandden (octave::sparse_params::get_bandden ()),
    m_bandden (0), m_upper_band (0), m_lower_band (0),
    m_dense (false), m_full (_full), m_nperm (0), m_perm (nullptr)
{
  if (t == MatrixType::Banded || t == MatrixType::Banded_Hermitian)
    {
      m_type = t;
      m_upper_band = ku;
      m_lower_band = kl;
    }
  else
    warn_invalid ();
}

MatrixType::~MatrixType (void)
{
  if (m_nperm != 0)
    {
      delete [] m_perm;
    }
}

MatrixType&
MatrixType::operator = (const MatrixType& a)
{
  if (this != &a)
    {
      m_type = a.m_type;
      m_sp_bandden = a.m_sp_bandden;
      m_bandden = a.m_bandden;
      m_upper_band = a.m_upper_band;
      m_lower_band = a.m_lower_band;
      m_dense = a.m_dense;
      m_full = a.m_full;

      if (m_nperm)
        {
          delete[] m_perm;
        }

      if (a.m_nperm != 0)
        {
          m_perm = new octave_idx_type [a.m_nperm];
          for (octave_idx_type i = 0; i < a.m_nperm; i++)
            m_perm[i] = a.m_perm[i];
        }

      m_nperm = a.m_nperm;
    }

  return *this;
}

int
MatrixType::type (bool quiet)
{
  if (m_type != MatrixType::Unknown
      && (m_full || m_sp_bandden == octave::sparse_params::get_bandden ()))
    {
      if (! quiet && octave::sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return m_type;
    }

  if (m_type != MatrixType::Unknown
      && octave::sparse_params::get_key ("spumoni") != 0.)
    (*current_liboctave_warning_with_id_handler)
      ("Octave:matrix-type-info", "invalidating matrix type");

  m_type = MatrixType::Unknown;

  return m_type;
}

int
MatrixType::type (const SparseMatrix& a)
{
  if (m_type != MatrixType::Unknown
      && (m_full || m_sp_bandden == octave::sparse_params::get_bandden ()))
    {
      if (octave::sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return m_type;
    }

  MatrixType tmp_typ (a);
  m_type = tmp_typ.m_type;
  m_sp_bandden = tmp_typ.m_sp_bandden;
  m_bandden = tmp_typ.m_bandden;
  m_upper_band = tmp_typ.m_upper_band;
  m_lower_band = tmp_typ.m_lower_band;
  m_dense = tmp_typ.m_dense;
  m_full = tmp_typ.m_full;
  m_nperm = tmp_typ.m_nperm;

  if (m_nperm != 0)
    {
      m_perm = new octave_idx_type [m_nperm];
      for (octave_idx_type i = 0; i < m_nperm; i++)
        m_perm[i] = tmp_typ.m_perm[i];
    }

  return m_type;
}

int
MatrixType::type (const SparseComplexMatrix& a)
{
  if (m_type != MatrixType::Unknown
      && (m_full || m_sp_bandden == octave::sparse_params::get_bandden ()))
    {
      if (octave::sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return m_type;
    }

  MatrixType tmp_typ (a);
  m_type = tmp_typ.m_type;
  m_sp_bandden = tmp_typ.m_sp_bandden;
  m_bandden = tmp_typ.m_bandden;
  m_upper_band = tmp_typ.m_upper_band;
  m_lower_band = tmp_typ.m_lower_band;
  m_dense = tmp_typ.m_dense;
  m_full = tmp_typ.m_full;
  m_nperm = tmp_typ.m_nperm;

  if (m_nperm != 0)
    {
      m_perm = new octave_idx_type [m_nperm];
      for (octave_idx_type i = 0; i < m_nperm; i++)
        m_perm[i] = tmp_typ.m_perm[i];
    }

  return m_type;
}

int
MatrixType::type (const Matrix& a)
{
  if (m_type != MatrixType::Unknown)
    {
      if (octave::sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return m_type;
    }

  MatrixType tmp_typ (a);
  m_type = tmp_typ.m_type;
  m_full = tmp_typ.m_full;
  m_nperm = tmp_typ.m_nperm;

  if (m_nperm != 0)
    {
      m_perm = new octave_idx_type [m_nperm];
      for (octave_idx_type i = 0; i < m_nperm; i++)
        m_perm[i] = tmp_typ.m_perm[i];
    }

  return m_type;
}

int
MatrixType::type (const ComplexMatrix& a)
{
  if (m_type != MatrixType::Unknown)
    {
      if (octave::sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return m_type;
    }

  MatrixType tmp_typ (a);
  m_type = tmp_typ.m_type;
  m_full = tmp_typ.m_full;
  m_nperm = tmp_typ.m_nperm;

  if (m_nperm != 0)
    {
      m_perm = new octave_idx_type [m_nperm];
      for (octave_idx_type i = 0; i < m_nperm; i++)
        m_perm[i] = tmp_typ.m_perm[i];
    }

  return m_type;
}

int
MatrixType::type (const FloatMatrix& a)
{
  if (m_type != MatrixType::Unknown)
    {
      if (octave::sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return m_type;
    }

  MatrixType tmp_typ (a);
  m_type = tmp_typ.m_type;
  m_full = tmp_typ.m_full;
  m_nperm = tmp_typ.m_nperm;

  if (m_nperm != 0)
    {
      m_perm = new octave_idx_type [m_nperm];
      for (octave_idx_type i = 0; i < m_nperm; i++)
        m_perm[i] = tmp_typ.m_perm[i];
    }

  return m_type;
}

int
MatrixType::type (const FloatComplexMatrix& a)
{
  if (m_type != MatrixType::Unknown)
    {
      if (octave::sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return m_type;
    }

  MatrixType tmp_typ (a);
  m_type = tmp_typ.m_type;
  m_full = tmp_typ.m_full;
  m_nperm = tmp_typ.m_nperm;

  if (m_nperm != 0)
    {
      m_perm = new octave_idx_type [m_nperm];
      for (octave_idx_type i = 0; i < m_nperm; i++)
        m_perm[i] = tmp_typ.m_perm[i];
    }

  return m_type;
}

void
MatrixType::info () const
{
  if (octave::sparse_params::get_key ("spumoni") != 0.)
    {
      if (m_type == MatrixType::Unknown)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "unknown matrix type");
      else if (m_type == MatrixType::Diagonal)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "diagonal sparse matrix");
      else if (m_type == MatrixType::Permuted_Diagonal)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "permuted diagonal sparse matrix");
      else if (m_type == MatrixType::Upper)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "upper triangular matrix");
      else if (m_type == MatrixType::Lower)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "lower triangular matrix");
      else if (m_type == MatrixType::Permuted_Upper)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "permuted upper triangular matrix");
      else if (m_type == MatrixType::Permuted_Lower)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "permuted lower triangular Matrix");
      else if (m_type == MatrixType::Banded)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info",
           "banded sparse matrix %" OCTAVE_IDX_TYPE_FORMAT "-1-"
           "%" OCTAVE_IDX_TYPE_FORMAT " (density %f)",
           m_lower_band, m_upper_band, m_bandden);
      else if (m_type == MatrixType::Banded_Hermitian)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info",
           "banded hermitian/symmetric sparse matrix %" OCTAVE_IDX_TYPE_FORMAT
           "-1-%" OCTAVE_IDX_TYPE_FORMAT " (density %f)",
           m_lower_band, m_upper_band, m_bandden);
      else if (m_type == MatrixType::Hermitian)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "hermitian/symmetric matrix");
      else if (m_type == MatrixType::Tridiagonal)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "tridiagonal sparse matrix");
      else if (m_type == MatrixType::Tridiagonal_Hermitian)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info",
           "hermitian/symmetric tridiagonal sparse matrix");
      else if (m_type == MatrixType::Rectangular)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "rectangular/singular matrix");
      else if (m_type == MatrixType::Full)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "m_full matrix");
    }
}

void
MatrixType::mark_as_symmetric (void)
{
  if (m_type == MatrixType::Tridiagonal
      || m_type == MatrixType::Tridiagonal_Hermitian)
    m_type = MatrixType::Tridiagonal_Hermitian;
  else if (m_type == MatrixType::Banded
           || m_type == MatrixType::Banded_Hermitian)
    m_type = MatrixType::Banded_Hermitian;
  else if (m_type == MatrixType::Full || m_type == MatrixType::Hermitian
           || m_type == MatrixType::Unknown)
    m_type = MatrixType::Hermitian;
  else
    (*current_liboctave_error_handler)
      ("Can not mark current matrix type as symmetric");
}

void
MatrixType::mark_as_unsymmetric (void)
{
  if (m_type == MatrixType::Tridiagonal
      || m_type == MatrixType::Tridiagonal_Hermitian)
    m_type = MatrixType::Tridiagonal;
  else if (m_type == MatrixType::Banded
           || m_type == MatrixType::Banded_Hermitian)
    m_type = MatrixType::Banded;
  else if (m_type == MatrixType::Full || m_type == MatrixType::Hermitian
           || m_type == MatrixType::Unknown)
    m_type = MatrixType::Full;
}

void
MatrixType::mark_as_permuted (const octave_idx_type np,
                              const octave_idx_type *p)
{
  m_nperm = np;
  m_perm = new octave_idx_type [m_nperm];
  for (octave_idx_type i = 0; i < m_nperm; i++)
    m_perm[i] = p[i];

  if (m_type == MatrixType::Diagonal
      || m_type == MatrixType::Permuted_Diagonal)
    m_type = MatrixType::Permuted_Diagonal;
  else if (m_type == MatrixType::Upper || m_type == MatrixType::Permuted_Upper)
    m_type = MatrixType::Permuted_Upper;
  else if (m_type == MatrixType::Lower || m_type == MatrixType::Permuted_Lower)
    m_type = MatrixType::Permuted_Lower;
  else
    (*current_liboctave_error_handler)
      ("Can not mark current matrix type as symmetric");
}

void
MatrixType::mark_as_unpermuted (void)
{
  if (m_nperm)
    {
      m_nperm = 0;
      delete [] m_perm;
    }

  if (m_type == MatrixType::Diagonal
      || m_type == MatrixType::Permuted_Diagonal)
    m_type = MatrixType::Diagonal;
  else if (m_type == MatrixType::Upper || m_type == MatrixType::Permuted_Upper)
    m_type = MatrixType::Upper;
  else if (m_type == MatrixType::Lower || m_type == MatrixType::Permuted_Lower)
    m_type = MatrixType::Lower;
}

MatrixType
MatrixType::transpose (void) const
{
  MatrixType retval (*this);
  if (m_type == MatrixType::Upper)
    retval.m_type = MatrixType::Lower;
  else if (m_type == MatrixType::Permuted_Upper)
    retval.m_type = MatrixType::Permuted_Lower;
  else if (m_type == MatrixType::Lower)
    retval.m_type = MatrixType::Upper;
  else if (m_type == MatrixType::Permuted_Lower)
    retval.m_type = MatrixType::Permuted_Upper;
  else if (m_type == MatrixType::Banded)
    {
      retval.m_upper_band = m_lower_band;
      retval.m_lower_band = m_upper_band;
    }

  return retval;
}

// Instantiate MatrixType template constructors that we need.

template MatrixType::MatrixType (const MSparse<double>&);
template MatrixType::MatrixType (const MSparse<Complex>&);
