/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#if !defined (octave_SparseType_h)
#define octave_SparseType_h

class SparseMatrix;
class SparseComplexMatrix;

class
SparseType
{
public:
  enum matrix_type {
    Unknown = 0,
    Full,
    Diagonal,
    Permuted_Diagonal,
    Upper,
    Lower,
    Permuted_Upper,
    Permuted_Lower,
    Banded,
    Hermitian,
    Banded_Hermitian,
    Tridiagonal,
    Tridiagonal_Hermitian,
    Rectangular
  };

  SparseType (void) : typ (Unknown), nperm (0) { }
    
  SparseType (const SparseType &a);

  SparseType (const SparseMatrix &a);

  SparseType (const SparseComplexMatrix &a);

  ~SparseType (void);

  SparseType& operator = (const SparseType& a);

  int type (void) const { return typ; }

  int type (const SparseMatrix &a);

  int type (const SparseComplexMatrix &a);

  double band_density (void) const { return bandden; }

  int nupper (void) const { return upper_band; }

  int nlower (void) const { return lower_band; }

  bool is_dense (void) const { return dense; }

  bool is_diagonal (void) const 
    { return (typ == Diagonal || typ == Permuted_Diagonal); }
  
  bool is_upper_triangular (void) const 
    { return (typ == Upper || typ == Permuted_Upper); }

  bool is_lower_triangular (void) const 
    { return (typ == Lower || typ == Permuted_Lower); }

  bool is_banded (void)
    { return (typ == Banded || typ == Banded_Hermitian); }
  
  bool is_tridiagonal (void) const
    { return (typ == Tridiagonal || typ == Tridiagonal_Hermitian); }
  
  bool is_hermitian (void) const
    { return (typ == Banded_Hermitian || typ == Tridiagonal_Hermitian ||
	      typ == Hermitian); }

  bool is_rectangular (void) const { return (typ == Rectangular); }

  bool is_known (void) const { return (typ != Unknown); }

  bool is_unknown (void) const { return (typ == Unknown); }

  void info (void) const;

  octave_idx_type * triangular_row_perm (void) const { return row_perm; }

  octave_idx_type * triangular_col_perm (void) const { return col_perm; }

  void invaldate_type (void) { typ = Unknown; }

  void mark_as_diagonal (void) { typ = Diagonal; }

  void mark_as_upper_triangular (void) { typ = Upper; }

  void mark_as_lower_triangular (void) { typ = Lower; }

  void mark_as_tridiagonal (void) {typ = Tridiagonal; }

  void mark_as_banded (const octave_idx_type ku, const octave_idx_type kl)
    { typ = Banded; upper_band = ku; lower_band = kl; }

  void mark_as_full (void) { typ = Full; }

  void mark_as_rectangular (void) { typ = Rectangular; }

  void mark_as_dense (void) { dense = true; }

  void mark_as_not_dense (void) { dense = false; }

  void mark_as_symmetric (void);

  void mark_as_unsymmetric (void);

  void mark_as_permuted (const octave_idx_type np, const octave_idx_type *pr, const octave_idx_type *pc);

  void mark_as_unpermuted (void);

  SparseType transpose (void) const;

private:
  void type (int new_typ) { typ = static_cast<matrix_type>(new_typ); }

  matrix_type typ;
  double sp_bandden;
  double bandden;
  octave_idx_type upper_band;
  octave_idx_type lower_band;
  bool dense;
  octave_idx_type nperm;
  octave_idx_type *row_perm;
  octave_idx_type *col_perm;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
