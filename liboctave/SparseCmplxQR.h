/*

Copyright (C) 2005 David Bateman

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

#if !defined (sparse_cmplx_QR_h)
#define sparse_cmplx_QR_h 1

#include <iostream>

#include "dMatrix.h"
#include "CMatrix.h"
#include "dSparse.h"
#include "CSparse.h"
#include "oct-sparse.h"

#ifdef IDX_TYPE_LONG
#define CXSPARSE_ZNAME(name) cs_cl ## name
#else
#define CXSPARSE_ZNAME(name) cs_ci ## name
#endif

class
SparseComplexQR
{
protected:
  class SparseComplexQR_rep
  {
  public:
    SparseComplexQR_rep (const SparseComplexMatrix& a, int order);

    ~SparseComplexQR_rep (void);
#ifdef HAVE_CXSPARSE
    bool ok (void) const { return (N && S); }
#else
    bool ok (void) const { return false; }
#endif
    SparseComplexMatrix V (void) const;

    ColumnVector Pinv (void) const;

    ColumnVector P (void) const;

    SparseComplexMatrix R (const bool econ) const;

    ComplexMatrix C (const ComplexMatrix &b) const;

    int count;

    octave_idx_type nrows;
#ifdef HAVE_CXSPARSE
    CXSPARSE_ZNAME (s) *S;

    CXSPARSE_ZNAME (n) *N;
#endif
  };
private:
  SparseComplexQR_rep *rep;

public:  
  SparseComplexQR (void) : 
    rep (new SparseComplexQR_rep (SparseComplexMatrix(), -1)) { }

  SparseComplexQR (const SparseComplexMatrix& a, int order = -1) : 
    rep (new SparseComplexQR_rep (a, order)) { }

  SparseComplexQR (const SparseComplexQR& a) : rep (a.rep) { rep->count++; }

  ~SparseComplexQR (void)
    {
      if (--rep->count <= 0)
	delete rep;
    }

  SparseComplexQR& operator = (const SparseComplexQR& a)
    {
      if (this != &a)
	{
	  if (--rep->count <= 0)
	    delete rep;

	  rep = a.rep;
	  rep->count++;
	}
      return *this;
    }

  bool ok (void) const { return rep->ok(); }

  SparseComplexMatrix V (void) const { return rep->V(); }

  ColumnVector Pinv (void) const { return rep->P(); }

  ColumnVector P (void) const { return rep->P(); }

  SparseComplexMatrix R (const bool econ = false) const 
    { return rep->R(econ); }

  ComplexMatrix C (const ComplexMatrix &b) const { return rep->C(b); }

  friend ComplexMatrix qrsolve (const SparseComplexMatrix &a, const Matrix &b,
				octave_idx_type &info);

  friend SparseComplexMatrix qrsolve (const SparseComplexMatrix &a, 
				      const SparseMatrix &b,
				      octave_idx_type &info);

  friend ComplexMatrix qrsolve (const SparseComplexMatrix &a, 
				const ComplexMatrix &b,
				octave_idx_type &info);

  friend SparseComplexMatrix qrsolve (const SparseComplexMatrix &a, 
				      const SparseComplexMatrix &b,
				      octave_idx_type &info);

protected:
#ifdef HAVE_CXSPARSE
  CXSPARSE_ZNAME (s) * S (void) { return rep->S; }

  CXSPARSE_ZNAME (n) * N (void) { return rep->N; }
#endif
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
