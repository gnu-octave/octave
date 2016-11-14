/*

Copyright (C) 1994-2016 John W. Eaton

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

#if ! defined (octave_EIG_h)
#define octave_EIG_h 1

#include "octave-config.h"

#include <iosfwd>

#include "dMatrix.h"
#include "CMatrix.h"
#include "CColVector.h"

class
OCTAVE_API
EIG
{
  friend class Matrix;
  friend class ComplexMatrix;

public:

  EIG (void) : lambda (), v (), w () { }

  EIG (const Matrix& a, bool calc_rev = true,
       bool calc_lev = true, bool balance = true)
    : lambda (), v (), w ()
  {
    init (a, calc_rev, calc_lev, balance);
  }

  EIG (const Matrix& a, octave_idx_type& info,
       bool calc_rev = true, bool calc_lev = true, bool balance = true)
    : lambda (), v (), w ()
  {
    info = init (a, calc_rev, calc_lev, balance);
  }

  EIG (const Matrix& a, const Matrix& b,
       bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : lambda (), v (), w ()
  {
    init (a, b, calc_rev, calc_lev, force_qz);
  }

  EIG (const Matrix& a, const Matrix& b, octave_idx_type& info,
       bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : lambda (), v (), w ()
  {
    info = init (a, b, calc_rev, calc_lev, force_qz);
  }

  EIG (const ComplexMatrix& a, bool calc_rev = true,
       bool calc_lev = true, bool balance = true)
    : lambda (), v (), w ()
  {
    init (a, calc_rev, calc_lev, balance);
  }

  EIG (const ComplexMatrix& a, octave_idx_type& info,
       bool calc_rev = true, bool calc_lev = true, bool balance = true)
    : lambda (), v (), w ()
  {
    info = init (a, calc_rev, calc_lev, balance);
  }

  EIG (const ComplexMatrix& a, const ComplexMatrix& b,
       bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : lambda (), v (), w ()
  {
    init (a, b, calc_rev, calc_lev, force_qz);
  }

  EIG (const ComplexMatrix& a, const ComplexMatrix& b,
       octave_idx_type& info, bool calc_rev = true, bool calc_lev = true,
       bool force_qz = false)
    : lambda (), v (), w ()
  {
    info = init (a, b, calc_rev, calc_lev, force_qz);
  }

  EIG (const EIG& a) : lambda (a.lambda), v (a.v), w (a.w) { }

  EIG& operator = (const EIG& a)
  {
    if (this != &a)
      {
        lambda = a.lambda;
        v = a.v;
        w = a.w;
      }
    return *this;
  }

  ~EIG (void) { }

  ComplexColumnVector eigenvalues (void) const { return lambda; }
  ComplexMatrix right_eigenvectors (void) const { return v; }
  ComplexMatrix left_eigenvectors (void) const { return w; }

  friend std::ostream&  operator << (std::ostream& os, const EIG& a);

private:

  ComplexColumnVector lambda;
  ComplexMatrix v;
  ComplexMatrix w;

  octave_idx_type init (const Matrix& a, bool calc_rev, bool calc_lev,
                        bool balance);

  octave_idx_type init (const Matrix& a, const Matrix& b,
                        bool calc_rev, bool calc_lev, bool force_qz);

  octave_idx_type init (const ComplexMatrix& a, bool calc_rev,
                        bool calc_lev, bool balance);

  octave_idx_type init (const ComplexMatrix& a, const ComplexMatrix& b,
                        bool calc_rev, bool calc_lev, bool force_qz);

  octave_idx_type symmetric_init (const Matrix& a, bool calc_rev,
                                  bool calc_lev);

  octave_idx_type symmetric_init (const Matrix& a, const Matrix& b,
                                  bool calc_rev, bool calc_lev);

  octave_idx_type hermitian_init (const ComplexMatrix& a,
                                  bool calc_rev, bool calc_lev);

  octave_idx_type hermitian_init (const ComplexMatrix& a,
                                  const ComplexMatrix& b,
                                  bool calc_rev, bool calc_lev);

};

#endif

