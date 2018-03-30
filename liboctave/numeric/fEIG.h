/*

Copyright (C) 1994-2018 John W. Eaton

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

#if ! defined (octave_fEIG_h)
#define octave_fEIG_h 1

#include "octave-config.h"

#include <iosfwd>

#include "fCColVector.h"
#include "fCMatrix.h"

class FloatMatrix;

class
OCTAVE_API
FloatEIG
{
  friend class FloatMatrix;
  friend class FloatComplexMatrix;

public:

  FloatEIG (void) : lambda (), v (), w () { }

  FloatEIG (const FloatMatrix& a, bool calc_rev = true,
            bool calc_lev = true, bool balance = true)
    : lambda (), v (), w ()
  {
    init (a, calc_rev, calc_lev, balance);
  }

  FloatEIG (const FloatMatrix& a, octave_idx_type& info,
            bool calc_rev = true, bool calc_lev = true, bool balance = true)
    : lambda (), v (), w ()
  {
    info = init (a, calc_rev, calc_lev, balance);
  }

  FloatEIG (const FloatMatrix& a, const FloatMatrix& b,
            bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : lambda (), v (), w ()
  {
    init (a, b, calc_rev, calc_lev, force_qz);
  }

  FloatEIG (const FloatMatrix& a, const FloatMatrix& b, octave_idx_type& info,
            bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : lambda (), v (), w ()
  {
    info = init (a, b, calc_rev, calc_lev, force_qz);
  }

  FloatEIG (const FloatComplexMatrix& a, bool calc_rev = true,
            bool calc_lev = true, bool balance = true)
    : lambda (), v (), w ()
  {
    init (a, calc_rev, calc_lev, balance);
  }

  FloatEIG (const FloatComplexMatrix& a, octave_idx_type& info,
            bool calc_rev = true, bool calc_lev = true, bool balance = true)
    : lambda (), v (), w ()
  {
    info = init (a, calc_rev, calc_lev, balance);
  }

  FloatEIG (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
            bool calc_rev = true, bool calc_lev = true, bool force_qz = false)
    : lambda (), v (), w ()
  {
    init (a, b, calc_rev, calc_lev, force_qz);
  }

  FloatEIG (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
            octave_idx_type& info, bool calc_rev = true, bool calc_lev = true,
            bool force_qz = false)
    : lambda (), v (), w ()
  {
    info = init (a, b, calc_rev, calc_lev, force_qz);
  }

  FloatEIG (const FloatEIG& a) : lambda (a.lambda), v (a.v), w (a.w) { }

  FloatEIG& operator = (const FloatEIG& a)
  {
    if (this != &a)
      {
        lambda = a.lambda;
        v = a.v;
        w = a.w;
      }
    return *this;
  }

  ~FloatEIG (void) = default;

  FloatComplexColumnVector eigenvalues (void) const { return lambda; }
  FloatComplexMatrix right_eigenvectors (void) const { return v; }
  FloatComplexMatrix left_eigenvectors (void) const { return w; }

  friend std::ostream&  operator << (std::ostream& os, const FloatEIG& a);

private:

  FloatComplexColumnVector lambda;
  FloatComplexMatrix v;
  FloatComplexMatrix w;

  octave_idx_type init (const FloatMatrix& a, bool calc_rev, bool calc_lev,
                        bool balance);

  octave_idx_type init (const FloatMatrix& a, const FloatMatrix& b,
                        bool calc_rev, bool calc_lev, bool force_qz);

  octave_idx_type init (const FloatComplexMatrix& a, bool calc_rev,
                        bool calc_lev, bool balance);

  octave_idx_type init (const FloatComplexMatrix& a,
                        const FloatComplexMatrix& b,
                        bool calc_rev, bool calc_lev, bool force_qz);

  octave_idx_type symmetric_init (const FloatMatrix& a, bool calc_rev,
                                  bool calc_lev);

  octave_idx_type symmetric_init (const FloatMatrix& a, const FloatMatrix& b,
                                  bool calc_rev, bool calc_lev);

  octave_idx_type hermitian_init (const FloatComplexMatrix& a,
                                  bool calc_rev, bool calc_lev);

  octave_idx_type hermitian_init (const FloatComplexMatrix& a,
                                  const FloatComplexMatrix& b,
                                  bool calc_rev, bool calc_lev);

};

#endif
