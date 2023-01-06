////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_CollocWt_h)
#define octave_CollocWt_h 1

#include "octave-config.h"

#include <iosfwd>

#include "dMatrix.h"
#include "dColVector.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class OCTAVE_API CollocWt
{
public:

  CollocWt (void)
    : m_n (0), m_inc_left (0), m_inc_right (0), m_lb (0.0), m_rb (1.0),
      m_alpha (0.0), m_beta (0.0), m_r (), m_q (), m_A (), m_B (),
      m_initialized (false)
  { }

  CollocWt (octave_idx_type nc, octave_idx_type il, octave_idx_type ir)
    : m_n (nc), m_inc_left (il), m_inc_right (ir), m_lb (0.0), m_rb (1.0),
      m_alpha (0.0), m_beta (0.0), m_r (), m_q (), m_A (), m_B (),
      m_initialized (false)
  { }

  CollocWt (octave_idx_type nc, octave_idx_type il, octave_idx_type ir,
            double l, double rr)
    : m_n (nc), m_inc_left (il), m_inc_right (ir), m_lb (l), m_rb (rr),
      m_alpha (0.0), m_beta (0.0), m_r (), m_q (), m_A (), m_B (),
      m_initialized (false)
  { }

  CollocWt (octave_idx_type nc, double a, double b, octave_idx_type il,
            octave_idx_type ir)
    : m_n (nc), m_inc_left (il), m_inc_right (ir), m_lb (0.0), m_rb (1.0),
      m_alpha (a), m_beta (b), m_r (), m_q (), m_A (), m_B (),
      m_initialized (false)
  { }

  CollocWt (octave_idx_type nc, double a, double b, octave_idx_type il,
            octave_idx_type ir,
            double ll, double rr)
    : m_n (nc), m_inc_left (il), m_inc_right (ir), m_lb (ll), m_rb (rr),
      m_alpha (a), m_beta (b), m_r (), m_q (), m_A (), m_B (),
      m_initialized (false)
  { }

  CollocWt (const CollocWt& a) = default;

  CollocWt& operator = (const CollocWt& a) = default;

  ~CollocWt (void) = default;

  CollocWt& resize (octave_idx_type nc)
  {
    m_n = nc;
    m_initialized = false;
    return *this;
  }

  CollocWt& add_left (void)
  {
    m_inc_left = 1;
    m_initialized = false;
    return *this;
  }

  CollocWt& delete_left (void)
  {
    m_inc_left = 0;
    m_initialized = false;
    return *this;
  }

  CollocWt& set_left (double val);

  CollocWt& add_right (void)
  {
    m_inc_right = 1;
    m_initialized = false;
    return *this;
  }

  CollocWt& delete_right (void)
  {
    m_inc_right = 0;
    m_initialized = false;
    return *this;
  }

  CollocWt& set_right (double val);

  CollocWt& set_alpha (double val)
  {
    m_alpha = val;
    m_initialized = false;
    return *this;
  }

  CollocWt& set_beta (double val)
  {
    m_beta = val;
    m_initialized = false;
    return *this;
  }

  octave_idx_type ncol (void) const { return m_n; }

  octave_idx_type left_included (void) const { return m_inc_left; }
  octave_idx_type right_included (void) const { return m_inc_right; }

  double left (void) const { return m_lb; }
  double right (void) const { return m_rb; }

  double width (void) const { return m_rb - m_lb; }

  double alpha (void) const { return m_alpha; }
  double beta (void) const { return m_beta; }

  ColumnVector roots (void)
  {
    if (! m_initialized)
      init ();

    return m_r;
  }

  ColumnVector quad (void)
  {
    if (! m_initialized)
      init ();

    return m_q;
  }

  ColumnVector quad_weights (void) { return quad (); }

  Matrix first (void)
  {
    if (! m_initialized)
      init ();

    return m_A;
  }

  Matrix second (void)
  {
    if (! m_initialized)
      init ();

    return m_B;
  }

  friend std::ostream& operator << (std::ostream&, const CollocWt&);

protected:

  octave_idx_type m_n;

  octave_idx_type m_inc_left;
  octave_idx_type m_inc_right;

  double m_lb;
  double m_rb;

  double m_alpha;
  double m_beta;

  ColumnVector m_r;
  ColumnVector m_q;

  Matrix m_A;
  Matrix m_B;

  bool m_initialized;

  void init (void);

  void error (const char *msg);
};

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
OCTAVE_DEPRECATED (7, "use 'octave::CollocWt' instead")
typedef octave::CollocWt CollocWt;
#endif

#endif
