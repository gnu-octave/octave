////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#if ! defined (octave_oct_handle_h)
#define octave_oct_handle_h 1

#include "octave-config.h"

#include "dMatrix.h"
#include "lo-ieee.h"

#include "error.h"
#include "ov.h"

// ---------------------------------------------------------------------

class octave_handle
{
public:
  octave_handle (void) : m_dval (octave::numeric_limits<double>::NaN ()) { }

  octave_handle (const octave_value& a)
    : m_dval (octave::numeric_limits<double>::NaN ())
  {
    if (a.isempty ())
      ; // do nothing
    else
      {
        try
          {
            m_dval = a.double_value ();
          }
        catch (octave::execution_exception& ee)
          {
            error (ee, "invalid handle");
          }
      }
  }

  octave_handle (int a) : m_dval (a) { }

  octave_handle (double a) : m_dval (a) { }

  octave_handle (const octave_handle& a) : m_dval (a.m_dval) { }

  octave_handle& operator = (const octave_handle& a)
  {
    if (&a != this)
      m_dval = a.m_dval;

    return *this;
  }

  ~octave_handle (void) = default;

  double value (void) const { return m_dval; }

  octave_value as_octave_value (void) const
  {
    return ok () ? octave_value (m_dval) : octave_value (Matrix ());
  }

  // Prefix increment/decrement operators.
  octave_handle& operator ++ (void)
  {
    ++m_dval;
    return *this;
  }

  octave_handle& operator -- (void)
  {
    --m_dval;
    return *this;
  }

  // Postfix increment/decrement operators.
  const octave_handle operator ++ (int)
  {
    octave_handle old_value = *this;
    ++(*this);
    return old_value;
  }

  const octave_handle operator -- (int)
  {
    octave_handle old_value = *this;
    --(*this);
    return old_value;
  }

  bool ok (void) const { return ! octave::math::isnan (m_dval); }

private:
  double m_dval;
};

inline bool
operator == (const octave_handle& a, const octave_handle& b)
{
  return a.value () == b.value ();
}

inline bool
operator != (const octave_handle& a, const octave_handle& b)
{
  return a.value () != b.value ();
}

inline bool
operator < (const octave_handle& a, const octave_handle& b)
{
  return a.value () < b.value ();
}

inline bool
operator <= (const octave_handle& a, const octave_handle& b)
{
  return a.value () <= b.value ();
}

inline bool
operator >= (const octave_handle& a, const octave_handle& b)
{
  return a.value () >= b.value ();
}

inline bool
operator > (const octave_handle& a, const octave_handle& b)
{
  return a.value () > b.value ();
}

#endif
