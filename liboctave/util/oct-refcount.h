////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

#if ! defined (octave_oct_refcount_h)
#define octave_oct_refcount_h 1

#include "octave-config.h"

#include <atomic>

OCTAVE_BEGIN_NAMESPACE(octave)

// Encapsulates a reference counter.

template <typename T>
class refcount
{
public:

  typedef T count_type;

  refcount (count_type initial_count)
    : m_count (initial_count)
  { }

  refcount (const refcount&) = delete;

  refcount& operator = (const refcount&) = delete;

  ~refcount (void) = default;

  // Increment/Decrement.  int is postfix.
  count_type operator++ (void)
  {
    return ++m_count;
  }

  count_type operator++ (int)
  {
    return m_count++;
  }

  count_type operator-- (void)
  {
    return --m_count;
  }

  count_type operator-- (int)
  {
    return m_count--;
  }

  count_type value (void) const
  {
    return m_count.load ();
  }

  operator count_type (void) const
  {
    return value ();
  }

private:

  std::atomic<T> m_count;
};

OCTAVE_END_NAMESPACE(octave)

#endif
