/*

Copyright (C) 2012-2017 Jaroslav Hajek

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

#if ! defined (octave_oct_refcount_h)
#define octave_oct_refcount_h 1

#include "octave-config.h"

#if (defined (OCTAVE_ENABLE_ATOMIC_REFCOUNT) \
     && (defined (__GNUC__) || defined (_MSC_VER)))

#  if defined (__GNUC__)

#    define OCTAVE_ATOMIC_INCREMENT(x) __sync_add_and_fetch (x,  1)
#    define OCTAVE_ATOMIC_DECREMENT(x) __sync_add_and_fetch (x, -1)
#    define OCTAVE_ATOMIC_POST_INCREMENT(x) __sync_fetch_and_add (x,  1)
#    define OCTAVE_ATOMIC_POST_DECREMENT(x) __sync_fetch_and_add (x, -1)

#  elif defined (_MSC_VER)

#    include <intrin.h>

#    define OCTAVE_ATOMIC_INCREMENT(x)                  \
  _InterlockedIncrement (static_cast<long *> (x))

#    define OCTAVE_ATOMIC_DECREMENT(x)                  \
  _InterlockedDecrement (static_cast<long *> (x))

#    define OCTAVE_ATOMIC_POST_INCREMENT(x)             \
  _InterlockedExchangeAdd (static_cast<long *> (x))

#    define OCTAVE_ATOMIC_POST_DECREMENT(x)             \
  _InterlockedExchangeAdd (static_cast<long *> (x))

#  endif

#else

// Generic non-locking versions

#  define OCTAVE_ATOMIC_INCREMENT(x) ++(*(x))
#  define OCTAVE_ATOMIC_DECREMENT(x) --(*(x))
#  define OCTAVE_ATOMIC_POST_INCREMENT(x) (*(x))++
#  define OCTAVE_ATOMIC_POST_DECREMENT(x) (*(x))--

#endif

namespace octave
{

  // Encapsulates a reference counter.

  template <typename T>
  class refcount
  {
  public:

    typedef T count_type;

    refcount (count_type initial_count)
      : count (initial_count)
    { }

    // Increment/Decrement.  int is postfix.
    count_type operator++ (void)
    {
      return OCTAVE_ATOMIC_INCREMENT (&count);
    }

    count_type operator++ (int)
    {
      return OCTAVE_ATOMIC_POST_INCREMENT (&count);
    }

    count_type operator-- (void)
    {
      return OCTAVE_ATOMIC_DECREMENT (&count);
    }

    count_type operator-- (int)
    {
      return OCTAVE_ATOMIC_POST_DECREMENT (&count);
    }

    operator count_type (void) const
    {
      return static_cast<count_type const volatile&> (count);
    }

    count_type * get (void)
    {
      return &count;
    }

  private:

    count_type count;
  };
}

template <typename T>
using octave_refcount OCTAVE_DEPRECATED (4.4, "use 'octave::refcount' instead") = octave::refcount<T>;

#endif
