/*

Copyright (C) 2011 Jaroslav Hajek

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_refcount_h)
#define octave_refcount_h 1

// Encapsulates a reference counter.
template <class T>
class octave_refcount
{
public:
  typedef T count_type;

  octave_refcount(count_type initial_count) : count(initial_count) {}

  // Increment/Decrement. int is postfix.
  count_type operator++(void) 
    { 
      return ++count; 
    }

  count_type operator++(int) 
    { 
      return count++; 
    }

  count_type operator--(void)
    { 
      return --count; 
    }

  count_type operator--(int)
    { 
      return count--; 
    }

  operator count_type (void) const { return count; }

  // For low-level optimizations only.
  count_type& direct (void) const { return count; }

private:
  count_type count;
};

#endif
