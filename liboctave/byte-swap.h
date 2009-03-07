/*

Copyright (C) 1996, 1997, 1998, 2004, 2005, 2006, 2007, 2008 John W. Eaton

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

#if !defined (octave_byte_swap_h)
#define octave_byte_swap_h 1

// FIXME -- not sure these volatile qualifiers are really
// needed or appropriate here.

static inline void
swap_bytes (volatile void *ptr, unsigned int i, unsigned int j)
{
  volatile char *t = static_cast<volatile char *> (ptr);

  char tmp = t[i];
  t[i] = t[j];
  t[j] = tmp;
}

template <int n>
void
swap_bytes (volatile void *ptr)
{
  for (int i = 0; i < n/2; i++)
    swap_bytes (ptr, i, n-1-i);
}

template <>
inline void
swap_bytes <1> (volatile void *)
{
}

template <>
inline void
swap_bytes <2> (volatile void *ptr)
{
  swap_bytes (ptr, 0, 1);
}

template <>
inline void
swap_bytes <4> (volatile void *ptr)
{
  swap_bytes (ptr, 0, 3);
  swap_bytes (ptr, 1, 2);
}

template <>
inline void
swap_bytes <8> (volatile void *ptr)
{
  swap_bytes (ptr, 0, 7);
  swap_bytes (ptr, 1, 6);
  swap_bytes (ptr, 2, 5);
  swap_bytes (ptr, 3, 4);
}

template <int n>
void
swap_bytes (volatile void *ptr, int len)
{
  volatile char *t = static_cast<volatile char *> (ptr);

  for (int i = 0; i < len; i++)
    {
      swap_bytes<n> (t);
      t += n;
    }
}

template <>
inline void
swap_bytes<1> (volatile void *, int)
{
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
