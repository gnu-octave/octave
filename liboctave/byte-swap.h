/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_byte_swap_h)
#define octave_byte_swap_h 1

static inline void
swap_bytes (char *t, unsigned int i, unsigned int j)
{
  char tmp = t[i];
  t[i] = t[j];
  t[j] = tmp;
}

static inline void
swap_2_bytes (char *t)
{
  swap_bytes (t, 0, 1);
}

static inline void
swap_4_bytes (char *t)
{
  swap_bytes (t, 0, 3);
  swap_bytes (t, 1, 2);
}

static inline void
swap_8_bytes (char *t)
{
  swap_bytes (t, 0, 7);
  swap_bytes (t, 1, 6);
  swap_bytes (t, 2, 5);
  swap_bytes (t, 3, 4);
}

static inline void
swap_2_bytes (char *t, int len)
{
  char *ptr = t;
  for (int i = 0; i < len; i++)
    {
      swap_2_bytes (ptr);
      ptr += 2;
    }
}

static inline void
swap_4_bytes (char *t, int len)
{
  char *ptr = t;
  for (int i = 0; i < len; i++)
    {
      swap_4_bytes (ptr);
      ptr += 4;
    }
}

static inline void
swap_8_bytes (char *t, int len)
{
  char *ptr = t;
  for (int i = 0; i < len; i++)
    {
      swap_8_bytes (ptr);
      ptr += 8;
    }
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
