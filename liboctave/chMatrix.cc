// Matrix manipulations.
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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstring>

#include <string>

#include <iostream.h>

// #include <sys/types.h>  // XXX FIXME XXX

#include "lo-error.h"
#include "str-vec.h"
#include "mx-base.h"
#include "mx-inlines.cc"

// charMatrix class.

charMatrix::charMatrix (const char *s)
  : MArray2<char> ((s ? 1 : 0), (s ? strlen (s) : 0))
{
  int nc = cols ();
  for (int i = 0; i < nc; i++)
    elem (0, i) = s[i];
}

charMatrix::charMatrix (const string& s)
  : MArray2<char> (1, s.length ())
{
  int nc = cols ();
  for (int i = 0; i < nc; i++)
    elem (0, i) = s[i];
}

charMatrix::charMatrix (const string_vector& s)
  : MArray2<char> (s.length (), s.max_length ())
{
  for (int i = 0; i < nr; i++)
    {
      int nc = s[i].length ();

      for (int j = 0; j < nc; j++)
	elem (i, j) = s[i][j];
    }
}

int
charMatrix::operator == (const charMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return equal (data (), a.data (), length ());
}

int
charMatrix::operator != (const charMatrix& a) const
{
  return !(*this == a);
}

charMatrix&
charMatrix::insert (const char *s, int r, int c)
{
  if (s)
    {
      int s_len = strlen (s);

      if (r < 0 || r >= rows () || c < 0 || c + s_len - 1 > cols ())
	{
	  (*current_liboctave_error_handler) ("range error for insert");
	  return *this;
	}

      for (int i = 0; i < s_len; i++)
	elem (r, c+i) = s[i];
    }
  return *this;
}

charMatrix&
charMatrix::insert (const charMatrix& a, int r, int c)
{
  Array2<char>::insert (a, r, c);
  return *this;
}

string
charMatrix::row_as_string (int r) const
{
  if (r < 0 || r >= rows ())
    {
      (*current_liboctave_error_handler) ("range error for row_as_string");
      return 0;
    }

  int nc = cols ();

  string retval (nc, '\0');

  for (int i = 0; i < nc; i++)
    retval[i] = elem (r, i);

  while (--nc >= 0)
    if (retval[nc])
      break;

  retval.resize (nc+1);

  return retval;
}

charMatrix
charMatrix::transpose (void) const
{
  int nr = rows ();
  int nc = cols ();
  charMatrix result (nc, nr);
  if (length () > 0)
    {
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result.elem (j, i) = elem (i, j);
    }
  return result;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
