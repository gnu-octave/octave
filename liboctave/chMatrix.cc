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

#include <string>

#include <iostream.h>

#include "lo-error.h"
#include "str-vec.h"
#include "mx-base.h"
#include "mx-inlines.cc"

// charMatrix class.

charMatrix::charMatrix (const char *s)
  : MArray2<char> ()
{
  int nc = s ? strlen (s) : 0;
  int nr = s && nc > 0 ? 1 : 0;

  resize (nr, nc);

  for (int i = 0; i < nc; i++)
    elem (0, i) = s[i];
}

charMatrix::charMatrix (const string& s)
  : MArray2<char> ()
{
  int nc = s.length ();
  int nr = nc > 0 ? 1 : 0;

  resize (nr, nc);

  for (int i = 0; i < nc; i++)
    elem (0, i) = s[i];
}

charMatrix::charMatrix (const string_vector& s)
  : MArray2<char> (s.length (), s.max_length (), 0)
{
  int nr = rows ();

  for (int i = 0; i < nr; i++)
    {
      int nc = s[i].length ();
      for (int j = 0; j < nc; j++)
	elem (i, j) = s[i][j];
    }
}

bool
charMatrix::operator == (const charMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return equal (data (), a.data (), length ());
}

bool
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
charMatrix::row_as_string (int r, bool strip_ws = false) const 
{
  string retval;

  int nr = rows ();
  int nc = cols ();

  if (r == 0 && nr == 0 && nc == 0)
    return retval;

  if (r < 0 || r >= nr)
    {
      (*current_liboctave_error_handler) ("range error for row_as_string");
      return retval;
    }

  retval.resize (nc, '\0');

  for (int i = 0; i < nc; i++)
    retval[i] = elem (r, i);

  if (strip_ws)
    {
      while (--nc >= 0)
	{
	  char c = retval[nc];
	  if (c && c != ' ')
	    break;
	}
    }
  else
    {
      while (--nc >= 0)
	if (retval[nc])
	  break;
    }

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
