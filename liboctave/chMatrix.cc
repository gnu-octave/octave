// Matrix manipulations.
/*

Copyright (C) 1996, 1997 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <string>

#include "lo-error.h"
#include "str-vec.h"
#include "mx-base.h"
#include "mx-inlines.cc"

// charMatrix class.

charMatrix::charMatrix (char c)
  : MArray2<char> ()
{
  int nc = 1;
  int nr = 1;

  resize (nr, nc);

  elem (0, 0) = c;
}

charMatrix::charMatrix (const char *s)
  : MArray2<char> ()
{
  int nc = s ? strlen (s) : 0;
  int nr = s && nc > 0 ? 1 : 0;

  resize (nr, nc);

  for (int i = 0; i < nc; i++)
    elem (0, i) = s[i];
}

charMatrix::charMatrix (const std::string& s)
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

  return mx_inline_equal (data (), a.data (), length ());
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

std::string
charMatrix::row_as_string (int r, bool strip_ws, bool raw) const 
{
  std::string retval;

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

  if (! raw)
    {
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
    }

  return retval;
}

charMatrix
charMatrix::extract (int r1, int c1, int r2, int c2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_r = r2 - r1 + 1;
  int new_c = c2 - c1 + 1;

  charMatrix result (new_r, new_c);

  for (int j = 0; j < new_c; j++)
    for (int i = 0; i < new_r; i++)
      result.elem (i, j) = elem (r1+i, c1+j);

  return result;
}

// XXX FIXME XXX Do these really belong here?  Maybe they should be
// in a base class?

boolMatrix
charMatrix::all (int dim) const
{
  MX_ALL_OP (dim);
}

boolMatrix
charMatrix::any (int dim) const
{
  MX_ANY_OP (dim);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
