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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>

#include "lo-error.h"
#include "str-vec.h"
#include "mx-base.h"
#include "mx-inlines.cc"

// boolMatrix class.

bool
boolMatrix::operator == (const boolMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return equal (data (), a.data (), length ());
}

bool
boolMatrix::operator != (const boolMatrix& a) const
{
  return !(*this == a);
}

boolMatrix&
boolMatrix::insert (const boolMatrix& a, int r, int c)
{
  Array2<bool>::insert (a, r, c);
  return *this;
}

// unary operations

boolMatrix
boolMatrix::operator ! (void) const
{
  int nr = rows ();
  int nc = cols ();

  boolMatrix b (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      b.elem (i, j) = ! elem (i, j);

  return b;
}

// other operations

boolMatrix
boolMatrix::all (void) const
{
  int nr = rows ();
  int nc = cols ();
  boolMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = true;
	  for (int j = 0; j < nc; j++)
	    {
	      if (! elem (0, j))
		{
		  retval.elem (0, 0) = false;
		  break;
		}
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = true;
	  for (int i = 0; i < nr; i++)
	    {
	      if (! elem (i, 0))
		{
		  retval.elem (0, 0) = false;
		  break;
		}
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = true;
	      for (int i = 0; i < nr; i++)
		{
		  if (! elem (i, j))
		    {
		      retval.elem (0, j) = false;
		      break;
		    }
		}
	    }
	}
    }
  return retval;
}

boolMatrix
boolMatrix::any (void) const
{
  int nr = rows ();
  int nc = cols ();
  boolMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = false;
	  for (int j = 0; j < nc; j++)
	    {
	      if (elem (0, j))
		{
		  retval.elem (0, 0) = true;
		  break;
		}
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = false;
	  for (int i = 0; i < nr; i++)
	    {
	      if (elem (i, 0))
		{
		  retval.elem (0, 0) = true;
		  break;
		}
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = false;
	      for (int i = 0; i < nr; i++)
		{
		  if (elem (i, j))
		    {
		      retval.elem (0, j) = true;
		      break;
		    }
		}
	    }
	}
    }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
