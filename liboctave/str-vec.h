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

#if !defined (octave_str_vec_h)
#define octave_str_vec_h 1

class ostream;

#include <string>

#include "Array.h"

static int
str_vec_compare (const void *a_arg, const void *b_arg)
{
  const string *a = (const string *) a_arg;
  const string *b = (const string *) b_arg;

  return a->compare (*b);
}

class
string_vector : public Array<string>
{
public:

  string_vector (void) : Array<string> () { }

  string_vector (int n) : Array<string> (n) { }

  string_vector (const char *s) : Array<string> (1, s) { }

  string_vector (const string& s) : Array<string> (1, s) { }

  string_vector (const string_vector& s) : Array<string> (s) { }

  string_vector (const char * const *s);

  string_vector (const char * const *s, int n);

  string_vector& operator = (const string_vector& s)
    {
      if (this != &s)
	Array<string>::operator = (s);

      return *this;
    }

  ~string_vector (void) { }

  int empty (void) const { return length () == 0; }

  int max_length (void) const
    {
      int n = length ();
      int longest = 0;

      for (int i = 0; i < n; i++)
	{
	  int tmp = elem(i).length ();

	  if (tmp > longest)
	    longest = tmp;
	}

      return longest;
    }

  string& operator[] (int i) { return Array<string>::elem (i); }

  string operator[] (int i) const { return Array<string>::elem (i); }

  string_vector& qsort (void)
    {
      Array<string>::qsort (str_vec_compare);
      return *this;
    }

  ostream& list_in_columns (ostream&) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
