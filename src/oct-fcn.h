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

#if !defined (octave_function_h)
#define octave_function_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

#include "oct-sym.h"

// Functions.

// This just provides a way to avoid infinite recursion when building
// octave_function objects.

class octave_function;

class
octave_function : public octave_symbol
{
public:

  octave_function (octave_function *new_rep);

  // Copy constructor.

  octave_function (const octave_function& a)
    {
      rep = a.rep;
      rep->count++;
    }

  // Delete the representation of this constant if the count drops to
  // zero.

  virtual ~octave_function (void);

  // This should only be called for derived types.

  virtual octave_function *clone (void);

  void make_unique (void)
    {
      if (rep->count > 1)
	{
	  --rep->count;
	  rep = rep->clone ();
	  rep->count = 1;
	}
    }

  // Simple assignment.

  octave_function& operator = (const octave_function& a)
    {
      if (rep != a.rep)
	{
	  if (--rep->count == 0)
	    delete rep;

	  rep = a.rep;
	  rep->count++;
	}

      return *this;
    }

  string name (void) const
    { return my_name; }

  string doc_string (void) const
    { return doc; }

  bool is_constant (void) const
    { return false; }

protected:

  octave_function (const string& nm, const string& ds)
    : rep (0), my_name (nm), doc (ds) { }

private:

  octave_function (void);

  union
    {
      octave_function *rep;      // The real representation.
      int count;                 // A reference count.
    };

  // The name of this function.
  string my_name;

  // The help text for this function.
  string doc;
};

#endif

/*
;; Local Variables: ***
;; mode: C++ ***
;; End: ***
*/
