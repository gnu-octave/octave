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

#include <ctime>

#include <SLList.h>

#include "error.h"
#include "oct-obj.h"
#include "pt-const.h"
#include "pt-fvc-base.h"

// A base class for objects that can be evaluated with argument lists.

tree_constant
tree_fvc::assign (tree_constant&, const Octave_object&)
{
  panic_impossible ();
  return tree_constant ();
}

string
tree_fvc::name (void) const
{
  string retval;
  panic_impossible ();
  return retval;
}

void
tree_fvc::bump_value (tree_expression::type)
{
  panic_impossible ();
}

tree_constant
tree_fvc::lookup_map_element (SLList<string>&, bool, bool)
{
  static tree_constant retval;

  int l = line ();
  int c = column ();

  if (l == -1 && c == -1)
    ::error ("invalid structure reference");
  else
    ::error ("invalid structure reference near line %d column %d", l, c);

  return retval;
}

time_t
tree_fvc::time_parsed (void)
{
  panic_impossible ();
  return 0;
}

int
tree_fvc::save (ostream&, bool, int)
{
  panic_impossible ();
  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
