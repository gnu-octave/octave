// tree-const.cc                                         -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined (__GNUG__)
#pragma implementation
#endif

#include <iostream.h>

#include "tree-const.h"
#include "error.h"
#include "gripes.h"
#include "user-prefs.h"

tree_constant::~tree_constant (void)
{
#if defined (MDEBUG)
  cerr << "~tree_constant: rep: " << rep
       << " rep->count: " << rep->count << "\n";
#endif

  if (--rep->count <= 0)
    {
      delete rep;
      rep = 0;
    }
}

#if defined (MDEBUG)
void *
tree_constant::operator new (size_t size)
{
  tree_constant *p = ::new tree_constant;
  cerr << "tree_constant::new(): " << p << "\n";
  return p;
}

void
tree_constant::operator delete (void *p, size_t size)
{
  cerr << "tree_constant::delete(): " << p << "\n";
  ::delete p;
}
#endif

// Construct return vector of empty matrices.  Return empty matrices
// and/or gripe when appropriate.

Octave_object
vector_of_empties (int nargout, const char *fcn_name)
{
  Octave_object retval;

// Got an empty argument, check if should gripe/return empty values.

  int flag = user_pref.propagate_empty_matrices;
  if (flag != 0)
    {
      if (flag < 0)
	gripe_empty_arg (fcn_name, 0);

      Matrix m;
      retval.resize (nargout ? nargout : 1);
      for (int i = 0; i < nargout; i++)
	retval(i) = m;
    }
  else
    gripe_empty_arg (fcn_name, 1);

  return retval;
}

void
tree_constant::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  if (rep)
    rep->print_code (os);

  if (in_parens)
    os << ")";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
