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

#if !defined (octave_tree_mat_h)
#define octave_tree_mat_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <iostream>

class octave_value;
class octave_value_list;
class tree_argument_list;

class tree_walker;

#include <SLList.h>

#include "pt-exp.h"

// General matrices.  This allows us to construct matrices from
// other matrices, variables, and functions.

class
tree_matrix : public tree_expression
{
public:

  tree_matrix (tree_argument_list *row = 0, int line = -1, int column = -1)
    : tree_expression (line, column), lst ()
  {
    if (row)
      lst.append (row);
  }

  ~tree_matrix (void);

  int length (void) const { return lst.length (); }

  void append (tree_argument_list *&s) { lst.append (s); }
  void append (tree_argument_list * const &s) { lst.append (s); }

  tree_argument_list *&operator () (Pix p) { return lst (p); }

  tree_argument_list * const &operator () (Pix p) const { return lst (p); }

  Pix first (void) const { return lst.first (); }

  void next (Pix& p) const { return lst.next (p); }

  bool all_elements_are_constant (void) const;

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  void accept (tree_walker& tw);

private:

  // The list matrix elements for this row.
  SLList<tree_argument_list *> lst;

  // No copying!

  tree_matrix (const tree_matrix&);

  tree_matrix& operator = (const tree_matrix&);
};

// The character to fill with when creating string arrays.
extern char Vstring_fill_char;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
