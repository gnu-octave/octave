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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_tree_mat_h)
#define octave_tree_mat_h 1

#include <iostream>

class octave_value;
class octave_value_list;
class tree_argument_list;

class tree_walker;

#include "base-list.h"
#include "pt-exp.h"

// General matrices.  This allows us to construct matrices from
// other matrices, variables, and functions.

class
tree_matrix : public tree_expression,
	      public octave_base_list<tree_argument_list *>
{
public:

  tree_matrix (tree_argument_list *row = 0, int l = -1, int c = -1)
    : tree_expression (l, c)
  {
    if (row)
      append (row);
  }

  ~tree_matrix (void);

  bool has_magic_end (void) const;

  bool all_elements_are_constant (void) const;

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  void accept (tree_walker& tw);

private:

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
