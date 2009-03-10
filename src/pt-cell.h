/*

Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
              2008, 2009 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_tree_cell_h)
#define octave_tree_cell_h 1

#include <iosfwd>

class octave_value;
class octave_value_list;
class tree_argument_list;

class tree_walker;

#include "pt-mat.h"
#include "symtab.h"

// General cells.

class
tree_cell : public tree_matrix
{
public:

  tree_cell (tree_argument_list *row = 0, int l = -1, int c = -1)
    : tree_matrix (row, l, c) { }

  ~tree_cell (void) { }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int);

  tree_expression *dup (symbol_table::scope_id scope,
			symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // No copying!

  tree_cell (const tree_cell&);

  tree_cell& operator = (const tree_cell&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
