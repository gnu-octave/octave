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

#if !defined (octave_tree_indirect_ref_h)
#define octave_tree_indirect_ref_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

#include <string>

class octave_value;
class octave_value_list;
class tree_walker;

#include "pt-exp-base.h"

// Indirect references to values (structure references).

class
tree_indirect_ref : public tree_expression
{
public:

  tree_indirect_ref (int l = -1, int c = -1)
    : tree_expression (l, c), expr (0), nm () { }

  tree_indirect_ref (tree_expression *e, const string& n,
		     int l = -1, int c = -1)
    : tree_expression (l, c), expr (e), nm (n) { }

  ~tree_indirect_ref (void);

  bool is_indirect_ref (void) const
    { return true; }

  string name (void) const;

  bool rvalue_ok (void) const
    { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout);

  octave_variable_reference lvalue (void);

  tree_expression *expression (void)
    { return expr; }

  string elt_name (void)
    { return nm; }

  void accept (tree_walker& tw);

private:

  // The LHS of this structure reference.
  tree_expression *expr;

  // The sub-element name.
  string nm;

  void eval_error (void) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
