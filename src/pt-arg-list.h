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

#if !defined (octave_tree_arg_list_h)
#define octave_tree_arg_list_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <SLList.h>

class octave_value_list;

class tree_expression;

class tree_walker;

#include "str-vec.h"

// Argument lists.  Used to hold the list of expressions that are the
// arguments in a function call or index expression.

class
tree_argument_list
{
public:

  tree_argument_list (void)
    : lst () { }

  tree_argument_list (tree_expression *t)
    : lst () { lst.append (t); }

  ~tree_argument_list (void);

  int length (void) const { return lst.length (); }

  void append (tree_expression *&s) { lst.append (s); }
  void append (tree_expression * const &s) { lst.append (s); }

  tree_expression *&operator () (Pix p) { return lst (p); }

  tree_expression * const &operator () (Pix p) const { return lst (p); }

  Pix first (void) const { return lst.first (); }

  void next (Pix& p) const { return lst.next (p); }

  int remove_front (tree_expression *x) { return lst.remove_front (x); }

  tree_expression *remove_front (void) { return lst.remove_front (); }

  int nargout_count (void) const;

  bool all_elements_are_constant (void) const;

  octave_value_list convert_to_const_vector (void);

  string_vector get_arg_names (void) const;

  void accept (tree_walker& tw);

private:

  // The list of argument list elements.
  SLList<tree_expression *> lst;

  // No copying!

  tree_argument_list (const tree_argument_list&);

  tree_argument_list& operator = (const tree_argument_list&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
