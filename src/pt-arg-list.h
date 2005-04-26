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

#if !defined (octave_tree_arg_list_h)
#define octave_tree_arg_list_h 1

class octave_value_list;

class tree_expression;

class tree_walker;

#include "str-vec.h"

#include "base-list.h"

// Argument lists.  Used to hold the list of expressions that are the
// arguments in a function call or index expression.

class
tree_argument_list : public octave_base_list<tree_expression *>
{
public:

  typedef tree_expression* element_type;

  tree_argument_list (void)
    : list_includes_magic_end (false) { }

  tree_argument_list (tree_expression *t)
    : list_includes_magic_end (false) { append (t); }

  ~tree_argument_list (void);

  bool has_magic_end (void) const;

  tree_expression *remove_front (void)
    {
      iterator p = begin ();
      tree_expression *retval = *p;
      erase (p);
      return retval;
    }

  void append (const element_type& s);

  int nargout_count (void) const;

  bool all_elements_are_constant (void) const;

  octave_value_list convert_to_const_vector (const octave_value *object = 0);

  string_vector get_arg_names (void) const;

  void accept (tree_walker& tw);

private:

  bool list_includes_magic_end;

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
