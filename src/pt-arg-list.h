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

#if defined (__GNUG__)
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
tree_argument_list : public SLList<tree_expression *>
{
public:

  tree_argument_list (void)
    : SLList<tree_expression *> () { }

  tree_argument_list (tree_expression *t)
    : SLList<tree_expression *> () { append (t); }

  ~tree_argument_list (void);

  bool all_elements_are_constant (void) const;

  octave_value_list convert_to_const_vector (void);

  string_vector get_arg_names (void) const;

  void accept (tree_walker& tw);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
