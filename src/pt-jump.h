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

#if !defined (octave_tree_jump_h)
#define octave_tree_jump_h 1

#if defined (__GNUG__) && ! defined (NO_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

class tree_walker;

#include "pt-exp.h"

// Break.

class
tree_break_expression : public tree_expression
{
public:

  tree_break_expression (int l = -1, int c = -1)
    : tree_expression (l, c) { }

  ~tree_break_expression (void) { }

  bool rvalue_ok (void) { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout) { return rvalue (); }

  void accept (tree_walker& tw);

  static int breaking;

private:

  // No copying!

  tree_break_expression (const tree_break_expression&);

  tree_break_expression& operator = (const tree_break_expression&);
};

// Continue.

class
tree_continue_expression : public tree_expression
{
public:

  tree_continue_expression (int l = -1, int c = -1)
    : tree_expression (l, c) { }

  ~tree_continue_expression (void) { }

  bool rvalue_ok (void) { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout) { return rvalue (); }

  void accept (tree_walker& tw);

  static int continuing;

private:

  // No copying!

  tree_continue_expression (const tree_continue_expression&);

  tree_continue_expression& operator = (const tree_continue_expression&);
};

// Return.

class
tree_return_expression : public tree_expression
{
public:

  tree_return_expression (int l = -1, int c = -1)
    : tree_expression (l, c) { }

  ~tree_return_expression (void) { }

  bool rvalue_ok (void) { return true; }

  octave_value rvalue (void);

  octave_value_list rvalue (int nargout) { return rvalue (); }

  void accept (tree_walker& tw);

  static int returning;

private:

  // No copying!

  tree_return_expression (const tree_return_expression&);

  tree_return_expression& operator = (const tree_return_expression&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
