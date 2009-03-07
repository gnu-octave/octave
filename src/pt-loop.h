/*

Copyright (C) 1996, 1997, 2000, 2002, 2004, 2005, 2006, 2007, 2008, 2009
              John W. Eaton

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

#if !defined (octave_tree_loop_h)
#define octave_tree_loop_h 1

class octave_value;
class octave_lvalue;

class tree_argument_list;
class tree_expression;
class tree_statement_list;

class tree_walker;

#include "comment-list.h"
#include "pt-cmd.h"
#include "symtab.h"

// While.

class
tree_while_command : public tree_command
{
public:

  tree_while_command (int l = -1, int c = -1)
    : tree_command (l, c), expr (0), list (0), lead_comm (0),
      trail_comm (0) { }

  tree_while_command (tree_expression *e,
		      octave_comment_list *lc = 0,
		      octave_comment_list *tc = 0,
		      int l = -1, int c = -1)
    : tree_command (l, c), expr (e), list (0), lead_comm (lc),
      trail_comm (tc) { }

  tree_while_command (tree_expression *e, tree_statement_list *lst,
		      octave_comment_list *lc = 0,
		      octave_comment_list *tc = 0,
		      int l = -1, int c = -1)
    : tree_command (l, c), expr (e), list (lst), lead_comm (lc),
      trail_comm (tc) { }

  ~tree_while_command (void);

  tree_expression *condition (void) { return expr; }

  tree_statement_list *body (void) { return list; }

  octave_comment_list *leading_comment (void) { return lead_comm; }

  octave_comment_list *trailing_comment (void) { return trail_comm; }

  tree_command *dup (symbol_table::scope_id scope,
		     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

protected:

  // Expression to test.
  tree_expression *expr;

  // List of commands to execute.
  tree_statement_list *list;

  // Comment preceding WHILE token.
  octave_comment_list *lead_comm;

  // Comment preceding ENDWHILE token.
  octave_comment_list *trail_comm;

private:

  // No copying!

  tree_while_command (const tree_while_command&);

  tree_while_command& operator = (const tree_while_command&);
};

// Do-Until.

class
tree_do_until_command : public tree_while_command
{
public:

  tree_do_until_command (int l = -1, int c = -1)
    : tree_while_command (l, c) { }

  tree_do_until_command (tree_expression *e,
			 octave_comment_list *lc = 0,
			 octave_comment_list *tc = 0,
			 int l = -1, int c = -1)
    : tree_while_command (e, lc, tc, l, c) { }

  tree_do_until_command (tree_expression *e, tree_statement_list *lst,
			 octave_comment_list *lc = 0,
			 octave_comment_list *tc = 0,
			 int l = -1, int c = -1)
    : tree_while_command (e, lst, lc, tc, l, c) { }

  ~tree_do_until_command (void) { }

  tree_command *dup (symbol_table::scope_id scope,
		     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // No copying!

  tree_do_until_command (const tree_do_until_command&);

  tree_do_until_command& operator = (const tree_do_until_command&);
};

// For.

class
tree_simple_for_command : public tree_command
{
public:

  tree_simple_for_command (int l = -1, int c = -1)
    : tree_command (l, c), lhs (0), expr (0), list (0), lead_comm (0),
      trail_comm (0) { }

  tree_simple_for_command (tree_expression *le, tree_expression *re,
			   tree_statement_list *lst,
			   octave_comment_list *lc = 0,
			   octave_comment_list *tc = 0,
			   int l = -1, int c = -1)
    : tree_command (l, c), lhs (le), expr (re), list (lst),
      lead_comm (lc), trail_comm (tc) { }

  ~tree_simple_for_command (void);

  tree_expression *left_hand_side (void) { return lhs; }

  tree_expression *control_expr (void) { return expr; }

  tree_statement_list *body (void) { return list; }

  octave_comment_list *leading_comment (void) { return lead_comm; }

  octave_comment_list *trailing_comment (void) { return trail_comm; }

  tree_command *dup (symbol_table::scope_id scope,
		     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // Expression to modify.
  tree_expression *lhs;

  // Expression to evaluate.
  tree_expression *expr;

  // List of commands to execute.
  tree_statement_list *list;

  // Comment preceding FOR token.
  octave_comment_list *lead_comm;

  // Comment preceding ENDFOR token.
  octave_comment_list *trail_comm;

  // No copying!

  tree_simple_for_command (const tree_simple_for_command&);

  tree_simple_for_command& operator = (const tree_simple_for_command&);
};

class
tree_complex_for_command : public tree_command
{
public:

  tree_complex_for_command (int l = -1, int c = -1)
    : tree_command (l, c), lhs (0), expr (0), list (0), lead_comm (0),
      trail_comm (0) { }

  tree_complex_for_command (tree_argument_list *le, tree_expression *re,
			    tree_statement_list *lst,
			    octave_comment_list *lc = 0,
			    octave_comment_list *tc = 0,
			    int l = -1, int c = -1)
    : tree_command (l, c), lhs (le), expr (re), list (lst),
      lead_comm (lc), trail_comm (tc) { }

  ~tree_complex_for_command (void);

  tree_argument_list *left_hand_side (void) { return lhs; }

  tree_expression *control_expr (void) { return expr; }

  tree_statement_list *body (void) { return list; }

  octave_comment_list *leading_comment (void) { return lead_comm; }

  octave_comment_list *trailing_comment (void) { return trail_comm; }

  tree_command *dup (symbol_table::scope_id scope,
		     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // Expression to modify.
  tree_argument_list *lhs;

  // Expression to evaluate.
  tree_expression *expr;

  // List of commands to execute.
  tree_statement_list *list;

  // Comment preceding FOR token.
  octave_comment_list *lead_comm;

  // Comment preceding ENDFOR token.
  octave_comment_list *trail_comm;

  // No copying!

  tree_complex_for_command (const tree_complex_for_command&);

  tree_complex_for_command& operator = (const tree_complex_for_command&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
