// tree-base.h                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#if !defined (_tree_base_h)
#define _tree_base_h 1

// NOTE: don\'t put #pragma interface here because there is no
// corresponding tree-base.cc file that implements this class!

#ifndef NULL_TREE
#define NULL_TREE (tree *)NULL
#endif

#ifndef NULL_TREE_CONST
#define NULL_TREE_CONST (tree_constant *)NULL
#endif

#include <time.h>
#include <assert.h>

class ostream;
class tree_constant;
class tree_identifier;
class tree_argument_list;

/*
 * Base class for the parse tree.
 */
class
tree
{
public:
  enum matrix_dir
    {
      md_none,
      md_right,
      md_down,
    };

  enum expression_type
    {
      unknown,
      assignment,
      simple_assignment,
      multi_assignment,
      add,
      subtract,
      multiply,
      el_mul,
      divide,
      el_div,
      leftdiv,
      el_leftdiv,
      power,
      elem_pow,
      cmp_lt,
      cmp_le,
      cmp_eq,
      cmp_ge,
      cmp_gt,
      cmp_ne,
      and,
      or,
      not,
      unot,
      uminus,
      hermitian,
      transpose,
      colon,
      index,
      increment,
      decrement,
   };

  virtual ~tree (void) { }

// Only the finest cheese...
  virtual int is_identifier (void) const
    { return 0; }

  virtual int is_constant (void) const
    { return 0; }

  virtual int is_builtin (void) const
    { return 0; }

  virtual int is_index_expression (void) const
    { return 0; }

  virtual int is_assignment_expression (void) const
    { return 0; }

  virtual tree *def (void)
    { assert (0); return (tree *) NULL; }
  
  virtual char *name (void)
    { assert (0); return (char *) NULL; }

  virtual int max_expected_args (void)
    { assert (0); return 0; }
  
  virtual void set_print_flag (int print)
    { assert (0); }

  virtual tree_constant assign (tree_constant& t, tree_constant *args,
				int nargs);

  virtual void bump_value (tree::expression_type)
    { assert (0); }

  virtual void stash_m_file_name (char *s)
    { assert (0); }

  virtual void stash_m_file_time (time_t t)
    { assert (0); }

  virtual char *m_file_name (void)
    { return (char *) NULL; }

  virtual time_t time_parsed (void)
    { assert (0); return 0; }

  virtual tree_constant eval (int print) = 0;

  virtual tree_constant *eval (int print, int nargout);

  virtual tree_constant eval (int argc, char **argv, int print);

  virtual tree_constant *eval (const tree_constant *args, int n_in, int nout,
			       int print)
    { assert (0); return NULL_TREE_CONST; }

  virtual int save (ostream& os, int mark_as_global = 0)
    { assert (0); return 0; }

  virtual int line (void) const { return line_num; }
  virtual int column (void) const { return column_num; }

protected:
  int line_num;
  int column_num;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
