// tree.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#if !defined (octave_tree_base_h)
#define octave_tree_base_h 1

#include <stdio.h>
#include <time.h>
#include <assert.h>

#ifndef NULL_TREE
#define NULL_TREE (tree *)NULL
#endif

#ifndef NULL_TREE_CONST
#define NULL_TREE_CONST (tree_constant *)NULL
#endif

class tree;
class tree_fvc;
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
      and_and,
      or_or,
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

  virtual tree_constant eval (int print) = 0;

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
