// pt-mat.h                                      -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_tree_mat_h)
#define octave_tree_mat_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

class tree_constant;
class tree_return_list;

#include "pt-exp.h"

// General matrices.  This allows us to construct matrices from
// other matrices, variables, and functions.

class
tree_matrix : public tree_expression
{
public:
  enum dir
    {
      md_none,
      md_right,
      md_down,
    };

  tree_matrix (void)
    : tree_expression (), direction (tree_matrix::md_none),
      element (0), next (0) { }

  tree_matrix (tree_expression *e, tree_matrix::dir d)
    : tree_expression (), direction (d), element (e), next (0) { }

  ~tree_matrix (void);

  int is_matrix_constant (void) const;

  tree_matrix *chain (tree_expression *e, tree_matrix::dir d);
  tree_matrix *reverse (void);
  int length (void);

  tree_return_list *to_return_list (void);

  tree_constant eval (int print);

  void print_code (ostream& os);

private:
  tree_matrix::dir direction; // Direction from the previous element.
  tree_expression *element;
  tree_matrix *next;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
