// tree-mat.cc                                          -*- C++ -*-
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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>
#include <strstream.h>

#include "error.h"
#include "oct-obj.h"
#include "pt-const.h"
#include "pt-exp.h"
#include "pt-fvc.h"
#include "pt-mat.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "user-prefs.h"

// General matrices.  This list type is much more work to handle than
// constant matrices, but it allows us to construct matrices from
// other matrices, variables, and functions.

tree_matrix::~tree_matrix (void)
{
  delete element;
  delete next;
}

int
tree_matrix::is_matrix_constant (void) const
{
  const tree_matrix *list = this;

  while (list)
    {
      tree_expression *elem = list->element;

      if (! elem->is_constant ())
	return 0;

      list = list->next;
    }

  return 1;
}

tree_matrix *
tree_matrix::chain (tree_expression *t, tree_matrix::dir d)
{
  tree_matrix *tmp = new tree_matrix (t, d);
  tmp->next = this;
  return tmp;
}

tree_matrix *
tree_matrix::reverse (void)
{
  tree_matrix *list = this;
  tree_matrix *next;
  tree_matrix *prev = 0;

  while (list)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return prev;
}

int
tree_matrix::length (void)
{
  tree_matrix *list = this;
  int len = 0;
  while (list)
    {
      len++;
      list = list->next;
    }
  return len;
}

tree_return_list *
tree_matrix::to_return_list (void)
{
  tree_return_list *retval = 0;

  tree_matrix *list;

  for (list = this; list; list = list->next)
    {
      tree_expression *elem = list->element;

      int is_id = elem->is_identifier ();

      int is_idx_expr = elem->is_index_expression ();

      if (is_id || is_idx_expr)
	{
	  tree_index_expression *idx_expr;
	  if (is_id)
	    {
	      tree_identifier *id = (tree_identifier *) elem;
	      idx_expr = new tree_index_expression (id);
	    }
	  else
	    idx_expr = (tree_index_expression *) elem;

	  if (list == this)
	    retval = new tree_return_list (idx_expr);
	  else
	    retval->append (idx_expr);
	}
      else
	{
	  delete retval;
	  retval = 0;
	  break;
	}
    }

  return retval;
}

// Just about as ugly as it gets.

struct const_matrix_list
{
  tree_matrix::dir direction;
  tree_constant elem;
  int nr;
  int nc;
};

// Less ugly than before, anyway.

tree_constant
tree_matrix::eval (int /* print */)
{
  tree_constant retval;

  if (error_state)
    return retval;

  // Just count the elements without looking at them.

  int total_len = length ();

  // Easier to deal with this later instead of a tree_matrix
  // structure.

  const_matrix_list *list = new const_matrix_list [total_len];

  // Stats we want to keep track of.

  int all_strings = 1;

  int found_complex = 0;

  int row_total = 0;
  int col_total = 0;

  int row_height = 0;

  int cols_this_row = 0;

  int first_row = 1;

  int empties_ok = user_pref.empty_list_elements_ok;

  tree_matrix *ptr = this;

  // Stuff for the result matrix or string.  Declared here so that we
  // don't get warnings from gcc about the goto crossing the
  // initialization of these values.

  int put_row = 0;
  int put_col = 0;

  int prev_nr = 0;
  int prev_nc = 0;

  Matrix m;
  ComplexMatrix cm;
  charMatrix chm;

  // Eliminate empties and gather stats.

  int found_new_row_in_empties = 0;

  int len = 0;
  for (int i = 0; i < total_len; i++)
    {
      tree_expression *elem = ptr->element;
      if (! elem)
	{
	  retval = tree_constant (Matrix ());
	  goto done;
	}

      tree_constant tmp = elem->eval (0);
      if (error_state || tmp.is_undefined ())
	{
	  retval = tree_constant ();
	  goto done;
	}

      int nr = tmp.rows ();
      int nc = tmp.columns ();

      dir direct = ptr->direction;

      if (nr == 0 || nc == 0)
	{
	  if (empties_ok < 0)
	    warning ("empty matrix found in matrix list");
	  else if (empties_ok == 0)
	    {
	      ::error ("empty matrix found in matrix list");
	      retval = tree_constant ();
	      goto done;
	    }

	  if (direct == md_down)
	    found_new_row_in_empties = 1;

	  goto next;
	}

      if (found_new_row_in_empties)
	{
	  found_new_row_in_empties = 0;
	  list[len].direction = md_down;
	}
      else
	list[len].direction = direct;

      list[len].elem = tmp;
      list[len].nr = nr;
      list[len].nc = nc;

      if (all_strings && ! tmp.is_string ())
	all_strings = 0;

      if (! found_complex && tmp.is_complex_type ())
	found_complex = 1;

      len++;

    next:

      ptr = ptr->next;
    }

  //  if (all_strings)
  //    cerr << "all strings\n";

  // Compute size of result matrix, and check to see that the dimensions
  // of all the elements will match up properly.

  for (int i = 0; i < len; i++)
    {
      dir direct = list[i].direction;

      int nr = list[i].nr;
      int nc = list[i].nc;

      if (i == 0)
	{
	  row_total = nr;
	  col_total = nc;

	  row_height = nr;
	  cols_this_row = nc;
	}
      else
	{
	  switch (direct)
	    {
	    case md_right:
	      {
		if (nr != row_height)
		  {
		    ::error ("number of rows must match");
		    goto done;
		  }
		else
		  {
		    cols_this_row += nc;

		    if (first_row)
		      col_total = cols_this_row;
		    else if (all_strings && cols_this_row > col_total)
		      col_total = cols_this_row;
		  }
	      }
	      break;

	    case md_down:
	      {
		if (cols_this_row != col_total && ! all_strings)
		  {
		    ::error ("number of columns must match");
		    goto done;
		  }
		first_row = 0;
		row_total += nr;
		row_height = nr;
		cols_this_row = nc;
	      }
	      break;

	    default:
	      panic_impossible ();
	      break;
	    }
	}
    }

  // Don't forget to check to see if the last element will fit.

  if (all_strings && cols_this_row > col_total)
    {
      col_total = cols_this_row;
    }
  else if (cols_this_row != col_total)
    {
      ::error ("number of columns must match");
      goto done;
    }

  // Now, extract the values from the individual elements and insert
  // them in the result matrix.

  if (all_strings)
    chm.resize (row_total, col_total, 0);
  else if (found_complex)
    cm.resize (row_total, col_total, 0.0);
  else
    m.resize (row_total, col_total, 0.0);

  for (int i = 0; i < len; i++)
    {
      tree_constant tmp = list[i].elem;

      int nr = list[i].nr;
      int nc = list[i].nc;

      if (nr == 0 || nc == 0)
	continue;

      if (i == 0)
	{
	  put_row = 0;
	  put_col = 0;
	}
      else
	{
	  switch (list[i].direction)
	    {
	    case md_right:
	      put_col += prev_nc;
	      break;

	    case md_down:
	      put_row += prev_nr;
	      put_col = 0;
	      break;

	    default:
	      panic_impossible ();
	      break;
	    }
	}

      if (found_complex)
	{
	  if (tmp.is_real_scalar ())
	    {
	      cm (put_row, put_col) = tmp.double_value ();
	    }
	  else if (tmp.is_real_matrix () || tmp.is_range ())
	    {
	      cm.insert (tmp.matrix_value (), put_row, put_col);
	    }
	  else if (tmp.is_complex_scalar ())
	    {
	      cm (put_row, put_col) = tmp.complex_value ();
	    }
	  else
	    {
	      ComplexMatrix cm_tmp = tmp.complex_matrix_value ();

	      if (error_state)
		goto done;

	      cm.insert (cm_tmp, put_row, put_col);
	    }
	}
      else
	{
	  if (tmp.is_real_scalar ())
	    {
	      m (put_row, put_col) = tmp.double_value ();
	    }
	  else if (tmp.is_string () && all_strings)
	    {
	      charMatrix chm_tmp = tmp.all_strings ();

	      if (error_state)
		goto done;

	      chm.insert (chm_tmp, put_row, put_col);
	    }
	  else
	    {
	      Matrix m_tmp = tmp.matrix_value ();

	      if (error_state)
		goto done;

	      m.insert (m_tmp, put_row, put_col);
	    }
	}

      prev_nr = nr;
      prev_nc = nc;
    }

  if (all_strings && chm.rows () > 0 && chm.cols () > 0)
    retval = tree_constant (chm, 1);
  else if (found_complex)
    retval = cm;
  else
    retval = m;

 done:
  delete [] list;

  return retval;
}

void
tree_matrix::print_code (ostream& os)
{
  print_code_indent (os);

  if (in_parens)
    os << "(";

  os << "[";

  tree_matrix *list = this;

  while (list)
    {
      list->element->print_code (os);

      list = list->next;

      if (list)
	{
	  switch (list->direction)
	    {
	    case md_right:
	      os << ", ";
	      break;

	    case md_down:
	      os << "; ";
	      break;

	    default:
	      break;
	    }
	}
    }

  os << "]";

  if (in_parens)
    os << ")";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
