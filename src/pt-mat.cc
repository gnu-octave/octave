/*

Copyright (C) 1996 John W. Eaton

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

#include "defun.h"
#include "error.h"
#include "oct-obj.h"
#include "pt-exp.h"
#include "pt-fvc.h"
#include "pt-mat.h"
#include "pt-misc.h"
#include "pt-mvr.h"
#include "pt-walk.h"
#include "utils.h"
#include "ov.h"
#include "variables.h"

// Are empty elements in a matrix list ok?  For example, is the empty
// matrix in an expression like `[[], 1]' ok?  A positive value means
// yes.  A negative value means yes, but print a warning message.
// Zero means it should be considered an error.
static int Vempty_list_elements_ok;

// The character to fill with when creating string arrays.
static char Vstring_fill_char;

// General matrices.  This list type is much more work to handle than
// constant matrices, but it allows us to construct matrices from
// other matrices, variables, and functions.

// But first, some internal classes that make our job much easier.

class
tm_row_const
{
private:

  class
  tm_row_const_rep : public SLList<octave_value>
  {
  public:

    tm_row_const_rep (void)
      : SLList<octave_value> (), count (1), nr (0), nc (0),
	all_str (false), is_cmplx (false), ok (false) { }

    tm_row_const_rep (const tree_matrix_row& mr)
      : SLList<octave_value> (), count (1), nr (0), nc (0),
	all_str (false), is_cmplx (false), ok (false)
        { init (mr); }

    ~tm_row_const_rep (void) { }

    int count;

    int nr;
    int nc;

    bool all_str;
    bool is_cmplx;

    bool ok;

    void init (const tree_matrix_row&);

  private:

    tm_row_const_rep (const tm_row_const_rep&);

    tm_row_const_rep& operator =
      (const tm_row_const_rep&);


    void eval_error (const char *msg, int l, int c) const;

    void eval_warning (const char *msg, int l, int c) const;
  };

public:

  tm_row_const (void) : rep (0) { }

  tm_row_const (const tree_matrix_row& mr)
    : rep (new tm_row_const_rep (mr)) { }

  tm_row_const (const tm_row_const& x) : rep (x.rep)
    {
      if (rep)
	rep->count++;
    }

  tm_row_const& operator = (const tm_row_const& x)
    {
      if (this != &x && rep != x.rep)
	{
	  if (rep && --rep->count == 0)
	    delete rep;

	  rep = x.rep;

	  if (rep)
	    rep->count++;
	}

      return *this;
    }

  ~tm_row_const (void)
    {
      if (rep && --rep->count == 0)
	delete rep;
    }

  int rows (void) { return rep->nr; }
  int cols (void) { return rep->nc; }

  bool all_strings (void) const { return rep->all_str; }
  bool is_complex (void) const { return rep->is_cmplx; }

  octave_value& operator () (Pix p) { return rep->operator () (p); }

  const octave_value& operator () (Pix p) const
    { return rep->operator () (p); }

  Pix first (void) const { return rep->first (); }
  void next (Pix& p) const { rep->next (p); }
  
  operator void* () const
    {
      return (rep && rep->ok) ? (void *) -1 : (void *) 0;
    }

private:

  tm_row_const_rep *rep;
};

void
tm_row_const::tm_row_const_rep::init (const tree_matrix_row& mr)
{
  all_str = true;

  bool first_elem = true;

  for (Pix p = mr.first (); p != 0; mr.next (p))
    {
      tree_expression *elt = mr (p);

      octave_value tmp = elt->eval (false);

      if (error_state || tmp.is_undefined ())
	break;
      else
	{
	  int this_elt_nr = tmp.rows ();
	  int this_elt_nc = tmp.columns ();

	  if (this_elt_nr == 0 || this_elt_nc == 0)
	    {
	      if (Vempty_list_elements_ok < 0)
		eval_warning ("empty matrix found in matrix list",
			      elt->line (), elt->column ());
	      else if (Vempty_list_elements_ok == 0)
		{
		  eval_error ("empty matrix found in matrix list",
			      elt->line (), elt->column ());
		  break;
		}
	    }
	  else
	    {
	      if (first_elem)
		{
		  first_elem = false;

		  nr = this_elt_nr;
		}
	      else if (this_elt_nr != nr)
		{
		  eval_error ("number of rows must match",
			      elt->line (), elt->column ());
		  break;
		}

	      nc += this_elt_nc;

	      append (tmp);
	    }

	  if (all_str && ! tmp.is_string ())
	    all_str = false;

	  if (! is_cmplx && tmp.is_complex_type ())
	    is_cmplx = true;
	}
    }

  ok = ! error_state;
}

void
tm_row_const::tm_row_const_rep::eval_error (const char *msg, int l,
					    int c) const
{
  if (l == -1 && c == -1)
    ::error ("%s", msg);
  else
    ::error ("%s near line %d, column %d", msg, l, c);
}

void
tm_row_const::tm_row_const_rep::eval_warning (const char *msg, int l,
					      int c) const
{
  if (l == -1 && c == -1)
    ::warning ("%s", msg);
  else
    ::warning ("%s near line %d, column %d", msg, l, c);
}

template class SLNode<tm_row_const>;
template class SLList<tm_row_const>;

class
tm_const : public SLList<tm_row_const>
{
public:

  tm_const (const tree_matrix& tm)
    : SLList<tm_row_const> (), nr (0), nc (0), all_str (false),
      is_cmplx (false), ok (false)
      { init (tm); }

  ~tm_const (void) { }

  int rows (void) const { return nr; }
  int cols (void) const { return nc; }

  bool all_strings (void) const { return all_str; }
  bool is_complex (void) const { return is_cmplx; }

  operator void* () const { return ok ? (void *) -1 : (void *) 0; }

private:

  int nr;
  int nc;

  bool all_str;
  bool is_cmplx;

  bool ok;

  tm_const (void);

  tm_const (const tm_const&);

  tm_const& operator = (const tm_const&);

  void init (const tree_matrix& tm);
};

void
tm_const::init (const tree_matrix& tm)
{
  all_str = true;

  bool first_elem = true;

  // Just eval and figure out if what we have is complex or all
  // strings.  We can't check columns until we know that this is a
  // numeric matrix -- collections of strings can have elements of
  // different lengths.

  for (Pix p = tm.first (); p != 0; tm.next (p))
    {
      tree_matrix_row *elt = tm (p);

      tm_row_const tmp (*elt);

      if (tmp)
	{
	  if (all_str && ! tmp.all_strings ())
	    all_str = false;

	  if (! is_cmplx && tmp.is_complex ())
	    is_cmplx = true;

	  append (tmp);
	}
      else
	break;
    }

  if (! error_state)
    {
      for (Pix p = first (); p != 0; next (p))
	{
	  tm_row_const elt = this->operator () (p);

	  int this_elt_nr = elt.rows ();
	  int this_elt_nc = elt.cols ();

	  if (this_elt_nr == 0 || this_elt_nc == 0)
	    {
	      if (Vempty_list_elements_ok < 0)
		warning ("empty matrix found in matrix list");
	      else if (Vempty_list_elements_ok == 0)
		{
		  ::error ("empty matrix found in matrix list");
		  break;
		}
	    }
	  else
	    {
	      if (first_elem)
		{
		  first_elem = false;

		  nc = this_elt_nc;
		}
	      else if (all_str)
		{
		  if (this_elt_nc > nc)
		    nc = this_elt_nc;
		}
	      else if (this_elt_nc != nc)
		{
		  ::error ("number of columns must match");
		  break;
		}

	      nr += this_elt_nr;
	    }
	}
    }

  ok = ! error_state;
}

bool
tree_matrix_row::is_matrix_constant (void) const
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_expression *elt = this->operator () (p);

      if (! elt->is_constant ())
	return false;
    }

  return true;
}

tree_return_list *
tree_matrix_row::to_return_list (void)
{
  tree_return_list *retval = 0;

  bool first_elem = true;

  for (Pix p = first (); p != 0; next (p))
    {
      tree_expression *elt = this->operator () (p);

      bool is_id = elt->is_identifier ();

      bool is_idx_expr = elt->is_index_expression ();

      if (is_id || is_idx_expr)
	{
	  tree_index_expression *idx_expr;

	  if (is_id)
	    {
	      tree_identifier *id = (tree_identifier *) elt;
	      idx_expr = new tree_index_expression (id);
	    }
	  else
	    idx_expr = (tree_index_expression *) elt;

	  if (first_elem)
	    {
	      first_elem = false;

	      retval = new tree_return_list (idx_expr);
	    }
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

void
tree_matrix_row::accept (tree_walker& tw)
{
  tw.visit_matrix_row (*this);
}

bool
tree_matrix::is_matrix_constant (void) const
{
  for (Pix p = first (); p != 0; next (p))
    {
      tree_matrix_row *elt = this->operator () (p);

      if (! elt->is_matrix_constant ())
	return false;
    }

  return true;
}

// Just about as ugly as it gets.
// Less ugly than before, anyway.
// Looking better all the time.

octave_value
tree_matrix::eval (bool /* print */)
{
  octave_value retval;

  tm_const tmp (*this);

  if (tmp)
    {
      int nr = tmp.rows ();
      int nc = tmp.cols ();

      Matrix m;
      ComplexMatrix cm;
      charMatrix chm;

      // Now, extract the values from the individual elements and
      // insert them in the result matrix.

      bool all_strings = tmp.all_strings ();
      bool found_complex = tmp.is_complex ();

      if (all_strings)
	chm.resize (nr, nc, Vstring_fill_char);
      else if (found_complex)
	cm.resize (nr, nc, 0.0);
      else
	m.resize (nr, nc, 0.0);

      int put_row = 0;

      for (Pix p = tmp.first (); p != 0; tmp.next (p))
	{
	  int put_col = 0;

	  tm_row_const row = tmp (p);

	  for (Pix q = row.first (); q != 0; row.next (q))
	    {
	      octave_value elt = row (q);

	      if (found_complex)
		{
		  if (elt.is_real_scalar ())
		    cm (put_row, put_col) = elt.double_value ();
		  else if (elt.is_real_matrix () || elt.is_range ())
		    cm.insert (elt.matrix_value (), put_row, put_col);
		  else if (elt.is_complex_scalar ())
		    cm (put_row, put_col) = elt.complex_value ();
		  else
		    {
		      ComplexMatrix cm_elt = elt.complex_matrix_value ();

		      if (error_state)
			goto done;

		      cm.insert (cm_elt, put_row, put_col);
		    }
		}
	      else
		{
		  if (elt.is_real_scalar ())
		    m (put_row, put_col) = elt.double_value ();
		  else if (elt.is_string () && all_strings)
		    {
		      charMatrix chm_elt = elt.all_strings ();

		      if (error_state)
			goto done;

		      chm.insert (chm_elt, put_row, put_col);
		    }
		  else
		    {
		      Matrix m_elt = elt.matrix_value ();

		      if (error_state)
			goto done;

		      m.insert (m_elt, put_row, put_col);
		    }
		}

	      if (all_strings && chm.rows () > 0 && chm.cols () > 0)
		retval = octave_value (chm, true);
	      else if (found_complex)
		retval = cm;
	      else
		retval = m;

	      put_col += elt.columns ();
	    }

	  put_row += row.rows ();
	}
    }

done:

  return retval;
}

void
tree_matrix::accept (tree_walker& tw)
{
  tw.visit_matrix (*this);
}

static int
empty_list_elements_ok (void)
{
  Vempty_list_elements_ok = check_preference ("empty_list_elements_ok");

  return 0;
}

static int
string_fill_char (void)
{
  int status = 0;

  string s = builtin_string_variable ("string_fill_char");

  switch (s.length ())
    {
    case 1:
      Vstring_fill_char = s[0];
      break;

    case 0:
      Vstring_fill_char = '\0';
      break;

    default:
      warning ("string_fill_char must be a single character");
      status = -1;
      break;
    }

  return status;
}

void
symbols_of_pt_mat (void)
{
  DEFVAR (empty_list_elements_ok, "warn", 0, empty_list_elements_ok,
    "ignore the empty element in expressions like `a = [[], 1]'");

  DEFVAR (string_fill_char, " ", 0, string_fill_char,
    "the character to fill with when creating string arrays.");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
