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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "defun.h"
#include "error.h"
#include "oct-obj.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-mat.h"
#include "pt-walk.h"
#include "utils.h"
#include "ov.h"
#include "variables.h"

// If TRUE, print a warning message for empty elements in a matrix list.
static bool Vwarn_empty_list_elements;

// The character to fill with when creating string arrays.
char Vstring_fill_char = ' ';

// General matrices.  This list type is much more work to handle than
// constant matrices, but it allows us to construct matrices from
// other matrices, variables, and functions.

// But first, some internal classes that make our job much easier.

class
tm_row_const
{
private:

  class
  tm_row_const_rep : public octave_base_list<octave_value>
  {
  public:

    tm_row_const_rep (void)
      : count (1), dv (),
	all_str (false), some_str (false), is_cmplx (false),
	all_mt (true), ok (false) { }

    tm_row_const_rep (const tree_argument_list& row)
      : count (1), dv (),
	all_str (false), some_str (false), is_cmplx (false),
	all_mt (true), ok (false)
    { init (row); }

    ~tm_row_const_rep (void) { }

    int count;

    dim_vector dv;

    bool all_str;
    bool some_str;
    bool is_cmplx;
    bool all_mt;

    bool ok;

    bool do_init_element (tree_expression *, const octave_value&, bool&);

    void init (const tree_argument_list&);

  private:

    tm_row_const_rep (const tm_row_const_rep&);

    tm_row_const_rep& operator = (const tm_row_const_rep&);

    void eval_error (const char *msg, int l, int c,
		     int x = -1, int y = -1) const;

    void eval_warning (const char *msg, int l, int c) const;
  };

public:

  typedef tm_row_const_rep::iterator iterator;
  typedef tm_row_const_rep::const_iterator const_iterator;

  tm_row_const (void)
    : rep (0) { }

  tm_row_const (const tree_argument_list& row)
    : rep (new tm_row_const_rep (row)) { }

  tm_row_const (const tm_row_const& x)
    : rep (x.rep)
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

  int rows (void)
  { return (rep->dv.length () > 0 ? rep->dv(0) : 0); }

  int cols (void)
  { return (rep->dv.length () > 1 ? rep->dv(1) : 0); }

  dim_vector dims (void) { return rep->dv; }

  bool all_strings_p (void) const { return rep->all_str; }
  bool some_strings_p (void) const { return rep->some_str; }
  bool complex_p (void) const { return rep->is_cmplx; }
  bool all_empty_p (void) const { return rep->all_mt; }

  operator bool () const { return (rep && rep->ok); }

  iterator begin (void) { return rep->begin (); }
  const_iterator begin (void) const { return rep->begin (); }

  iterator end (void) { return rep->end (); }
  const_iterator end (void) const { return rep->end (); }

private:

  tm_row_const_rep *rep;
};

bool
tm_row_const::tm_row_const_rep::do_init_element (tree_expression *elt,
						 const octave_value& val,
						 bool& first_elem)
{
  int this_elt_nr = val.rows ();
  int this_elt_nc = val.columns ();

  dim_vector this_elt_dv = val.dims ();

  if (!this_elt_dv.all_zero ())
    {
      all_mt = false;

      if (first_elem)
	{
	  first_elem = false;

	  dv.resize (this_elt_dv.length ());
	  for (int i = 2; i < dv.length (); i++)
	    dv.elem (i) = this_elt_dv.elem (i);

	  dv.elem (0) = this_elt_nr;

	  dv.elem (1) = 0;
	}
      else
	{
	  int len = (this_elt_dv.length () < dv.length ()
		     ? this_elt_dv.length () : dv.length ());

	  if (this_elt_nr != dv (0))
	    {
	      eval_error ("number of rows must match",
			  elt->line (), elt->column (), this_elt_nr, dv (0));
	      return false;
	    }
	  for (int i = 2; i < len; i++)
	    {
	      if (this_elt_dv (i) != dv (i))
		{
		  eval_error ("dimensions mismatch", elt->line (), elt->column (), this_elt_dv (i), dv (i));
		  return false;
		}
	    }

	  if (this_elt_dv.length () > len)
	    for (int i = len; i < this_elt_dv.length (); i++)
	      if (this_elt_dv (i) != 1)
		{
		  eval_error ("dimensions mismatch", elt->line (), elt->column (), this_elt_dv (i), 1);
		  return false;
		}

	  if (dv.length () > len)
	    for (int i = len; i < dv.length (); i++)
	      if (dv (i) != 1)
		{
		  eval_error ("dimensions mismatch", elt->line (), elt->column (), 1, dv (i));
		  return false;
		}
	}
      dv.elem (1) = dv.elem (1) + this_elt_nc;

    }
  else if (Vwarn_empty_list_elements)
    eval_warning ("empty matrix found in matrix list",
		  elt->line (), elt->column ());

  append (val);

  if (all_str && ! val.is_string ())
    all_str = false;

  if (! some_str && val.is_string ())
    some_str = true;

  if (! is_cmplx && val.is_complex_type ())
    is_cmplx = true;

  return true;
}

void
tm_row_const::tm_row_const_rep::init (const tree_argument_list& row)
{
  all_str = true;

  bool first_elem = true;

  for (tree_argument_list::const_iterator p = row.begin ();
       p != row.end ();
       p++)
    {
      tree_expression *elt = *p;

      octave_value tmp = elt->rvalue ();

      if (error_state || tmp.is_undefined ())
	break;
      else
	{
	  if (tmp.is_cs_list ())
	    {
	      octave_value_list tlst = tmp.list_value ();

	      for (int i = 0; i < tlst.length (); i++)
		{
		  if (! do_init_element (elt, tlst(i), first_elem))
		    goto done;
		}
	    }
	  else
	    {
	      if (! do_init_element (elt, tmp, first_elem))
		goto done;
	    }
	}
    }

 done:

  ok = ! error_state;
}

void
tm_row_const::tm_row_const_rep::eval_error (const char *msg, int l,
					    int c, int x, int y) const
{
  if (l == -1 && c == -1)
    {
      if (x == -1 || y == -1)
	::error ("%s", msg);
      else
	::error ("%s (%d != %d)", msg, x, y);
    }
  else
    {
      if (x == -1 || y == -1)
	::error ("%s near line %d, column %d", msg, l, c);
      else
	::error ("%s (%d != %d) near line %d, column %d", msg, x, y, l, c);
    }
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

class
tm_const : public octave_base_list<tm_row_const>
{
public:

  tm_const (const tree_matrix& tm)
    : dv (), all_str (false), some_str (false), is_cmplx (false),
      all_mt (true), ok (false)
      { init (tm); }

  ~tm_const (void) { }

  int rows (void) const { return (dv.length () > 0 ? dv.elem (0) : 0); }
  int cols (void) const { return (dv.length () > 1 ? dv.elem (1) : 0); }

  dim_vector dims (void) const { return dv; }

  bool all_strings_p (void) const { return all_str; }
  bool some_strings_p (void) const { return some_str; }
  bool complex_p (void) const { return is_cmplx; }
  bool all_empty_p (void) const { return all_mt; }

  operator bool () const { return ok; }

private:

  dim_vector dv;

  bool all_str;
  bool some_str;
  bool is_cmplx;
  bool all_mt;

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

  for (tree_matrix::const_iterator p = tm.begin (); p != tm.end (); p++)
    {
      tree_argument_list *elt = *p;

      tm_row_const tmp (*elt);

      if (tmp)
	{
	  if (all_str && ! tmp.all_strings_p ())
	    all_str = false;

	  if (! some_str && tmp.some_strings_p ())
	    some_str = true;

	  if (! is_cmplx && tmp.complex_p ())
	    is_cmplx = true;

	  if (all_mt && ! tmp.all_empty_p ())
	    all_mt = false;

	  append (tmp);
	}
      else
	break;
    }

  if (! error_state)
    {
      for (iterator p = begin (); p != end (); p++)
	{
	  tm_row_const elt = *p;

	  int this_elt_nr = elt.rows ();
	  int this_elt_nc = elt.cols ();

	  dim_vector this_elt_dv = elt.dims ();

	  if (!this_elt_dv.all_zero ())
	    {
	      all_mt = false;

	      if (first_elem)
		{
		  first_elem = false;

		  dv.resize (this_elt_dv.length ());
		  for (int i = 2; i < dv.length (); i++)
		    dv.elem (i) = this_elt_dv.elem (i);

		  dv.elem (0) = 0;

		  dv.elem (1) = this_elt_nc;
		}
	      else if (all_str)
		{
		  if (this_elt_nc > cols ())
		    dv.elem (1) = this_elt_nc;
		}
	      else
		{
		  bool get_out = false;
		  int len = (this_elt_dv.length () < dv.length ()
			     ? this_elt_dv.length () : dv.length ());

		  for (int i = 1; i < len; i++)
		    {
		      if (i == 1 && this_elt_nc != dv (1))
			{
			  ::error ("number of columns must match (%d != %d)",
				   this_elt_nc, dv (1));
			  get_out = true;
			  break;
			}
		      else if (this_elt_dv (i) != dv (i))
			{
			  ::error ("dimensions mismatch (dim = %i, %d != %d)", i+1, this_elt_dv (i), dv (i));
			  get_out = true;
			  break;
			}
		    }

		  if (this_elt_dv.length () > len)
		    for (int i = len; i < this_elt_dv.length (); i++)
		      if (this_elt_dv (i) != 1)
			{
			  ::error ("dimensions mismatch (dim = %i, %d != %d)", i+1, this_elt_dv (i), 1);
			  get_out = true;
			  break;
			}

		  if (dv.length () > len)
		    for (int i = len; i < dv.length (); i++)
		      if (dv (i) != 1)
			{
			  ::error ("dimensions mismatch (dim = %i, %d != %d)", i+1, 1, dv(i));
			  get_out = true;
			  break;
			}

		  if (get_out)
		    break;
		}
	      dv.elem (0) = dv.elem (0) + this_elt_nr;
	    }
	  else if (Vwarn_empty_list_elements)
	    warning ("empty matrix found in matrix list");
	}
    }

  ok = ! error_state;
}

tree_matrix::~tree_matrix (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

bool
tree_matrix::has_magic_end (void) const
{
  for (const_iterator p = begin (); p != end (); p++)
    {
      tree_argument_list *elt = *p;

      if (elt && elt->has_magic_end ())
	return true;
    }

  return false;
}

bool
tree_matrix::all_elements_are_constant (void) const
{
  for (const_iterator p = begin (); p != end (); p++)
    {
      tree_argument_list *elt = *p;

      if (! elt->all_elements_are_constant ())
	return false;
    }

  return true;
}

octave_value_list
tree_matrix::rvalue (int nargout)
{
  octave_value_list retval;

  MAYBE_DO_BREAKPOINT;

  if (nargout > 1)
    error ("invalid number of output arguments for matrix list");
  else
    retval = rvalue ();

  return retval;
}

octave_value
tree_matrix::rvalue (void)
{
  octave_value retval;

  bool all_strings_p = false;
  bool all_empty_p = false;
  bool frc_str_conv = false;

  tm_const tmp (*this);

  if (tmp)
    {
      dim_vector dv = tmp.dims ();
      all_strings_p = tmp.all_strings_p ();
      all_empty_p = tmp.all_empty_p ();
      frc_str_conv = tmp.some_strings_p ();

      // XXX FIXME XX
      // The previous version of this code obtained the return type and 
      // initialized an array of the correct type. However the return type
      // is now built-up from the return types of do_cat_op. Should we special
      // case the situation where there are only NDArray and ComplexNDArray
      // elements, or things like boolMatrix that widen to them, and do the
      // correct initialization? How to do this? Will it be faster? Check against
      // version 2.1.57


      // The line below might seem crazy, since we take a copy
      // of the first argument, resize it to be empty and then resize
      // it to be full. This is done since it means that there is no
      // recopying of data, as would happen if we used a single resize.
      // It should be noted that resize operation is also significantly 
      // slower than the do_cat_op function, so it makes sense to have an
      // empty matrix and copy all data.
      //
      // We might also start with a empty octave_value using
      //    ctmp = octave_value_typeinfo::lookup_type
      //          (tmp.begin() -> begin() -> type_name());
      // and then directly resize. However, for some types there might be
      // some additional setup needed, and so this should be avoided.

      octave_value ctmp;
      if (all_strings_p)
	if (all_empty_p)
	  ctmp = octave_value (charNDArray (), true);
	else
	  ctmp = octave_value (charNDArray (dv, Vstring_fill_char), true);
      else
	{
	  if (all_empty_p)
	    ctmp = (*(tmp.begin() -> begin()));
	  else
	    ctmp = (*(tmp.begin() -> begin())).resize (dim_vector (0,0)).resize (dv);
	}

      if (error_state)
	goto done;

      // Now, extract the values from the individual elements and
      // insert them in the result matrix.
      Array<int> ra_idx (dv.length (), 0);
      for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
	{
	  tm_row_const row = *p;
	  for (tm_row_const::iterator q = row.begin (); q != row.end (); q++)
	    {
	      octave_value elt = *q;

	      ctmp = do_cat_op (ctmp, elt, ra_idx);
	      if (error_state)
		goto done;
	      ra_idx (1) += elt.columns ();
	    }
	  ra_idx (0) += row.rows ();
	  ra_idx (1) = 0;
	}
      retval = ctmp;
      if (frc_str_conv && ! retval.is_string ())
	retval = retval.convert_to_str ();
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
warn_empty_list_elements (void)
{
  Vwarn_empty_list_elements = check_preference ("warn_empty_list_elements");

  return 0;
}

static int
string_fill_char (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("string_fill_char");

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
  DEFVAR (string_fill_char, " ", string_fill_char,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} string_fill_char\n\
The value of this variable is used to pad all strings in a string matrix\n\
to the same length.  It should be a single character.  The default value\n\
is @code{\" \"} (a single space).  For example,\n\
\n\
@example\n\
@group\n\
string_fill_char = \"X\";\n\
[ \"these\"; \"are\"; \"strings\" ]\n\
     @result{} \"theseXX\"\n\
        \"areXXXX\"\n\
        \"strings\"\n\
@end group\n\
@end example\n\
@end defvr");

  DEFVAR (warn_empty_list_elements, false, warn_empty_list_elements,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_empty_list_elements\n\
If the value of @code{warn_empty_list_elements} is nonzero, print a\n\
warning when an empty matrix is found in a matrix list.  For example,\n\
\n\
@example\n\
a = [1, [], 3, [], 5]\n\
@end example\n\
\n\
@noindent\n\
The default value is 0.\n\
@end defvr");

}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
