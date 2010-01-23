/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
              2005, 2006, 2007, 2008, 2009 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "quit.h"

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

#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

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
      : count (1), dv (0, 0), all_str (false),
        all_sq_str (false), all_dq_str (false),
        some_str (false), all_real (false), all_cmplx (false),
        all_mt (true), any_sparse (false), any_class (false),
        class_nm (), ok (false)
    { }

    tm_row_const_rep (const tree_argument_list& row)
      : count (1), dv (0, 0), all_str (false), all_sq_str (false),
        some_str (false), all_real (false), all_cmplx (false),
        all_mt (true), any_sparse (false), any_class (false),
        class_nm (), ok (false)
    { init (row); }

    ~tm_row_const_rep (void) { }

    int count;

    dim_vector dv;

    bool all_str;
    bool all_sq_str;
    bool all_dq_str;
    bool some_str;
    bool all_real;
    bool all_cmplx;
    bool all_mt;
    bool any_sparse;
    bool any_class;

    std::string class_nm;

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

  octave_idx_type rows (void) { return rep->dv(0); }
  octave_idx_type cols (void) { return rep->dv(1); }

  bool empty (void) const { return rep->empty (); }

  size_t length (void) const { return rep->length (); }

  dim_vector dims (void) { return rep->dv; }

  bool all_strings_p (void) const { return rep->all_str; }
  bool all_sq_strings_p (void) const { return rep->all_sq_str; }
  bool all_dq_strings_p (void) const { return rep->all_dq_str; }
  bool some_strings_p (void) const { return rep->some_str; }
  bool all_real_p (void) const { return rep->all_real; }
  bool all_complex_p (void) const { return rep->all_cmplx; }
  bool all_empty_p (void) const { return rep->all_mt; }
  bool any_sparse_p (void) const { return rep->any_sparse; }
  bool any_class_p (void) const { return rep->any_class; }

  std::string class_name (void) const { return rep->class_nm; }

  operator bool () const { return (rep && rep->ok); }

  iterator begin (void) { return rep->begin (); }
  const_iterator begin (void) const { return rep->begin (); }

  iterator end (void) { return rep->end (); }
  const_iterator end (void) const { return rep->end (); }

private:

  tm_row_const_rep *rep;
};

std::string
get_concat_class (const std::string& c1, const std::string& c2)
{
  std::string retval = octave_base_value::static_class_name ();

  if (c1 == c2)
    retval = c1;
  else if (c1.empty ())
    retval = c2;
  else if (c2.empty ())
    retval = c1;
  else
    {
      bool c1_is_int = (c1 == "int8" || c1 == "uint8"
                        || c1 == "int16" || c1 == "uint16"
                        || c1 == "int32" || c1 == "uint32"
                        || c1 == "int64" || c1 == "uint64");
      bool c2_is_int = (c2 == "int8" || c2 == "uint8"
                        || c2 == "int16" || c2 == "uint16"
                        || c2 == "int32" || c2 == "uint32"
                        || c2 == "int64" || c2 == "uint64");

      bool c1_is_char = (c1 == "char");
      bool c2_is_char = (c2 == "char");

      bool c1_is_double = (c1 == "double");
      bool c2_is_double = (c2 == "double");

      bool c1_is_single = (c1 == "single");
      bool c2_is_single = (c2 == "single");

      bool c1_is_logical = (c1 == "logical");
      bool c2_is_logical = (c2 == "logical");

      bool c1_is_built_in_type
        = (c1_is_int || c1_is_char || c1_is_double || c1_is_single
           || c1_is_logical);

      bool c2_is_built_in_type
        = (c2_is_int || c2_is_char ||  c2_is_double || c2_is_single
           || c2_is_logical);

      // Order is important here...

      if (c1_is_char && c2_is_built_in_type)
        retval = c1;
      else if (c2_is_char && c1_is_built_in_type)
        retval = c2;
      else if (c1_is_int && c2_is_built_in_type)
        retval = c1;
      else if (c2_is_int && c1_is_built_in_type)
        retval = c2;
      else if (c1_is_single && c2_is_built_in_type)
        retval = c1;
      else if (c2_is_single && c1_is_built_in_type)
        retval = c2;
      else if (c1_is_double && c2_is_built_in_type)
        retval = c1;
      else if (c2_is_double && c1_is_built_in_type)
        retval = c2;
      else if (c1_is_logical && c2_is_logical)
        retval = c1;
    }

  return retval;    
}

bool
tm_row_const::tm_row_const_rep::do_init_element (tree_expression *elt,
                                                 const octave_value& val,
                                                 bool& first_elem)
{
  octave_idx_type this_elt_nr = val.rows ();
  octave_idx_type this_elt_nc = val.columns ();

  std::string this_elt_class_nm = val.class_name ();

  dim_vector this_elt_dv = val.dims ();

  class_nm = get_concat_class (class_nm, this_elt_class_nm);


  if (! this_elt_dv.all_zero ())
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
  else
    eval_warning ("empty matrix found in matrix list",
                  elt->line (), elt->column ());

  append (val);

  if (all_str && ! val.is_string ())
    all_str = false;

  if (all_sq_str && ! val.is_sq_string ())
    all_sq_str = false;

  if (all_dq_str && ! val.is_dq_string ())
    all_dq_str = false;

  if (! some_str && val.is_string ())
    some_str = true;

  if (all_real && ! val.is_real_type ())
    all_real = false;

  if (all_cmplx && ! (val.is_complex_type () || val.is_real_type ()))
    all_cmplx = false;

  if (!any_sparse && val.is_sparse_type ())
    any_sparse = true;

  if (!any_class && val.is_object ())
    any_class = true;

  return true;
}

void
tm_row_const::tm_row_const_rep::init (const tree_argument_list& row)
{
  all_str = true;
  all_sq_str = true;
  all_dq_str = true;
  all_real = true;
  all_cmplx = true;
  any_sparse = false;
  any_class = false;

  bool first_elem = true;

  for (tree_argument_list::const_iterator p = row.begin ();
       p != row.end ();
       p++)
    {
      octave_quit ();

      tree_expression *elt = *p;

      octave_value tmp = elt->rvalue1 ();

      if (error_state || tmp.is_undefined ())
        break;
      else
        {
          if (tmp.is_cs_list ())
            {
              octave_value_list tlst = tmp.list_value ();

              for (octave_idx_type i = 0; i < tlst.length (); i++)
                {
                  octave_quit ();

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
    warning_with_id ("Octave:empty-list-elements", "%s", msg);
  else
    warning_with_id ("Octave:empty-list-elements",
                     "%s near line %d, column %d", msg, l, c);
}

class
tm_const : public octave_base_list<tm_row_const>
{
public:

  tm_const (const tree_matrix& tm)
    : dv (0, 0), all_str (false), all_sq_str (false), all_dq_str (false),
      some_str (false), all_real (false), all_cmplx (false),
      all_mt (true), any_sparse (false), any_class (false),
      class_nm (), ok (false)
  { init (tm); }

  ~tm_const (void) { }

  octave_idx_type rows (void) const { return dv.elem (0); }
  octave_idx_type cols (void) const { return dv.elem (1); }

  dim_vector dims (void) const { return dv; }

  bool all_strings_p (void) const { return all_str; }
  bool all_sq_strings_p (void) const { return all_sq_str; }
  bool all_dq_strings_p (void) const { return all_dq_str; }
  bool some_strings_p (void) const { return some_str; }
  bool all_real_p (void) const { return all_real; }
  bool all_complex_p (void) const { return all_cmplx; }
  bool all_empty_p (void) const { return all_mt; }
  bool any_sparse_p (void) const { return any_sparse; }
  bool any_class_p (void) const { return any_class; }

  std::string class_name (void) const { return class_nm; }

  operator bool () const { return ok; }

private:

  dim_vector dv;

  bool all_str;
  bool all_sq_str;
  bool all_dq_str;
  bool some_str;
  bool all_real;
  bool all_cmplx;
  bool all_mt;
  bool any_sparse;
  bool any_class;

  std::string class_nm;

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
  all_sq_str = true;
  all_dq_str = true;
  all_real = true;
  all_cmplx = true;
  any_sparse = false;
  any_class = false;

  bool first_elem = true;

  // Just eval and figure out if what we have is complex or all
  // strings.  We can't check columns until we know that this is a
  // numeric matrix -- collections of strings can have elements of
  // different lengths.

  for (tree_matrix::const_iterator p = tm.begin (); p != tm.end (); p++)
    {
      octave_quit ();

      tree_argument_list *elt = *p;

      tm_row_const tmp (*elt);

      if (tmp && ! tmp.empty ())
        {
          if (all_str && ! tmp.all_strings_p ())
            all_str = false;

          if (all_sq_str && ! tmp.all_sq_strings_p ())
            all_sq_str = false;

          if (all_dq_str && ! tmp.all_dq_strings_p ())
            all_dq_str = false;

          if (! some_str && tmp.some_strings_p ())
            some_str = true;

          if (all_real && ! tmp.all_real_p ())
            all_real = false;

          if (all_cmplx && ! tmp.all_complex_p ())
            all_cmplx = false;

          if (all_mt && ! tmp.all_empty_p ())
            all_mt = false;

          if (!any_sparse && tmp.any_sparse_p ())
            any_sparse = true;

          if (!any_class && tmp.any_class_p ())
            any_class = true;

          append (tmp);
        }
      else
        break;
    }

  if (! error_state)
    {
      for (iterator p = begin (); p != end (); p++)
        {
          octave_quit ();

          tm_row_const elt = *p;

          octave_idx_type this_elt_nr = elt.rows ();
          octave_idx_type this_elt_nc = elt.cols ();

          std::string this_elt_class_nm = elt.class_name ();

          dim_vector this_elt_dv = elt.dims ();

          if (!this_elt_dv.all_zero ())
            {
              all_mt = false;

              if (first_elem)
                {
                  first_elem = false;

                  class_nm = this_elt_class_nm;

                  dv.resize (this_elt_dv.length ());
                  for (int i = 2; i < dv.length (); i++)
                    dv.elem (i) = this_elt_dv.elem (i);

                  dv.elem (0) = 0;

                  dv.elem (1) = this_elt_nc;
                }
              else if (all_str)
                {
                  class_nm = get_concat_class (class_nm, this_elt_class_nm);

                  if (this_elt_nc > cols ())
                    dv.elem (1) = this_elt_nc;
                }
              else
                {
                  class_nm = get_concat_class (class_nm, this_elt_class_nm);

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
          else
            warning_with_id ("Octave:empty-list-elements",
                             "empty matrix found in matrix list");
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
      octave_quit ();

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
      octave_quit ();

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

  if (nargout > 1)
    error ("invalid number of output arguments for matrix list");
  else
    retval = rvalue1 (nargout);

  return retval;
}

void
maybe_warn_string_concat (bool all_dq_strings_p, bool all_sq_strings_p)
{
  if (! (all_dq_strings_p || all_sq_strings_p))
    warning_with_id ("Octave:string-concat",
                     "concatenation of different character string types may have unintended consequences");
}

template<class TYPE>
static void 
single_type_concat (TYPE& result,
                    tm_const& tmp)
{
  octave_idx_type r = 0, c = 0;

  for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
    {
      tm_row_const row = *p;

      for (tm_row_const::iterator q = row.begin ();
           q != row.end ();
           q++)
        {
          octave_quit ();

          TYPE ra = octave_value_extract<TYPE> (*q);

          if (! error_state)
            {
              result.insert (ra, r, c);

              if (! error_state)
                c += ra.columns ();
              else
                return;
            }
          else
            return;
        }

      r += row.rows ();
      c = 0;
    }
}

template<class TYPE>
static octave_value 
do_single_type_concat (const dim_vector& dv,
                       tm_const& tmp)
{
  TYPE result (dv);

  single_type_concat (result, tmp);

  return result;
}

template<class TYPE, class OV_TYPE>
static octave_value 
do_single_type_concat_no_mutate (const dim_vector& dv,
                                 tm_const& tmp)
{
  TYPE result (dv);

  single_type_concat (result, tmp);

  return new OV_TYPE (result);
}

octave_value
tree_matrix::rvalue1 (int)
{
  octave_value retval = Matrix ();

  bool all_strings_p = false;
  bool all_sq_strings_p = false;
  bool all_dq_strings_p = false;
  bool all_empty_p = false;
  bool all_real_p = false;
  bool all_complex_p = false;
  bool any_sparse_p = false;
  bool any_class_p = false;
  bool frc_str_conv = false;

  tm_const tmp (*this);

  if (tmp && ! tmp.empty ())
    {
      dim_vector dv = tmp.dims ();
      all_strings_p = tmp.all_strings_p ();
      all_sq_strings_p = tmp.all_sq_strings_p ();
      all_dq_strings_p = tmp.all_dq_strings_p ();
      all_empty_p = tmp.all_empty_p ();
      all_real_p = tmp.all_real_p ();
      all_complex_p = tmp.all_complex_p ();
      any_sparse_p = tmp.any_sparse_p ();
      any_class_p = tmp.any_class_p ();
      frc_str_conv = tmp.some_strings_p ();

      // Try to speed up the common cases.

      std::string result_type = tmp.class_name ();

      if (any_class_p)
        {
          octave_value_list tmp3 (tmp.length (), octave_value ());

          int j = 0;
          for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
            {
              octave_quit ();

              tm_row_const row = *p;

              if (row.length () == 1)
                tmp3 (j++) = *(row.begin ());
              else
                {
                  octave_value_list tmp1 (row.length (), octave_value ());

                  int i = 0;
                  for (tm_row_const::iterator q = row.begin (); 
                       q != row.end (); q++)
                    tmp1 (i++) = *q;

                  octave_value_list tmp2;
                  octave_value fcn = 
                    symbol_table::find_function ("horzcat", tmp1);

                  if (fcn.is_defined ())
                    {
                      tmp2 = fcn.do_multi_index_op (1, tmp1);
                      
                      if (error_state)
                        goto done;

                      tmp3 (j++) = tmp2 (0);
                    }
                  else
                    {
                      ::error ("cat not find overloaded horzcat function");
                      goto done;
                    }
                }
            }

          if (tmp.length () == 1)
            retval = tmp3 (0);
          else
            {
              octave_value_list tmp2;
              octave_value fcn = symbol_table::find_function ("vertcat", tmp3);

              if (fcn.is_defined ())
                {
                  tmp2 = fcn.do_multi_index_op (1, tmp3);
                      
                  if (! error_state)
                    retval = tmp2 (0);
                }
              else
                ::error ("cat not find overloaded vertcat function");
            }
        }
      else if (result_type == "double")
        {
          if (any_sparse_p)
            {       
              if (all_real_p)
                retval = do_single_type_concat<SparseMatrix> (dv, tmp);
              else
                retval = do_single_type_concat_no_mutate<SparseComplexMatrix,
                                octave_sparse_complex_matrix> (dv, tmp);
            }
          else
            {
              if (all_real_p)
                retval = do_single_type_concat<NDArray> (dv, tmp);
              else
                retval = do_single_type_concat_no_mutate<ComplexNDArray,
                                octave_complex_matrix> (dv, tmp);
            }
        }
      else if (result_type == "single")
        {
          if (all_real_p)
            retval = do_single_type_concat<FloatNDArray> (dv, tmp);
          else
            retval = do_single_type_concat_no_mutate<FloatComplexNDArray,
                        octave_float_complex_matrix> (dv, tmp);
        }
      else if (result_type == "char")
        {
          char type = all_dq_strings_p ? '"' : '\'';

          maybe_warn_string_concat (all_dq_strings_p, all_sq_strings_p);

          charNDArray result (dv, Vstring_fill_char);

          single_type_concat (result, tmp);

          retval = octave_value (result, type);
        }
      else if (result_type == "logical")
        {
          if (any_sparse_p)
            retval = do_single_type_concat<SparseBoolMatrix> (dv, tmp);
          else
            retval = do_single_type_concat<boolNDArray> (dv, tmp);
        }
      else if (result_type == "int8")
        retval = do_single_type_concat<int8NDArray> (dv, tmp);
      else if (result_type == "int16")
        retval = do_single_type_concat<int16NDArray> (dv, tmp);
      else if (result_type == "int32")
        retval = do_single_type_concat<int32NDArray> (dv, tmp);
      else if (result_type == "int64")
        retval = do_single_type_concat<int64NDArray> (dv, tmp);
      else if (result_type == "uint8")
        retval = do_single_type_concat<uint8NDArray> (dv, tmp);
      else if (result_type == "uint16")
        retval = do_single_type_concat<uint16NDArray> (dv, tmp);
      else if (result_type == "uint32")
        retval = do_single_type_concat<uint32NDArray> (dv, tmp);
      else if (result_type == "uint64")
        retval = do_single_type_concat<uint64NDArray> (dv, tmp);
      else
        {
          // The line below might seem crazy, since we take a copy of
          // the first argument, resize it to be empty and then resize
          // it to be full. This is done since it means that there is
          // no recopying of data, as would happen if we used a single
          // resize.  It should be noted that resize operation is also
          // significantly slower than the do_cat_op function, so it
          // makes sense to have an empty matrix and copy all data.
          //
          // We might also start with a empty octave_value using
          //
          //    ctmp = octave_value_typeinfo::lookup_type
          //          (tmp.begin() -> begin() -> type_name());
          //
          // and then directly resize. However, for some types there
          // might be some additional setup needed, and so this should
          // be avoided.

          octave_value ctmp;

          // Find the first non-empty object

          if (any_sparse_p)
            {
              // Start with sparse matrix to avoid issues memory issues
              // with things like [ones(1,4),sprandn(1e8,4,1e-4)]
              if (all_real_p)
                ctmp = octave_sparse_matrix ().resize (dv); 
              else
                ctmp = octave_sparse_complex_matrix ().resize (dv); 
            }
          else
            {
              for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
                {
                  octave_quit ();

                  tm_row_const row = *p;

                  for (tm_row_const::iterator q = row.begin (); 
                       q != row.end (); q++)
                    {
                      octave_quit ();

                      ctmp = *q;

                      if (! ctmp.all_zero_dims ())
                        goto found_non_empty;
                    }
                }

              ctmp = (*(tmp.begin() -> begin()));

            found_non_empty:

              if (! all_empty_p)
                ctmp = ctmp.resize (dim_vector (0,0)).resize (dv);
            }

          if (! error_state)
            {
              // Now, extract the values from the individual elements and
              // insert them in the result matrix.

              int dv_len = dv.length ();
              Array<octave_idx_type> ra_idx (dv_len > 1 ? dv_len : 2, 1, 0);

              for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
                {
                  octave_quit ();

                  tm_row_const row = *p;

                  for (tm_row_const::iterator q = row.begin ();
                       q != row.end ();
                       q++)
                    {
                      octave_quit ();

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
        }
    }

done:
  return retval;
}

tree_expression *
tree_matrix::dup (symbol_table::scope_id scope,
                  symbol_table::context_id context) const
{
  tree_matrix *new_matrix = new tree_matrix (0, line (), column ());

  for (const_iterator p = begin (); p != end (); p++)
    {
      const tree_argument_list *elt = *p;

      new_matrix->append (elt ? elt->dup (scope, context) : 0);
    }

  new_matrix->copy_base (*this);

  return new_matrix;
}

void
tree_matrix::accept (tree_walker& tw)
{
  tw.visit_matrix (*this);
}

DEFUN (string_fill_char, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} string_fill_char ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} string_fill_char (@var{new_val})\n\
Query or set the internal variable used to pad all rows of a character\n\
matrix to the same length.  It must be a single character.  The default\n\
value is @code{\" \"} (a single space).  For example,\n\
\n\
@example\n\
@group\n\
string_fill_char (\"X\");\n\
[ \"these\"; \"are\"; \"strings\" ]\n\
     @result{} \"theseXX\"\n\
        \"areXXXX\"\n\
        \"strings\"\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (string_fill_char);
}
