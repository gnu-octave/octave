////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2020 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "Cell.h"
#include "error.h"
#include "interpreter-private.h"
#include "oct-map.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-arg-list.h"
#include "pt-eval.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "utils.h"
#include "variables.h"
#include "errwarn.h"

namespace octave
{
  // Index expressions.

  tree_index_expression::tree_index_expression (int l, int c)
    : tree_expression (l, c), m_expr (nullptr), m_args (0), m_type (),
      m_arg_nm (), m_dyn_field (), m_word_list_cmd (false) { }

  tree_index_expression::tree_index_expression (tree_expression *e,
                                                tree_argument_list *lst,
                                                int l, int c, char t)
    : tree_expression (l, c), m_expr (e), m_args (0), m_type (),
      m_arg_nm (), m_dyn_field (), m_word_list_cmd (false)
  {
    append (lst, t);
  }

  tree_index_expression::tree_index_expression (tree_expression *e,
                                                const std::string& n,
                                                int l, int c)
    : tree_expression (l, c), m_expr (e), m_args (0), m_type (),
      m_arg_nm (), m_dyn_field (), m_word_list_cmd (false)
  {
    append (n);
  }

  tree_index_expression::tree_index_expression (tree_expression *e,
                                                tree_expression *df,
                                                int l, int c)
    : tree_expression (l, c), m_expr (e), m_args (0), m_type (),
      m_arg_nm (), m_dyn_field (), m_word_list_cmd (false)
  {
    append (df);
  }

  void
  tree_index_expression::append (tree_argument_list *lst, char t)
  {
    m_args.push_back (lst);
    m_type.append (1, t);
    m_arg_nm.push_back (lst ? lst->get_arg_names () : string_vector ());
    m_dyn_field.push_back (static_cast<tree_expression *> (nullptr));

    if (lst && lst->has_magic_tilde ())
      error ("invalid use of empty argument (~) in index expression");
  }

  void
  tree_index_expression::append (const std::string& n)
  {
    m_args.push_back (static_cast<tree_argument_list *> (nullptr));
    m_type += '.';
    m_arg_nm.push_back (n);
    m_dyn_field.push_back (static_cast<tree_expression *> (nullptr));
  }

  void
  tree_index_expression::append (tree_expression *df)
  {
    m_args.push_back (static_cast<tree_argument_list *> (nullptr));
    m_type += '.';
    m_arg_nm.push_back ("");
    m_dyn_field.push_back (df);
  }

  tree_index_expression::~tree_index_expression (void)
  {
    delete m_expr;

    while (! m_args.empty ())
      {
        auto p = m_args.begin ();
        delete *p;
        m_args.erase (p);
      }

    while (! m_dyn_field.empty ())
      {
        auto p = m_dyn_field.begin ();
        delete *p;
        m_dyn_field.erase (p);
      }
  }

  bool
  tree_index_expression::has_magic_end (void) const
  {
    for (const tree_argument_list *elt : m_args)
      {
        if (elt && elt->has_magic_end ())
          return true;
      }

    return false;
  }

  // This is useful for printing the name of the variable in an indexed
  // assignment.

  std::string
  tree_index_expression::name (void) const
  {
    return m_expr->name ();
  }

  static inline octave_value_list
  make_value_list (tree_evaluator& tw,
                   tree_argument_list *m_args,
                   const string_vector& m_arg_nm, const octave_value *object,
                   bool rvalue = true)
  {
    // FIXME: This function duplicates tree_evaluator::make_value_list.
    // See also the way convert_to_const_vector is used in
    // tree_index_expression::evaluate_n.

    octave_value_list retval;

    if (m_args)
      {
        unwind_action act ([&tw] (const std::list<octave_lvalue> *lvl)
                           {
                             tw.set_lvalue_list (lvl);
                           }, tw.lvalue_list ());
        tw.set_lvalue_list (nullptr);

        if (rvalue && object && m_args->has_magic_end ()
            && object->is_undefined ())
          err_invalid_inquiry_subscript ();

        retval = tw.convert_to_const_vector (m_args, object);
      }

    octave_idx_type n = retval.length ();

    if (n > 0)
      retval.stash_name_tags (m_arg_nm);

    return retval;
  }

  std::string
  tree_index_expression::get_struct_index
  (tree_evaluator& tw,
   std::list<string_vector>::const_iterator p_arg_nm,
   std::list<tree_expression *>::const_iterator p_dyn_field) const
  {
    std::string fn = (*p_arg_nm)(0);

    if (fn.empty ())
      {
        tree_expression *df = *p_dyn_field;

        if (df)
          {
            octave_value t = df->evaluate (tw);

            fn = t.xstring_value ("dynamic structure field names must be strings");
          }
        else
          panic_impossible ();
      }

    return fn;
  }

  octave_lvalue
  tree_index_expression::lvalue (tree_evaluator& tw)
  {
    std::list<octave_value_list> idx;
    std::string tmp_type;

    int n = m_args.size ();

    auto p_args = m_args.begin ();
    auto p_arg_nm = m_arg_nm.begin ();
    auto p_dyn_field = m_dyn_field.begin ();

    octave_lvalue retval = m_expr->lvalue (tw);

    octave_value tmp = retval.value ();

    octave_idx_type tmpi = 0;
    std::list<octave_value_list> tmpidx;

    for (int i = 0; i < n; i++)
      {
        if (retval.numel () != 1)
          err_indexed_cs_list ();

        if (tmpi < i)
          {
            try
              {
                tmp = tmp.subsref (m_type.substr (tmpi, i-tmpi), tmpidx, true);
              }
            catch (index_exception& e)  // problems with range, invalid type etc.
              {
                tw.final_index_error (e, m_expr);
              }

            tmpidx.clear ();
          }

        switch (m_type[i])
          {
          case '(':
            {
              octave_value_list tidx
                = make_value_list (tw, *p_args, *p_arg_nm, &tmp, false);

              idx.push_back (tidx);

              if (i < n - 1)
                {
                  if (m_type[i+1] != '.')
                    error ("() must be followed by . or close the index chain");

                  tmpidx.push_back (tidx);
                  tmpi = i+1;
                }
            }
            break;

          case '{':
            {
              octave_value_list tidx
                = make_value_list (tw, *p_args, *p_arg_nm, &tmp, false);

              if (tmp.is_undefined ())
                {
                  if (tidx.has_magic_colon ())
                    err_invalid_inquiry_subscript ();

                  tmp = Cell ();
                }
              else if (tmp.is_zero_by_zero ()
                       && (tmp.is_matrix_type () || tmp.is_string ()))
                {
                  tmp = Cell ();
                }

              retval.numel (tmp.xnumel (tidx));

              idx.push_back (tidx);
              tmpidx.push_back (tidx);
              tmpi = i;
            }
            break;

          case '.':
            {
              octave_value tidx = get_struct_index (tw, p_arg_nm, p_dyn_field);

              bool autoconv = (tmp.is_zero_by_zero ()
                               && (tmp.is_matrix_type () || tmp.is_string ()
                                   || tmp.iscell ()));

              if (i > 0 && m_type[i-1] == '(')
                {
                  octave_value_list pidx = idx.back ();

                  // Use octave_map, not octave_scalar_map so that the
                  // dimensions are 0x0, not 1x1.
                  if (tmp.is_undefined ())
                    {
                      if (pidx.has_magic_colon ())
                        err_invalid_inquiry_subscript ();

                      tmp = octave_map ();
                    }
                  else if (autoconv)
                    tmp = octave_map ();

                  retval.numel (tmp.xnumel (pidx));

                  tmpi = i-1;
                  tmpidx.push_back (tidx);
                }
              else
                {
                  if (tmp.is_undefined () || autoconv)
                    {
                      tmpi = i+1;
                      tmp = octave_value ();
                    }
                  else
                    {
                      retval.numel (tmp.xnumel (octave_value_list ()));

                      tmpi = i;
                      tmpidx.push_back (tidx);
                    }
                }

              idx.push_back (tidx);
            }
            break;

          default:
            panic_impossible ();
          }

        if (idx.back ().empty ())
          error ("invalid empty index list");

        p_args++;
        p_arg_nm++;
        p_dyn_field++;
      }

    retval.set_index (m_type, idx);

    return retval;
  }

  tree_index_expression *
  tree_index_expression::dup (symbol_scope& scope) const
  {
    tree_index_expression *new_idx_expr
      = new tree_index_expression (line (), column ());

    new_idx_expr->m_expr = (m_expr ? m_expr->dup (scope) : nullptr);

    std::list<tree_argument_list *> new_args;

    for (const tree_argument_list *elt : m_args)
      new_args.push_back (elt ? elt->dup (scope) : nullptr);

    new_idx_expr->m_args = new_args;

    new_idx_expr->m_type = m_type;

    new_idx_expr->m_arg_nm = m_arg_nm;

    std::list<tree_expression *> new_dyn_field;

    for (const tree_expression *elt : m_dyn_field)
      new_dyn_field.push_back (elt ? elt->dup (scope) : nullptr);

    new_idx_expr->m_dyn_field = new_dyn_field;

    new_idx_expr->copy_base (*this);

    return new_idx_expr;
  }

  // Unlike Matlab, which does not allow the result of a function call
  // or array indexing expression to be further indexed, Octave attempts
  // to handle arbitrary index expressions.  For example, Octave allows
  // expressions like
  //
  //   svd (rand (10))(1:5)
  //
  // Although octave_value objects may contain function objects, no
  // indexing operation or function call is supposed to return them
  // directly.  Instead, the language is supposed to only allow function
  // objects to be stored as function handles (named or anonymous) or as
  // inline functions.  The only place a function object should appear
  // directly is if the symbol stored in a tree_identifier object
  // resolves to a function.  This means that the only place we need to
  // look for functions is in the first element of the index
  // expression.
  //
  // Steps:
  //
  //  * Obtain the initial value from the expression component of the
  //    tree_index_expression object.  If it is a tree_identifier object
  //    indexed by '(args)' and the identifier is not a variable, then
  //    perform a function call.  Use the (optional) arguments to perform
  //    the function lookup so we choose the correct function or class
  //    method to call.  Otherwise, evaluate the first expression
  //    without any additional arguments.
  //
  //  * Iterate over the remaining elements of the index expression and
  //    call the octave_value::subsref method.  If indexing a class or
  //    classdef object, build up a list of indices for a call to the
  //    subsref method for the object.  Otherwise, use the result of
  //    each temporary evaluation for the next index element.
  //
  //  * If not indexing a class or classdef object and any partial
  //    expression evaluation produces a class or classdef object, then
  //    build up a complete argument list from that point on for a final
  //    subsref call for that object.
  //
  //    Multiple partial evaluations may be required.  For example,
  //    given a class or classdef object X, then for the expression
  //
  //      x.a{end}(2:end).b
  //
  //    we must evaluate x.a to obtain the size for the first {end}
  //    expression, then we must evaluate x.a{end} to obtain the size
  //    for the second (2:end) expression.  Finally, the complete
  //    expression may be evaluated.
  //
  //    If X is a cell array in the above expression, and none of the
  //    intermediate evaluations produces a class or classdef object,
  //    then the evaluation is performed as the following series of
  //    steps
  //
  //      tmp = x.a
  //      tmp = tmp{end}
  //      tmp = tmp(2:end)
  //      result = tmp.b
  //
  //    If any of the partial evaluations produces a class or classdef
  //    object, then the subsref method for that object is called as
  //    described above.  For example, suppose x.a produces a classdef
  //    object.  Then the evaluation is performed as the following
  //    series of steps
  //
  //      base_expr = tmp = x.a
  //      tmp = base_expr{end}
  //      base_expr{end}(2:end).b
  //
  //    In the last two steps, the partial value computed in the
  //    previous step is used to determine the value of END.

  octave_value_list
  tree_index_expression::evaluate_n (tree_evaluator& tw, int nargout)
  {
    octave_value_list retval;

    assert (! m_args.empty ());

    auto p_args = m_args.begin ();
    auto p_arg_nm = m_arg_nm.begin ();
    auto p_dyn_field = m_dyn_field.begin ();

    int n = m_args.size ();
    int beg = 0;

    octave_value base_expr_val;

    if (m_expr->is_identifier () && m_type[beg] == '(')
      {
        tree_identifier *id = dynamic_cast<tree_identifier *> (m_expr);

        bool is_var = tw.is_variable (m_expr);

        std::string nm =  id->name ();

        if (is_var && is_word_list_cmd ())
          error ("%s used as variable and later as function", nm.c_str ());

        if (! is_var)
          {
            octave_value_list first_args;

            tree_argument_list *al = *p_args;

            if (al && al->length () > 0)
              {
                unwind_action act ([&tw] (const std::list<octave_lvalue> *lvl)
                                   {
                                     tw.set_lvalue_list (lvl);
                                   }, tw.lvalue_list ());

                tw.set_lvalue_list (nullptr);

                string_vector anm = *p_arg_nm;

                first_args = tw.convert_to_const_vector (al);

                first_args.stash_name_tags (anm);
              }

            interpreter& interp = tw.get_interpreter ();

            symbol_table& symtab = interp.get_symbol_table ();

            octave_value val = symtab.find_function (nm, first_args);

            octave_function *fcn = nullptr;

            if (val.is_function ())
              fcn = val.function_value (true);

            if (fcn)
              {
                try
                  {
                    retval = fcn->call (tw, nargout, first_args);
                  }
                catch (index_exception& e)
                  {
                    tw.final_index_error (e, m_expr);
                  }

                beg++;
                p_args++;
                p_arg_nm++;
                p_dyn_field++;

                if (n > beg)
                  {
                    // More indices to follow.  Silently ignore
                    // extra output values.

                    if (retval.length () == 0)
                      error ("indexing undefined value");
                    else
                      base_expr_val = retval(0);
                  }
                else
                  {
                    // No more indices, so we are done.

                    // See note at end of function about deleting
                    // temporaries prior to pushing result.

                    base_expr_val = octave_value ();
                    first_args = octave_value_list ();

                    return retval;
                  }
              }
          }
      }

    if (base_expr_val.is_undefined ())
      base_expr_val = m_expr->evaluate (tw);

    // If we are indexing an object or looking at something like
    //
    //   classname.static_function (args, ...);
    //
    // then we'll just build a complete index list for one big subsref
    // call.  If the expression we are indexing is a classname then
    // base_expr_val will be an octave_classdef_meta object.  If we have
    // files in a +packagename folder, they will also be an
    // octave_classdef_meta object, but we don't want to index them.

    bool indexing_object = (base_expr_val.isobject ()
                            || base_expr_val.isjava ()
                            || (base_expr_val.is_classdef_meta ()
                                && ! base_expr_val.is_package ()));

    std::list<octave_value_list> idx_list;

    octave_value partial_expr_val = base_expr_val;

    for (int i = beg; i < n; i++)
      {
        if (i > beg)
          {
            tree_argument_list *al = *p_args;

            if (! indexing_object || (al && al->has_magic_end ()))
              {
                // Evaluate what we have so far to find the value to
                // pass to the END function.

                try
                  {
                    // Silently ignore extra output values.

                    octave_value_list tmp_list
                      = base_expr_val.subsref (m_type.substr (beg, i-beg),
                                               idx_list, nargout);

                    partial_expr_val
                      = tmp_list.length () ? tmp_list(0) : octave_value ();

                    if (! indexing_object)
                      {
                        base_expr_val = partial_expr_val;

                        if (partial_expr_val.is_cs_list ())
                          err_indexed_cs_list ();

                        retval = partial_expr_val;

                        beg = i;
                        idx_list.clear ();

                        if (partial_expr_val.isobject ()
                            || partial_expr_val.isjava ()
                            || (partial_expr_val.is_classdef_meta ()
                                && ! partial_expr_val.is_package ()))
                          {
                            // Found an object, so now we'll build up
                            // complete index list for one big subsref
                            // call from this point on.

                            // FIXME: is is also possible to have a
                            // static method call buried somewhere in
                            // the depths of a complex indexing
                            // expression so that we would also need to
                            // check for an octave_classdef_meta object
                            // here?

                            indexing_object = true;
                          }
                      }
                  }
                catch (index_exception& e)
                  {
                    tw.final_index_error (e, m_expr);
                  }
              }
          }

        switch (m_type[i])
          {
          case '(':
            idx_list.push_back (make_value_list (tw, *p_args, *p_arg_nm,
                                                 &partial_expr_val));
            break;

          case '{':
            idx_list.push_back (make_value_list (tw, *p_args, *p_arg_nm,
                                                 &partial_expr_val));
            break;

          case '.':
            idx_list.push_back (octave_value
                                (get_struct_index (tw, p_arg_nm, p_dyn_field)));
            break;

          default:
            panic_impossible ();
          }

        p_args++;
        p_arg_nm++;
        p_dyn_field++;
      }


    // If ! idx_list.empty () that means we still have stuff to index
    // otherwise they would have been dealt with and idx_list would have
    // been emptied.
    if (! idx_list.empty ())
      {
        // This is for +package and other classdef_meta objects
        if (! base_expr_val.is_function ()
            || base_expr_val.is_classdef_meta ())
          {
            try
              {
                retval = base_expr_val.subsref (m_type.substr (beg, n-beg),
                                                idx_list, nargout);
                beg = n;
                idx_list.clear ();
              }
            catch (index_exception& e)
              {
                tw.final_index_error (e, m_expr);
              }
          }
        else
          {
            // FIXME: we want this to only be a superclass constructor
            // call Should we actually make a check for this or are all
            // other types of calls already dealt with?

            octave_function *fcn = base_expr_val.function_value ();

            if (fcn)
              {
                try
                  {
                    // FIXME: is it possible for the IDX_LIST to have
                    // more than one element here?  Do we need to check?

                    octave_value_list final_args;

                    if (idx_list.size () != 1)
                      error ("unexpected extra index at end of expression");

                    if (m_type[beg] != '(')
                      error ("invalid index type '%c' for function call",
                             m_type[beg]);

                    final_args = idx_list.front ();

                    // FIXME: Do we ever need the names of the arguments
                    // passed to FCN here?

                    retval = fcn->call (tw, nargout, final_args);
                  }
                catch (index_exception& e)
                  {
                    tw.final_index_error (e, m_expr);
                  }
              }
          }
      }

    // FIXME: when can the following happen?  In what case does indexing
    // result in a value that is a function?  Classdef method calls?
    // Something else?

    octave_value val = (retval.length () ? retval(0) : octave_value ());

    if (val.is_function ())
      {
        octave_function *fcn = val.function_value (true);

        if (fcn)
          {
            octave_value_list final_args;

            if (! idx_list.empty ())
              {
                if (n - beg != 1)
                  error ("unexpected extra index at end of expression");

                if (m_type[beg] != '(')
                  error ("invalid index type '%c' for function call",
                         m_type[beg]);

                final_args = idx_list.front ();
              }

            retval = fcn->call (tw, nargout, final_args);
          }
      }

    // Delete any temporary values prior to pushing the result and
    // returning so that destructors for any temporary classdef handle
    // objects will be called before we return.  Otherwise, the
    // destructor may push result values that will wipe out the result
    // that we push below.  Although the method name is "push_result"
    // there is only a single register (either an octave_value or an
    // octave_value_list) not a stack.

    idx_list.clear ();
    partial_expr_val = octave_value ();
    base_expr_val = octave_value ();
    val = octave_value ();

    return retval;
  }
}

/*
%!test
%! clear x;
%! clear y;
%! y = 3;
%! x(y(end)) = 1;
%! assert (x, [0, 0, 1]);
%! clear x;
%! clear y;
%! y = {3};
%! x(y{end}) = 1;
%! assert (x, [0, 0, 1]);

%!test
%! x = {1, 2, 3};
%! [x{:}] = deal (4, 5, 6);
%! assert (x, {4, 5, 6});

%!test
%! [x.a, x.b.c] = deal (1, 2);
%! assert (x.a == 1 && x.b.c == 2);

%!test
%! [x.a, x(2).b] = deal (1, 2);
%! assert (x(1).a == 1 && isempty (x(2).a) && isempty (x(1).b) && x(2).b == 2);

%!test
%! x = struct (zeros (0, 1), {"a", "b"});
%! x(2).b = 1;
%! assert (x(2).b == 1);

%!test
%! x = struct (zeros (0, 1), {"a", "b"});
%! x(2).b = 1;
%! assert (x(2).b == 1);
*/
