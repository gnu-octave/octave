/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>
#include <set>

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pager.h"
#include "parse.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-assign.h"
#include "pt-eval.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"

namespace octave
{
  // Simple assignment expressions.

  tree_simple_assignment::tree_simple_assignment
  (tree_expression *le, tree_expression *re,
   bool plhs, int l, int c, octave_value::assign_op t)
    : tree_expression (l, c), lhs (le), rhs (re), preserve (plhs), etype (t)
  { }

  tree_simple_assignment::~tree_simple_assignment (void)
  {
    if (! preserve)
      delete lhs;

    delete rhs;
  }

  octave_value_list
  tree_simple_assignment::rvalue (int nargout)
  {
    if (nargout > 1)
      error ("invalid number of output arguments for expression X = RHS");

    return rvalue1 (nargout);
  }

  octave_value
  tree_simple_assignment::rvalue1 (int)
  {
    octave_value retval;

    if (rhs)
      {
        octave_value rhs_val = rhs->rvalue1 ();

        if (rhs_val.is_undefined ())
          error ("value on right hand side of assignment is undefined");

        if (rhs_val.is_cs_list ())
          {
            const octave_value_list lst = rhs_val.list_value ();

            if (lst.empty ())
              error ("invalid number of elements on RHS of assignment");

            rhs_val = lst(0);
          }

        try
          {
            octave_lvalue ult = lhs->lvalue ();

            if (ult.numel () != 1)
              err_nonbraced_cs_list_assignment ();

            ult.assign (etype, rhs_val);

            if (etype == octave_value::op_asn_eq)
              retval = rhs_val;
            else
              retval = ult.value ();

            if (print_result ()
                && octave::tree_evaluator::statement_printing_enabled ())
              {
                // We clear any index here so that we can
                // get the new value of the referenced
                // object below, instead of the indexed
                // value (which should be the same as the
                // right hand side value).

                ult.clear_index ();

                octave_value lhs_val = ult.value ();

                octave_value_list args = ovl (lhs_val);
                args.stash_name_tags (string_vector (lhs->name ()));
                octave::feval ("display", args);
              }
          }
        catch (octave::index_exception& e)
          {
            e.set_var (lhs->name ());
            std::string msg = e.message ();
            error_with_id (e.err_id (), msg.c_str ());
          }
      }

    return retval;
  }

  std::string
  tree_simple_assignment::oper (void) const
  {
    return octave_value::assign_op_as_string (etype);
  }

  tree_expression *
  tree_simple_assignment::dup (symbol_table::scope_id scope,
                               symbol_table::context_id context) const
  {
    tree_simple_assignment *new_sa
      = new tree_simple_assignment (lhs ? lhs->dup (scope, context) : 0,
                                    rhs ? rhs->dup (scope, context) : 0,
                                    preserve, etype);

    new_sa->copy_base (*this);

    return new_sa;
  }

  void
  tree_simple_assignment::accept (tree_walker& tw)
  {
    tw.visit_simple_assignment (*this);
  }

  // Multi-valued assignment expressions.

  tree_multi_assignment::tree_multi_assignment
  (tree_argument_list *lst, tree_expression *r,
   bool plhs, int l, int c)
    : tree_expression (l, c), lhs (lst), rhs (r), preserve (plhs)
  { }

  tree_multi_assignment::~tree_multi_assignment (void)
  {
    if (! preserve)
      delete lhs;

    delete rhs;
  }

  octave_value
  tree_multi_assignment::rvalue1 (int nargout)
  {
    octave_value retval;

    const octave_value_list tmp = rvalue (nargout);

    if (! tmp.empty ())
      retval = tmp(0);

    return retval;
  }

  // FIXME: this works, but it would look a little better if
  // it were broken up into a couple of separate functions.

  octave_value_list
  tree_multi_assignment::rvalue (int)
  {
    octave_value_list retval;

    if (rhs)
      {
        std::list<octave_lvalue> lvalue_list = lhs->lvalue_list ();

        octave_idx_type n_out = 0;

        for (const auto& lval : lvalue_list)
          n_out += lval.numel ();

        // The following trick is used to keep rhs_val constant.
        const octave_value_list rhs_val1 = rhs->rvalue (n_out, &lvalue_list);
        const octave_value_list rhs_val = (rhs_val1.length () == 1
                                           && rhs_val1(0).is_cs_list ()
                                           ? rhs_val1(0).list_value ()
                                           : rhs_val1);

        octave_idx_type k = 0;

        octave_idx_type n = rhs_val.length ();

        // To avoid copying per elements and possible optimizations, we
        // postpone joining the final values.
        std::list<octave_value_list> retval_list;

        tree_argument_list::iterator q = lhs->begin ();

        for (octave_lvalue ult : lvalue_list)
          {
            tree_expression *lhs_elt = *q++;

            octave_idx_type nel = ult.numel ();

            if (nel != 1)
              {
                // Huge kluge so that wrapper scripts with lines like
                //
                //   [varargout{1:nargout}] = fcn (args);
                //
                // Will work the same as calling fcn directly when nargout
                // is 0 and fcn produces more than one output even when
                // nargout is 0.  This only works if varargout has not yet
                // been defined.  See also bug #43813.

                if (lvalue_list.size () == 1 && nel == 0 && n > 0
                    && ! ult.is_black_hole () && ult.is_undefined ()
                    && ult.index_type () == "{" && ult.index_is_empty ())
                  {
                    // Convert undefined lvalue with empty index to a cell
                    // array with a single value and indexed by 1 to
                    // handle a single output.

                    nel = 1;

                    ult.define (Cell (1, 1));

                    ult.clear_index ();
                    std::list<octave_value_list> idx;
                    idx.push_back (octave_value_list (octave_value (1)));
                    ult.set_index ("{", idx);
                  }

                if (k + nel > n)
                  error ("some elements undefined in return list");

                // This element of the return list expects a
                // comma-separated list of values.  Slicing avoids
                // copying.

                octave_value_list ovl = rhs_val.slice (k, nel);

                ult.assign (octave_value::op_asn_eq, octave_value (ovl));

                retval_list.push_back (ovl);

                k += nel;
              }
            else
              {
                if (k < n)
                  {
                    ult.assign (octave_value::op_asn_eq, rhs_val(k));

                    if (ult.is_black_hole ())
                      {
                        k++;
                        continue;
                      }
                    else
                      {
                        retval_list.push_back (rhs_val(k));

                        k++;
                      }
                  }
                else
                  {
                    // This can happen for a function like
                    //
                    //   function varargout = f ()
                    //     varargout{1} = nargout;
                    //   endfunction
                    //
                    // called with
                    //
                    //    [a, ~] = f ();
                    //
                    // Then the list of of RHS values will contain one
                    // element but we are iterating over the list of all
                    // RHS values.  We shouldn't complain that a value we
                    // don't need is missing from the list.

                    if (! ult.is_black_hole ())
                      error ("element number %d undefined in return list", k+1);

                    k++;
                    continue;
                  }
              }

            if (print_result ()
                && octave::tree_evaluator::statement_printing_enabled ())
              {
                // We clear any index here so that we can get
                // the new value of the referenced object below,
                // instead of the indexed value (which should be
                // the same as the right hand side value).

                ult.clear_index ();

                octave_value lhs_val = ult.value ();

                octave_value_list args = ovl (lhs_val);
                args.stash_name_tags (string_vector (lhs_elt->name ()));
                octave::feval ("display", args);
              }
          }

        // Concatenate return values.
        retval = retval_list;
      }

    return retval;
  }

  std::string
  tree_multi_assignment::oper (void) const
  {
    return octave_value::assign_op_as_string (op_type ());
  }

  tree_expression *
  tree_multi_assignment::dup (symbol_table::scope_id scope,
                              symbol_table::context_id context) const
  {
    tree_multi_assignment *new_ma
      = new tree_multi_assignment (lhs ? lhs->dup (scope, context) : 0,
                                   rhs ? rhs->dup (scope, context) : 0,
                                   preserve);

    new_ma->copy_base (*this);

    return new_ma;
  }

  void
  tree_multi_assignment::accept (tree_walker& tw)
  {
    tw.visit_multi_assignment (*this);
  }
}

/*
%!function varargout = f ()
%!  varargout{1} = nargout;
%!endfunction
%!
%!test
%! [a, ~] = f ();
%! assert (a, 2);
%!test
%! [a, ~, ~, ~, ~] = f ();
%! assert (a, 5);
*/
