////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <string>

#include "error.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "parse.h"
#include "pt-arg-list.h"
#include "pt-assign.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Simple assignment expressions.

tree_simple_assignment::tree_simple_assignment (tree_expression *le,
    tree_expression *re,
    bool plhs, int l, int c,
    octave_value::assign_op t)
  : tree_expression (l, c), m_lhs (le), m_rhs (re), m_preserve (plhs),
    m_ans_assign (), m_etype (t)
{ }

tree_simple_assignment::~tree_simple_assignment (void)
{
  if (! m_preserve)
    delete m_lhs;

  delete m_rhs;
}

std::string
tree_simple_assignment::oper (void) const
{
  return octave_value::assign_op_as_string (m_etype);
}

tree_expression *
tree_simple_assignment::dup (symbol_scope& scope) const
{
  tree_simple_assignment *new_sa
    = new tree_simple_assignment (m_lhs ? m_lhs->dup (scope) : nullptr,
                                  m_rhs ? m_rhs->dup (scope) : nullptr,
                                  m_preserve, m_etype);

  new_sa->copy_base (*this);

  return new_sa;
}

octave_value
tree_simple_assignment::evaluate (tree_evaluator& tw, int)
{
  octave_value val;

  if (m_rhs)
    {
      try
        {
          octave_lvalue ult = m_lhs->lvalue (tw);

          std::list<octave_lvalue> lvalue_list;
          lvalue_list.push_back (ult);

          unwind_action act ([&tw] (const std::list<octave_lvalue> *lvl)
          {
            tw.set_lvalue_list (lvl);
          }, tw.lvalue_list ());
          tw.set_lvalue_list (&lvalue_list);

          if (ult.numel () != 1)
            err_invalid_structure_assignment ();

          octave_value rhs_val = m_rhs->evaluate (tw);

          if (rhs_val.is_undefined ())
            error ("value on right hand side of assignment is undefined");

          if (rhs_val.is_cs_list ())
            {
              const octave_value_list lst = rhs_val.list_value ();

              if (lst.empty ())
                error ("invalid number of elements on RHS of assignment");

              rhs_val = lst(0);
            }

          ult.assign (m_etype, rhs_val);

          if (m_etype == octave_value::op_asn_eq)
            val = rhs_val;
          else
            val = ult.value ();

          if (print_result () && tw.statement_printing_enabled ())
            {
              // We clear any index here so that we can
              // get the new value of the referenced
              // object below, instead of the indexed
              // value (which should be the same as the
              // right hand side value).

              ult.clear_index ();

              octave_value lhs_val = ult.value ();

              octave_value_list args = ovl (lhs_val);
              args.stash_name_tags (string_vector (m_lhs->name ()));
              feval ("display", args);
            }
        }
      catch (index_exception& ie)
        {
          ie.set_var (m_lhs->name ());
          std::string msg = ie.message ();
          error_with_id (ie.err_id (), "%s", msg.c_str ());
        }
    }

  return val;
}

// Multi-valued assignment expressions.

tree_multi_assignment::tree_multi_assignment (tree_argument_list *lst,
    tree_expression *r,
    bool plhs, int l, int c)
  : tree_expression (l, c), m_lhs (lst), m_rhs (r), m_preserve (plhs)
{ }

tree_multi_assignment::~tree_multi_assignment (void)
{
  if (! m_preserve)
    delete m_lhs;

  delete m_rhs;
}

std::string
tree_multi_assignment::oper (void) const
{
  return octave_value::assign_op_as_string (op_type ());
}

tree_expression *
tree_multi_assignment::dup (symbol_scope&) const
{
  panic_impossible ();
  return nullptr;
}

octave_value_list
tree_multi_assignment::evaluate_n (tree_evaluator& tw, int)
{
  octave_value_list val;

  if (m_rhs)
    {
      std::list<octave_lvalue> lvalue_list = tw.make_lvalue_list (m_lhs);

      unwind_action act ([&tw] (const std::list<octave_lvalue> *lvl)
      {
        tw.set_lvalue_list (lvl);
      }, tw.lvalue_list ());
      tw.set_lvalue_list (&lvalue_list);

      octave_idx_type n_out = 0;

      for (const auto& lval : lvalue_list)
        n_out += lval.numel ();

      // The following trick is used to keep rhs_val constant.
      const octave_value_list rhs_val1 = m_rhs->evaluate_n (tw, n_out);
      const octave_value_list rhs_val = (rhs_val1.length () == 1
                                         && rhs_val1(0).is_cs_list ()
                                         ? rhs_val1(0).list_value ()
                                         : rhs_val1);

      tw.set_lvalue_list (nullptr);

      octave_idx_type k = 0;

      octave_idx_type n = rhs_val.length ();

      // To avoid copying per elements and possible optimizations, we
      // postpone joining the final values.
      std::list<octave_value_list> retval_list;

      auto q = m_lhs->begin ();

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
              // or
              //
              //   varargout = cell (1, nargout);
              //   [varargout{1:nargout}] = fcn (args);
              //
              // or
              //
              //   varargout = cell (1, nargout);
              //   [varargout{:}] = fcn (args);
              //
              // Will work the same as calling fcn directly when nargout
              // is 0 and fcn produces more than one output even when
              // nargout is 0.  See also bug #43813.

              if (lvalue_list.size () == 1 && nel == 0 && n > 0
                  && ! ult.is_black_hole () && ult.index_type () == "{"
                  && (ult.index_is_empty ()
                      || (ult.is_defined () && ult.index_is_colon ())))
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
                  if (ult.is_black_hole ())
                    {
                      k++;
                      continue;
                    }
                  else
                    {
                      octave_value tmp = rhs_val(k);

                      if (tmp.is_undefined ())
                        error ("element number %" OCTAVE_IDX_TYPE_FORMAT
                               " undefined in return list", k+1);

                      ult.assign (octave_value::op_asn_eq, tmp);

                      retval_list.push_back (tmp);

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
                    error ("element number %" OCTAVE_IDX_TYPE_FORMAT
                           " undefined in return list", k+1);

                  k++;
                  continue;
                }
            }

          if (print_result () && tw.statement_printing_enabled ())
            {
              // We clear any index here so that we can get
              // the new value of the referenced object below,
              // instead of the indexed value (which should be
              // the same as the right hand side value).

              ult.clear_index ();

              octave_value lhs_val = ult.value ();

              octave_value_list args = ovl (lhs_val);
              args.stash_name_tags (string_vector (lhs_elt->name ()));
              feval ("display", args);
            }
        }

      // Concatenate return values.
      val = retval_list;
    }

  return val;
}

OCTAVE_END_NAMESPACE(octave)

/*
%!function varargout = f1 ()
%!  varargout{1} = nargout;
%!endfunction
%!
%!test
%! [a, ~] = f1 ();
%! assert (a, 2);
%!test
%! [a, ~, ~, ~, ~] = f1 ();
%! assert (a, 5);

%!function [x, y] = f2 ()
%!  y = 1;
%!endfunction
%!
%!test
%! [~, y] = f2 ();
%! assert (y, 1);

%!function [x, y, varargout] = f3 ()
%!  y = 1;
%!  varargout = {2, 3};
%!endfunction
%!
%!test
%! [~, y, a, b] = f3 ();
%! assert ([y, a, b], [1, 2, 3]);
%!test
%! [~, y, ~, b] = f3 ();
%! assert ([y, b], [1, 3]);
*/
