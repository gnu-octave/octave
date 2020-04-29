////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2020 The Octave Project Developers
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

#include <ostream>

#include "interpreter-private.h"
#include "pt-anon-scopes.h"
#include "pt-fcn-handle.h"
#include "stack-frame.h"

namespace octave
{
  void
  tree_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax,
                          bool pr_orig_text)
  {
    print_raw (os, pr_as_read_syntax, pr_orig_text);
  }

  void
  tree_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax,
                              bool pr_orig_text)
  {
    os << ((pr_as_read_syntax || pr_orig_text) ? "@" : "") << m_name;
  }

  tree_expression *
  tree_fcn_handle::dup (symbol_scope&) const
  {
    tree_fcn_handle *new_fh = new tree_fcn_handle (m_name, line (), column ());

    new_fh->copy_base (*this);

    return new_fh;
  }

  octave_value
  tree_fcn_handle::evaluate (tree_evaluator& tw, int)
  {
    return tw.make_fcn_handle (m_name);
  }

  tree_anon_fcn_handle::~tree_anon_fcn_handle (void)
  {
    delete m_parameter_list;
    delete m_expression;
  }

  tree_expression *
  tree_anon_fcn_handle::dup (symbol_scope&) const
  {
    tree_parameter_list *param_list = parameter_list ();
    tree_expression *expr = expression ();

    symbol_scope af_scope = m_scope;
    symbol_scope af_parent_scope = m_parent_scope;

    symbol_scope new_scope;

    if (af_scope)
      new_scope = af_scope.dup ();

    // FIXME: if new scope is nullptr, then we are in big trouble here...

    tree_anon_fcn_handle *new_afh = new
      tree_anon_fcn_handle (param_list ? param_list->dup (new_scope) : nullptr,
                            expr ? expr->dup (new_scope) : nullptr,
                            new_scope, af_parent_scope, line (), column ());

    new_afh->copy_base (*this);

    return new_afh;
  }

  octave_value
  tree_anon_fcn_handle::evaluate (tree_evaluator& tw, int)
  {
    // FIXME: should CMD_LIST be limited to a single expression?
    // I think that is what Matlab does.

    symbol_scope new_scope;
    if (m_scope)
      new_scope = m_scope.dup ();

    tree_parameter_list *param_list_dup
      = m_parameter_list ? m_parameter_list->dup (new_scope) : nullptr;

    tree_parameter_list *ret_list = nullptr;

    tree_statement_list *stmt_list = nullptr;

    symbol_scope parent_scope = tw.get_current_scope ();

    new_scope.set_parent (parent_scope);
    new_scope.set_primary_parent (parent_scope);

    if (m_expression)
      {
        tree_expression *expr_dup = m_expression->dup (new_scope);
        tree_statement *stmt = new tree_statement (expr_dup, nullptr);
        stmt_list = new tree_statement_list (stmt);
      }

    tree_anon_scopes anon_fcn_ctx (*this);

    std::set<std::string> free_vars = anon_fcn_ctx.free_variables ();

    stack_frame::local_vars_map local_vars;

    call_stack& cs = tw.get_call_stack ();

    std::shared_ptr<stack_frame> frame = cs.get_current_stack_frame ();

    for (auto& name : free_vars)
      {
        octave_value val = frame->varval (name);

        if (val.is_defined ())
          local_vars[name] = val;
      }

    octave_user_function *af
      = new octave_user_function (new_scope, param_list_dup, ret_list,
                                  stmt_list);

    octave_function *curr_fcn = cs.current_function ();

    if (curr_fcn)
      {
        // FIXME: maybe it would be better to just stash curr_fcn
        // instead of individual bits of info about it?

        af->stash_parent_fcn_name (curr_fcn->name ());
        af->stash_dir_name (curr_fcn->dir_name ());

        new_scope.cache_fcn_file_name (curr_fcn->fcn_file_name ());
        new_scope.cache_dir_name (curr_fcn->dir_name ());

        // The following is needed so that class method dispatch works
        // properly for anonymous functions that wrap class methods.

        if (curr_fcn->is_class_method () || curr_fcn->is_class_constructor ())
          af->stash_dispatch_class (curr_fcn->dispatch_class ());

        af->stash_fcn_file_name (curr_fcn->fcn_file_name ());
      }

    af->mark_as_anonymous_function ();

    octave_value ov_fcn (af);

    return octave_value (new octave_fcn_handle (ov_fcn, local_vars));
  }
}

/*
%!function r = __f2 (f, x)
%!  r = f (x);
%!endfunction
%!function f = __f1 (k)
%!  f = @(x) __f2 (@(y) y-k, x);
%!endfunction

%!assert ((__f1 (3)) (10) == 7)

%!test
%! g = @(t) feval (@(x) t*x, 2);
%! assert (g(0.5) == 1);

%!test
%! h = @(x) sin (x);
%! g = @(f, x) h (x);
%! f = @() g (@(x) h, pi);
%! assert (f () == sin (pi));

The next two tests are intended to test parsing of a character string
vs. hermitian operator at the beginning of an anonymous function
expression.  The use of ' for the character string and the spacing is
intentional, so don't change it.

%!test
%! f = @() 'foo';
%! assert (f (), 'foo');

%!test
%! f = @()'foo';
%! assert (f (), 'foo');
*/
