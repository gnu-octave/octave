////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2023 The Octave Project Developers
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

OCTAVE_BEGIN_NAMESPACE(octave)

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
  return tw.evaluate_anon_fcn_handle (*this);
}

OCTAVE_END_NAMESPACE(octave)

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
