/*

Copyright (C) 2003-2017 John W. Eaton

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

#include "call-stack.h"
#include "error.h"
#include "interpreter-private.h"
#include "ovl.h"
#include "ov-fcn-handle.h"
#include "pt-fcn-handle.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-walk.h"
#include "variables.h"

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
    os << ((pr_as_read_syntax || pr_orig_text) ? "@" : "") << nm;
  }

  tree_expression *
  tree_fcn_handle::dup (symbol_table::scope_id,
                        symbol_table::context_id) const
  {
    tree_fcn_handle *new_fh = new tree_fcn_handle (nm, line (), column ());

    new_fh->copy_base (*this);

    return new_fh;
  }

  tree_anon_fcn_handle::~tree_anon_fcn_handle (void)
  {
    delete m_parameter_list;
    delete m_return_list;
    delete m_statement_list;
  }

  tree_expression *
  tree_anon_fcn_handle::dup (symbol_table::scope_id,
                             symbol_table::context_id) const
  {
    tree_parameter_list *param_list = parameter_list ();
    tree_parameter_list *ret_list = return_list ();
    tree_statement_list *stmt_list = body ();

    symbol_table::scope_id af_sid = scope ();
    symbol_table::scope_id af_parent_sid = parent_scope ();

    symbol_table& symtab
      = octave::__get_symbol_table__ ("tree_anon_fcn_handle::dup");

    symbol_table::scope_id new_scope = symtab.dup_scope (af_sid);

    if (new_scope > 0)
      symtab.inherit (new_scope);

    tree_anon_fcn_handle *new_afh = new
      tree_anon_fcn_handle (param_list ? param_list->dup (new_scope, 0) : 0,
                            ret_list ? ret_list->dup (new_scope, 0) : 0,
                            stmt_list ? stmt_list->dup (new_scope, 0) : 0,
                            new_scope, af_parent_sid, line (), column ());

    new_afh->copy_base (*this);

    return new_afh;
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
