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

#if ! defined (octave_pt_fcn_handle_h)
#define octave_pt_fcn_handle_h 1

#include "octave-config.h"

#include <iosfwd>
#include <string>

#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-misc.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "symtab.h"

class octave_value_list;

#include "ov.h"
#include "ov-usr-fcn.h"
#include "symtab.h"

namespace octave
{
  class tree_fcn_handle : public tree_expression
  {
  public:

    tree_fcn_handle (int l = -1, int c = -1)
      : tree_expression (l, c), nm () { }

    tree_fcn_handle (const std::string& n, int l = -1, int c = -1)
      : tree_expression (l, c), nm (n) { }

    // No copying!

    tree_fcn_handle (const tree_fcn_handle&) = delete;

    tree_fcn_handle& operator = (const tree_fcn_handle&) = delete;

    ~tree_fcn_handle (void) = default;

    bool has_magic_end (void) const { return false; }

    void print (std::ostream& os, bool pr_as_read_syntax = false,
                bool pr_orig_txt = true);

    void print_raw (std::ostream& os, bool pr_as_read_syntax = false,
                    bool pr_orig_txt = true);

    std::string name (void) const { return nm; }

    bool rvalue_ok (void) const { return true; }

    tree_expression * dup (symbol_table::scope& scope) const;

    void accept (tree_walker& tw)
    {
      tw.visit_fcn_handle (*this);
    }

  private:

    // The name of this function handle.
    std::string nm;
  };

  class tree_anon_fcn_handle : public tree_expression
  {
  public:

    tree_anon_fcn_handle (int l = -1, int c = -1)
      : tree_expression (l, c), m_parameter_list (nullptr),
        m_expression (nullptr), m_scope (nullptr), m_parent_scope (nullptr),
        m_file_name ()
    { }

    tree_anon_fcn_handle (tree_parameter_list *pl, tree_expression *ex,
                          symbol_table::scope *scope,
                          symbol_table::scope *parent_scope,
                          int l = -1, int c = -1)
      : tree_expression (l, c), m_parameter_list (pl), m_expression (ex),
        m_scope (scope), m_parent_scope (parent_scope), m_file_name ()
    { }

    // No copying!

    tree_anon_fcn_handle (const tree_anon_fcn_handle&) = delete;

    tree_anon_fcn_handle& operator = (const tree_anon_fcn_handle&) = delete;

    ~tree_anon_fcn_handle (void);

    bool has_magic_end (void) const { return false; }

    bool rvalue_ok (void) const { return true; }

    tree_parameter_list * parameter_list (void) const
    {
      return m_parameter_list;
    }

    tree_expression * expression (void) const { return m_expression; }

    symbol_table::scope *scope (void) const { return m_scope; }

    symbol_table::scope *parent_scope (void) const { return m_parent_scope; }

    bool has_parent_scope (void) const { return m_parent_scope; }

    tree_expression * dup (symbol_table::scope& scope) const;

    void accept (tree_walker& tw) { tw.visit_anon_fcn_handle (*this); }

    void stash_file_name (const std::string& file) { m_file_name = file; }

    std::string file_name (void) const { return m_file_name; }

  private:

    // Inputs parameters.
    tree_parameter_list *m_parameter_list;

    // Function body, limited to a single expression.
    tree_expression *m_expression;

    // Function scope.
    symbol_table::scope *m_scope;

    // Parent scope, or 0 if none.
    symbol_table::scope *m_parent_scope;

    // Filename where the handle was defined.
    std::string m_file_name;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_fcn_handle' instead")
typedef octave::tree_fcn_handle tree_fcn_handle;

OCTAVE_DEPRECATED (4.4, "use 'octave::tree_anon_fcn_handle' instead")
typedef octave::tree_anon_fcn_handle tree_anon_fcn_handle;

#endif

#endif
