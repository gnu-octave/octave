/*

Copyright (C) 1996-2018 John W. Eaton
Copyright (C) 2015-2018 Olaf Till

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

#if !defined (octave_pt_anon_scopes_h)
#define octave_pt_anon_scopes_h 1

#include "pt-walk.h"
#include "ov-usr-fcn.h"

namespace octave
{
  // In possibly nested definitions of anonymous functions, collect
  // their scopes and the symbol records therein.

  class
  tree_anon_scopes : public tree_walker
  {
  public:

    tree_anon_scopes (void) : scopes (), merged_tables () { }

    tree_anon_scopes (octave_user_function *);

    tree_anon_scopes& operator = (tree_anon_scopes&& tas)
    {
      scopes = std::move (tas.scopes);
      merged_tables = std::move (tas.merged_tables);
      return *this;
    }

    typedef std::map<std::string, symbol_record> symrec_map;

    symrec_map::const_iterator begin (void)
    {
      return merged_tables.cbegin ();
    }

    symrec_map::const_iterator end (void)
    {
      return merged_tables.cend ();
    }

    unsigned int symrec_map_size (void)
    {
      return merged_tables.size ();
    }

    // No copying!

    tree_anon_scopes (const tree_anon_scopes&) = delete;

    tree_anon_scopes& operator = (const tree_anon_scopes&) = delete;

    ~tree_anon_scopes (void) { }

    // The following methods, though public, don't belong to the
    // intended user interface of this class.

    void visit_anon_fcn_handle (tree_anon_fcn_handle&);

    void visit_argument_list (tree_argument_list&);

    void visit_binary_expression (tree_binary_expression&);

    void visit_break_command (tree_break_command&);

    void visit_colon_expression (tree_colon_expression&);

    void visit_continue_command(tree_continue_command&);

    void visit_decl_command (tree_decl_command&);

    void visit_decl_elt (tree_decl_elt&);

    void visit_decl_init_list (tree_decl_init_list&);

    void visit_simple_for_command (tree_simple_for_command&);

    void visit_complex_for_command (tree_complex_for_command&);

    void visit_octave_user_script (octave_user_script&);

    void visit_octave_user_function (octave_user_function&);

    void visit_function_def (tree_function_def&);

    void visit_identifier (tree_identifier&);

    void visit_if_clause (tree_if_clause&);

    void visit_if_command (tree_if_command&);

    void visit_if_command_list (tree_if_command_list&);

    void visit_switch_case (tree_switch_case&);

    void visit_switch_case_list (tree_switch_case_list&);

    void visit_switch_command (tree_switch_command&);

    void visit_index_expression (tree_index_expression&);

    void visit_matrix (tree_matrix&);

    void visit_cell (tree_cell&);

    void visit_multi_assignment (tree_multi_assignment&);

    void visit_no_op_command (tree_no_op_command&);

    void visit_constant (tree_constant&);

    void visit_fcn_handle (tree_fcn_handle&);

    void visit_funcall (tree_funcall&);

    void visit_parameter_list (tree_parameter_list&);

    void visit_postfix_expression (tree_postfix_expression&);

    void visit_prefix_expression (tree_prefix_expression&);

    void visit_return_command (tree_return_command&);

    void visit_return_list (tree_return_list&);

    void visit_simple_assignment (tree_simple_assignment&);

    void visit_statement (tree_statement&);

    void visit_statement_list (tree_statement_list&);

    void visit_try_catch_command (tree_try_catch_command&);

    void visit_unwind_protect_command (tree_unwind_protect_command&);

    void visit_while_command (tree_while_command&);

    void visit_do_until_command (tree_do_until_command&);

  private:

    void stash_scope_if_valid (const symbol_scope& sc)
    {
      if (sc)
        scopes.push_back (sc);
      else
        error ("internal error, invalid scope");
    }

    // The scope of this anonymous function and the collected scopes
    // of all anonymous functions whose definitions are nested in the
    // current anonymous function definition.

    std::vector<symbol_scope> scopes;

    // Symbol records of all collected scopes are merged over variable names.

    typedef std::list<symbol_record> symrec_list;

    void merge_tables (void);

    symrec_map merged_tables;
  };
}

#endif
