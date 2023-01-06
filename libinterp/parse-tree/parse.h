////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_parse_h)
#define octave_parse_h 1

#include "octave-config.h"

#include <cstdio>

#include <deque>
#include <map>
#include <memory>
#include <set>
#include <string>

#include "input.h"
#include "lex.h"
#include "pt-misc.h"
#include "symscope.h"
#include "token.h"

class octave_function;
class octave_user_code;
class octave_user_function;

OCTAVE_BEGIN_NAMESPACE(octave)

class comment_list;
class parse_exception;
class tree;
class tree_anon_fcn_handle;
class tree_arg_size_spec;
class tree_arg_validation;
class tree_arg_validation_fcns;
class tree_args_block_attribute_list;
class tree_args_block_validation_list;
class tree_argument_list;
class tree_arguments_block;
class tree_array_list;
class tree_cell;
class tree_classdef;
class tree_classdef_attribute_list;
class tree_classdef_body;
class tree_classdef_enum_block;
class tree_classdef_enum_list;
class tree_classdef_events_block;
class tree_classdef_events_list;
class tree_classdef_methods_block;
class tree_classdef_methods_list;
class tree_classdef_properties_block;
class tree_classdef_property_list;
class tree_classdef_superclass_list;
class tree_colon_expression;
class tree_command;
class tree_constant;
class tree_decl_command;
class tree_decl_init_list;
class tree_expression;
class tree_fcn_handle;
class tree_function_def;
class tree_identifier;
class tree_if_clause;
class tree_if_command;
class tree_if_command_list;
class tree_index_expression;
class tree_matrix;
class tree_matrix;
class tree_parameter_list;
class tree_spmd_command;
class tree_statement;
class tree_statement_list;
class tree_statement_listtree_statement;
class tree_switch_case;
class tree_switch_case_list;
class tree_switch_command;

OCTAVE_END_NAMESPACE(octave)

#include "ovl.h"

// Nonzero means print parser debugging info (-d).
extern int octave_debug;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_parser
{
private:

  class parent_scope_info
  {
  public:

    typedef std::pair<symbol_scope, std::string> value_type;

    typedef std::deque<value_type>::iterator iterator;
    typedef std::deque<value_type>::const_iterator const_iterator;

    typedef std::deque<value_type>::reverse_iterator reverse_iterator;
    typedef std::deque<value_type>::const_reverse_iterator const_reverse_iterator;

    parent_scope_info (void) = delete;

    parent_scope_info (base_parser& parser)
      : m_parser (parser), m_info (), m_all_names ()
    { }

    // No copying!

    parent_scope_info (const parent_scope_info&) = delete;

    parent_scope_info& operator = (const parent_scope_info&) = delete;

    ~parent_scope_info (void) = default;

    OCTINTERP_API std::size_t size (void) const;

    OCTINTERP_API void push (const value_type& elt);

    OCTINTERP_API void push (const symbol_scope& id);

    OCTINTERP_API void pop (void);

    OCTINTERP_API bool name_ok (const std::string& name);

    OCTINTERP_API bool name_current_scope (const std::string& name);

    OCTINTERP_API symbol_scope parent_scope (void) const;

    OCTINTERP_API std::string parent_name (void) const;

    OCTINTERP_API void clear (void);

  private:

    base_parser& m_parser;
    std::deque<value_type> m_info;
    std::set<std::string> m_all_names;
  };

public:

  OCTINTERP_API base_parser (base_lexer& lxr);

  // No copying!

  base_parser (const base_parser&) = delete;

  base_parser& operator = (const base_parser&) = delete;

  virtual ~base_parser (void);

  base_lexer& get_lexer (void) const { return m_lexer; }

  bool at_end_of_input (void) const { return m_lexer.m_end_of_input; }

  OCTINTERP_API void reset (void);

  void classdef_object (const std::shared_ptr<tree_classdef>& obj)
  {
    m_classdef_object = obj;
  }

  std::shared_ptr<tree_classdef> classdef_object (void) const
  {
    return m_classdef_object;
  }

  OCTINTERP_API void statement_list (std::shared_ptr<tree_statement_list>& lst);

  std::shared_ptr<tree_statement_list> statement_list (void) const
  {
    return m_stmt_list;
  }

  void parsing_subfunctions (bool flag)
  {
    m_parsing_subfunctions = flag;
  }

  bool parsing_subfunctions (void) const
  {
    return m_parsing_subfunctions;
  }

  void parsing_local_functions (bool flag)
  {
    m_parsing_local_functions = flag;
  }

  bool parsing_local_functions (void) const
  {
    return m_parsing_local_functions;
  }

  int curr_fcn_depth (void) const
  {
    return m_curr_fcn_depth;
  }

  void endfunction_found (bool flag)
  {
    m_endfunction_found = flag;
  }

  bool endfunction_found (void) const
  {
    return m_endfunction_found;
  }

  // Error messages for mismatched end tokens.
  OCTINTERP_API void
  end_token_error (token *tok, token::end_tok_type expected);

  // Check to see that end tokens are properly matched.
  OCTINTERP_API bool end_token_ok (token *tok, token::end_tok_type expected);

  // Handle pushing symbol table for new function scope.
  OCTINTERP_API bool push_fcn_symtab (void);

  // Build a constant.
  OCTINTERP_API tree_constant * make_constant (token *tok_val);

  OCTINTERP_API tree_black_hole * make_black_hole (void);

  OCTINTERP_API tree_matrix * make_matrix (tree_argument_list *row);

  OCTINTERP_API tree_matrix *
  append_matrix_row (tree_matrix *matrix, tree_argument_list *row);

  OCTINTERP_API tree_cell * make_cell (tree_argument_list *row);

  OCTINTERP_API tree_cell *
  append_cell_row (tree_cell *cell, tree_argument_list *row);

  // Build a function handle.
  OCTINTERP_API tree_fcn_handle * make_fcn_handle (token *tok_val);

  // Build an anonymous function handle.
  OCTINTERP_API tree_anon_fcn_handle *
  make_anon_fcn_handle (tree_parameter_list *param_list,
                        tree_expression *expr, const filepos& at_pos);

  // Build a colon expression.
  OCTINTERP_API tree_expression *
  make_colon_expression (tree_expression *base, tree_expression *limit,
                         tree_expression *incr = nullptr);

  // Build a binary expression.
  OCTINTERP_API tree_expression *
  make_binary_op (int op, tree_expression *op1, token *tok_val,
                  tree_expression *op2);

  // Maybe convert EXPR to a braindead_shortcircuit expression.
  OCTINTERP_API void
  maybe_convert_to_braindead_shortcircuit (tree_expression *&expr);

  // Build a boolean expression.
  OCTINTERP_API tree_expression *
  make_boolean_op (int op, tree_expression *op1, token *tok_val,
                   tree_expression *op2);

  // Build a prefix expression.
  OCTINTERP_API tree_expression *
  make_prefix_op (int op, tree_expression *op1, token *tok_val);

  // Build a postfix expression.
  OCTINTERP_API tree_expression *
  make_postfix_op (int op, tree_expression *op1, token *tok_val);

  // Build an unwind-protect command.
  OCTINTERP_API tree_command *
  make_unwind_command (token *unwind_tok, tree_statement_list *body,
                       tree_statement_list *cleanup, token *end_tok,
                       comment_list *lc, comment_list *mc);

  // Build a try-catch command.
  OCTINTERP_API tree_command *
  make_try_command (token *try_tok, tree_statement_list *body,
                    char catch_sep, tree_statement_list *cleanup,
                    token *end_tok, comment_list *lc,
                    comment_list *mc);

  // Build a while command.
  OCTINTERP_API tree_command *
  make_while_command (token *while_tok, tree_expression *expr,
                      tree_statement_list *body, token *end_tok,
                      comment_list *lc);

  // Build a do-until command.
  OCTINTERP_API tree_command *
  make_do_until_command (token *until_tok, tree_statement_list *body,
                         tree_expression *expr, comment_list *lc);

  // Build a for command.
  OCTINTERP_API tree_command *
  make_for_command (int tok_id, token *for_tok, tree_argument_list *lhs,
                    tree_expression *expr, tree_expression *maxproc,
                    tree_statement_list *body, token *end_tok,
                    comment_list *lc);

  // Build a break command.
  OCTINTERP_API tree_command * make_break_command (token *break_tok);

  // Build a continue command.
  OCTINTERP_API tree_command * make_continue_command (token *continue_tok);

  // Build a return command.
  OCTINTERP_API tree_command * make_return_command (token *return_tok);

  // Build an spmd command.

  OCTINTERP_API tree_spmd_command *
  make_spmd_command (token *spmd_tok, tree_statement_list *body,
                     token *end_tok, comment_list *lc, comment_list *tc);

  // Start an if command.
  OCTINTERP_API tree_if_command_list *
  start_if_command (tree_expression *expr, tree_statement_list *list);

  // Finish an if command.
  OCTINTERP_API tree_if_command *
  finish_if_command (token *if_tok, tree_if_command_list *list,
                     token *end_tok, comment_list *lc);

  // Build an elseif clause.
  OCTINTERP_API tree_if_clause *
  make_elseif_clause (token *elseif_tok, tree_expression *expr,
                      tree_statement_list *list, comment_list *lc);

  OCTINTERP_API tree_if_clause *
  make_else_clause (token *else_tok, comment_list *lc,
                    tree_statement_list *list);

  OCTINTERP_API tree_if_command_list *
  append_if_clause (tree_if_command_list *list, tree_if_clause *clause);

  // Finish a switch command.
  OCTINTERP_API tree_switch_command *
  finish_switch_command (token *switch_tok, tree_expression *expr,
                         tree_switch_case_list *list, token *end_tok,
                         comment_list *lc);

  OCTINTERP_API tree_switch_case_list *
  make_switch_case_list (tree_switch_case *switch_case);

  // Build a switch case.
  OCTINTERP_API tree_switch_case *
  make_switch_case (token *case_tok, tree_expression *expr,
                    tree_statement_list *list, comment_list *lc);

  OCTINTERP_API tree_switch_case *
  make_default_switch_case (token *default_tok, comment_list *lc,
                            tree_statement_list *list);

  OCTINTERP_API tree_switch_case_list *
  append_switch_case (tree_switch_case_list *list, tree_switch_case *elt);

  // Build an assignment to a variable.
  OCTINTERP_API tree_expression *
  make_assign_op (int op, tree_argument_list *lhs, token *eq_tok,
                  tree_expression *rhs);

  // Define a script.
  OCTINTERP_API void
  make_script (tree_statement_list *cmds, tree_statement *end_script);

  // Handle identifier that is recognized as a function name.
  OCTINTERP_API tree_identifier *
  make_fcn_name (tree_identifier *id);

  // Define a function.
  OCTINTERP_API tree_function_def *
  make_function (token *fcn_tok, tree_parameter_list *ret_list,
                 tree_identifier *id, tree_parameter_list *param_list,
                 tree_statement_list *body, tree_statement *end_fcn_stmt,
                 comment_list *lc);

  // Begin defining a function.
  OCTINTERP_API octave_user_function *
  start_function (tree_identifier *id, tree_parameter_list *param_list,
                  tree_statement_list *body, tree_statement *end_function);

  // Create a no-op statement for end_function.
  OCTINTERP_API tree_statement *
  make_end (const std::string& type, bool eof,
            const filepos& beg_pos, const filepos& end_pos);

  // Do most of the work for defining a function.
  OCTINTERP_API octave_user_function *
  frob_function (tree_identifier *id, octave_user_function *fcn);

  // Finish defining a function.
  OCTINTERP_API tree_function_def *
  finish_function (tree_parameter_list *ret_list,
                   octave_user_function *fcn, comment_list *lc,
                   int l, int c);

  OCTINTERP_API tree_statement_list *
  append_function_body (tree_statement_list *body, tree_statement_list *list);

  // Make an arguments validation block.
  OCTINTERP_API tree_arguments_block *
  make_arguments_block (token *arguments_tok,
                        tree_args_block_attribute_list *attr_list,
                        tree_args_block_validation_list *validation_list,
                        token *end_tok, comment_list *lc, comment_list *tc);

  OCTINTERP_API tree_args_block_attribute_list *
  make_args_attribute_list (tree_identifier *attribute_name);

  // Make an argument validation.
  OCTINTERP_API tree_arg_validation *
  make_arg_validation (tree_arg_size_spec *size_spec,
                       tree_identifier *class_name,
                       tree_arg_validation_fcns *validation_fcns,
                       tree_expression *default_value);

  // Make an argument validation list.
  OCTINTERP_API tree_args_block_validation_list *
  make_args_validation_list (tree_arg_validation *arg_validation);

  // Append an argument validation to an existing list.
  OCTINTERP_API tree_args_block_validation_list *
  append_args_validation_list (tree_args_block_validation_list *list,
                               tree_arg_validation *arg_validation);

  // Make an argument size specification object.
  OCTINTERP_API tree_arg_size_spec *
  make_arg_size_spec (tree_argument_list *size_args);

  // Make a list of argument validation functions.
  OCTINTERP_API tree_arg_validation_fcns *
  make_arg_validation_fcns (tree_argument_list *fcn_args);

  // Reset state after parsing function.
  OCTINTERP_API void
  recover_from_parsing_function (void);

  OCTINTERP_API tree_classdef *
  make_classdef (token *tok_val, tree_classdef_attribute_list *a,
                 tree_identifier *id, tree_classdef_superclass_list *sc,
                 tree_classdef_body *body, token *end_tok,
                 comment_list *lc, comment_list *tc);

  OCTINTERP_API tree_classdef_properties_block *
  make_classdef_properties_block (token *tok_val,
                                  tree_classdef_attribute_list *a,
                                  tree_classdef_property_list *plist,
                                  token *end_tok, comment_list *lc,
                                  comment_list *tc);

  OCTINTERP_API tree_classdef_property_list *
  make_classdef_property_list (tree_classdef_property *prop);

  OCTINTERP_API tree_classdef_property *
  make_classdef_property (comment_list *lc, tree_identifier *id,
                          tree_arg_validation *av);

  OCTINTERP_API tree_classdef_property_list *
  append_classdef_property (tree_classdef_property_list *list,
                            tree_classdef_property *elt);

  OCTINTERP_API tree_classdef_methods_block *
  make_classdef_methods_block (token *tok_val,
                               tree_classdef_attribute_list *a,
                               tree_classdef_methods_list *mlist,
                               token *end_tok, comment_list *lc,
                               comment_list *tc);

  OCTINTERP_API tree_classdef_events_block *
  make_classdef_events_block (token *tok_val,
                              tree_classdef_attribute_list *a,
                              tree_classdef_events_list *elist,
                              token *end_tok, comment_list *lc,
                              comment_list *tc);

  OCTINTERP_API tree_classdef_events_list *
  make_classdef_events_list (tree_classdef_event *e);

  OCTINTERP_API tree_classdef_event *
  make_classdef_event (comment_list *lc, tree_identifier *id);

  OCTINTERP_API tree_classdef_events_list *
  append_classdef_event (tree_classdef_events_list *list,
                         tree_classdef_event *elt);

  OCTINTERP_API tree_classdef_enum_block *
  make_classdef_enum_block (token *tok_val,
                            tree_classdef_attribute_list *a,
                            tree_classdef_enum_list *elist,
                            token *end_tok, comment_list *lc,
                            comment_list *tc);

  OCTINTERP_API tree_classdef_enum_list *
  make_classdef_enum_list (tree_classdef_enum *e);

  OCTINTERP_API tree_classdef_enum *
  make_classdef_enum (tree_identifier *id, tree_expression *expr,
                      comment_list *lc);

  OCTINTERP_API tree_classdef_enum_list *
  append_classdef_enum (tree_classdef_enum_list *list,
                        tree_classdef_enum *elt);

  OCTINTERP_API tree_classdef_superclass_list *
  make_classdef_superclass_list (tree_classdef_superclass *sc);

  OCTINTERP_API tree_classdef_superclass *
  make_classdef_superclass (token *fqident);

  OCTINTERP_API tree_classdef_superclass_list *
  append_classdef_superclass (tree_classdef_superclass_list *list,
                              tree_classdef_superclass *elt);

  OCTINTERP_API tree_classdef_attribute_list *
  make_classdef_attribute_list (tree_classdef_attribute *attr);

  OCTINTERP_API tree_classdef_attribute *
  make_classdef_attribute (tree_identifier *id,
                           tree_expression *expr = nullptr);

  OCTINTERP_API tree_classdef_attribute *
  make_not_classdef_attribute (tree_identifier *id);

  OCTINTERP_API tree_classdef_attribute_list *
  append_classdef_attribute (tree_classdef_attribute_list *list,
                             tree_classdef_attribute *elt);

  OCTINTERP_API tree_classdef_body *
  make_classdef_body (tree_classdef_properties_block *pb);

  OCTINTERP_API tree_classdef_body *
  make_classdef_body (tree_classdef_methods_block *mb);

  OCTINTERP_API tree_classdef_body *
  make_classdef_body (tree_classdef_events_block *evb);

  OCTINTERP_API tree_classdef_body *
  make_classdef_body  (tree_classdef_enum_block *enb);

  OCTINTERP_API tree_classdef_body *
  append_classdef_properties_block (tree_classdef_body *body,
                                    tree_classdef_properties_block *block);

  OCTINTERP_API tree_classdef_body *
  append_classdef_methods_block (tree_classdef_body *body,
                                 tree_classdef_methods_block *block);

  OCTINTERP_API tree_classdef_body *
  append_classdef_events_block (tree_classdef_body *body,
                                tree_classdef_events_block *block);

  OCTINTERP_API tree_classdef_body *
  append_classdef_enum_block (tree_classdef_body *body,
                              tree_classdef_enum_block *block);

  OCTINTERP_API octave_user_function *
  start_classdef_external_method (tree_identifier *id,
                                  tree_parameter_list *pl);

  OCTINTERP_API tree_function_def *
  finish_classdef_external_method (octave_user_function *fcn,
                                   tree_parameter_list *ret_list,
                                   comment_list *cl);

  OCTINTERP_API tree_classdef_methods_list *
  make_classdef_methods_list (tree_function_def *fcn_def);

  OCTINTERP_API tree_classdef_methods_list *
  append_classdef_method (tree_classdef_methods_list *list,
                          tree_function_def *fcn_def);

  OCTINTERP_API bool
  finish_classdef_file (tree_classdef *cls,
                        tree_statement_list *local_fcns);

  // Make an index expression.
  OCTINTERP_API tree_index_expression *
  make_index_expression (tree_expression *expr,
                         tree_argument_list *args, char type);

  // Make an indirect reference expression.
  OCTINTERP_API tree_index_expression *
  make_indirect_ref (tree_expression *expr, const std::string&);

  // Make an indirect reference expression with dynamic field name.
  OCTINTERP_API tree_index_expression *
  make_indirect_ref (tree_expression *expr, tree_expression *field);

  // Make a declaration command.
  OCTINTERP_API tree_decl_command *
  make_decl_command (int tok, token *tok_val, tree_decl_init_list *lst);

  OCTINTERP_API tree_decl_init_list *
  make_decl_init_list (tree_decl_elt *elt);

  OCTINTERP_API tree_decl_elt *
  make_decl_elt (tree_identifier *id, token *eq_op = nullptr,
                 tree_expression *expr = nullptr);

  OCTINTERP_API tree_decl_init_list *
  append_decl_init_list (tree_decl_init_list *list, tree_decl_elt *elt);

  // Validate an function parameter list.
  OCTINTERP_API bool
  validate_param_list (tree_parameter_list *lst,
                       tree_parameter_list::in_or_out type);
  // Validate matrix or cell
  OCTINTERP_API bool validate_array_list (tree_expression *e);

  // Validate matrix object used in "[lhs] = ..." assignments.
  OCTINTERP_API tree_argument_list *
  validate_matrix_for_assignment (tree_expression *e);

  // Finish building an array_list (common action for finish_matrix
  // and finish_cell).
  OCTINTERP_API tree_expression *
  finish_array_list (tree_array_list *a, token *open_delim,
                     token *close_delim);

  // Finish building a matrix list.
  OCTINTERP_API tree_expression *
  finish_matrix (tree_matrix *m, token *open_delim, token *close_delim);

  // Finish building a cell list.
  OCTINTERP_API tree_expression *
  finish_cell (tree_cell *c, token *open_delim, token *close_delim);

  OCTINTERP_API tree_identifier *
  make_identifier (token *ident);

  OCTINTERP_API tree_superclass_ref *
  make_superclass_ref (token *superclassref);

  OCTINTERP_API tree_metaclass_query *
  make_metaclass_query (token *metaquery);

  // Set the print flag for a statement based on the separator type.
  OCTINTERP_API tree_statement_list *
  set_stmt_print_flag (tree_statement_list *, char, bool);

  // Finish building a statement.
  template <typename T>
  OCTINTERP_API tree_statement * make_statement (T *arg);

  // Create a statement list.
  OCTINTERP_API tree_statement_list *
  make_statement_list (tree_statement *stmt);

  // Append a statement to an existing statement list.
  OCTINTERP_API tree_statement_list *
  append_statement_list (tree_statement_list *list, char sep,
                         tree_statement *stmt, bool warn_missing_semi);

  OCTINTERP_API tree_argument_list *
  make_argument_list (tree_expression *expr);

  OCTINTERP_API tree_argument_list *
  append_argument_list (tree_argument_list *list, tree_expression *expr);

  OCTINTERP_API tree_parameter_list *
  make_parameter_list (tree_parameter_list::in_or_out io);

  OCTINTERP_API tree_parameter_list *
  make_parameter_list (tree_parameter_list::in_or_out io, tree_decl_elt *t);

  OCTINTERP_API tree_parameter_list *
  make_parameter_list (tree_parameter_list::in_or_out io,
                       tree_identifier *id);

  OCTINTERP_API tree_parameter_list *
  append_parameter_list (tree_parameter_list *list, tree_decl_elt *t);

  OCTINTERP_API tree_parameter_list *
  append_parameter_list (tree_parameter_list *list, tree_identifier *id);

  // Don't allow parsing command syntax.  If the parser/lexer is
  // reset, this setting is also reset to the default (allow command
  // syntax).
  OCTINTERP_API void disallow_command_syntax (void);

  // Generic error messages.
  OCTINTERP_API void bison_error (const std::string& s);
  OCTINTERP_API void bison_error (const std::string& s, const filepos& pos);
  OCTINTERP_API void bison_error (const std::string& s, int line, int column);
  OCTINTERP_API void bison_error (const std::list<parse_exception>& pe);
  OCTINTERP_API void bison_error (const parse_exception& pe);

  friend OCTINTERP_API octave_value
  parse_fcn_file (interpreter& interp, const std::string& full_file,
                  const std::string& file, const std::string& dir_name,
                  const std::string& dispatch_type,
                  const std::string& package_name, bool require_file,
                  bool force_script, bool autoload, bool relative_lookup);

  // Thih interface allows push or pull parsers to be used
  // equivalently, provided that the push parser also owns its input
  // method (see below).  Alternatively, the push parser interface may
  // use a separate run method and completely separate input from
  // lexical analysis and parsing.

  virtual int run (void) = 0;

  // Check primary script or function generated by the parser for
  // semantic errors.
  OCTINTERP_API bool validate_primary_fcn (void);

  OCTINTERP_API bool finish_input (tree_statement_list *lst,
                                   bool at_eof = false);

protected:

  // Contains error message if Bison-generated parser returns non-zero
  // status.
  std::string m_parse_error_msg;

  // Have we found an explicit end to a function?
  bool m_endfunction_found;

  // TRUE means we are in the process of autoloading a function.
  bool m_autoloading;

  // TRUE means the current function file was found in a relative path
  // element.
  bool m_fcn_file_from_relative_lookup;

  // FALSE if we are still at the primary function.  Subfunctions can
  // only be declared inside function files.
  bool m_parsing_subfunctions;

  // TRUE if we are parsing local functions defined at after a
  // classdef block.  Local functions can only be declared inside
  // classdef files.
  bool m_parsing_local_functions;

  // Maximum function depth detected.  Used to determine whether
  // we have nested functions or just implicitly ended subfunctions.
  int m_max_fcn_depth;

  // = 0 currently outside any function.
  // = 1 inside the primary function or a subfunction.
  // > 1 means we are looking at a function definition that seems to be
  //     inside a function.  Note that the function still might not be a
  //     nested function.
  int m_curr_fcn_depth;

  // Scope where we install all subfunctions and nested functions.  Only
  // used while reading function files.
  symbol_scope m_primary_fcn_scope;

  // Name of the current class when we are parsing class methods or
  // constructors.
  std::string m_curr_class_name;

  // Name of the current package when we are parsing an element contained
  // in a package directory (+-directory).
  std::string m_curr_package_name;

  // Nested function scopes and names currently being parsed.
  parent_scope_info m_function_scopes;

  // Pointer to the primary user function or user script function.
  octave_value m_primary_fcn;

  // List of subfunction names, initially in the order they are
  // installed in the symbol table, then ordered as they appear in the
  // file.  Eventually stashed in the primary function object.
  std::list<std::string> m_subfunction_names;

  // Pointer to the classdef object we just parsed, if any.
  std::shared_ptr<tree_classdef> m_classdef_object;

  // Result of parsing input.
  std::shared_ptr <tree_statement_list> m_stmt_list;

  // State of the lexer.
  base_lexer& m_lexer;

  // Internal state of the Bison parser.
  void *m_parser_state;

private:

  // Maybe print a warning if an assignment expression is used as the
  // test in a logical expression.
  OCTINTERP_API void maybe_warn_assign_as_truth_value (tree_expression *expr);

  // Maybe print a warning about switch labels that aren't constants.
  OCTINTERP_API void maybe_warn_variable_switch_label (tree_expression *expr);

  // Maybe print a warning.
  OCTINTERP_API void maybe_warn_missing_semi (tree_statement_list *);
};

// Publish externally used friend functions.

extern OCTINTERP_API octave_value
parse_fcn_file (interpreter& interp, const std::string& full_file,
                const std::string& file, const std::string& dir_name,
                const std::string& dispatch_type,
                const std::string& package_name, bool require_file,
                bool force_script, bool autoload, bool relative_lookup);

class parser : public base_parser
{
public:

  parser (interpreter& interp)
    : base_parser (*(new lexer (interp)))
  { }

  parser (FILE *file, interpreter& interp)
    : base_parser (*(new lexer (file, interp)))
  { }

  parser (FILE *file, interpreter& interp, std::string encoding)
    : base_parser (*(new lexer (file, interp, encoding)))
  { }

  parser (const std::string& eval_string, interpreter& interp)
    : base_parser (*(new lexer (eval_string, interp)))
  { }

  // The lexer must be allocated with new.  The parser object
  // takes ownership of and deletes the lexer object in its
  // destructor.

  parser (lexer *lxr)
    : base_parser (*lxr)
  { }

  // No copying!

  parser (const parser&) = delete;

  parser& operator = (const parser&) = delete;

  ~parser (void) = default;

  OCTINTERP_API int run (void);
};

class push_parser : public base_parser
{
public:

  push_parser (interpreter& interp)
    : base_parser (*(new push_lexer (interp))),
      m_interpreter (interp), m_reader ()
  { }

  // The parser assumes ownership of READER, which must be created
  // with new.

  push_parser (interpreter& interp, input_reader *reader)
    : base_parser (*(new push_lexer (interp))),
      m_interpreter (interp), m_reader (reader)
  { }

  // No copying!

  push_parser (const push_parser&) = delete;

  push_parser& operator = (const push_parser&) = delete;

  ~push_parser (void) = default;

  // Use the push parser in the same way as the pull parser.  The
  // parser arranges for input through the M_READER object.  See, for
  // example, interpreter::main_loop.

  OCTINTERP_API int run (void);

  // Parse INPUT.  M_READER is not used.  The user is responsible for
  // collecting input.

  OCTINTERP_API int run (const std::string& input, bool eof);

private:

  interpreter& m_interpreter;

  std::shared_ptr<input_reader> m_reader;
};

extern OCTINTERP_API std::string
get_help_from_file (const std::string& nm, bool& symbol_found,
                    std::string& file);

extern OCTINTERP_API std::string
get_help_from_file (const std::string& nm, bool& symbol_found);

extern OCTINTERP_API octave_value
load_fcn_from_file (const std::string& file_name,
                    const std::string& dir_name = "",
                    const std::string& dispatch_type = "",
                    const std::string& package_name = "",
                    const std::string& fcn_name = "",
                    bool autoload = false);

extern OCTINTERP_API void
source_file (const std::string& file_name,
             const std::string& context = "",
             bool verbose = false, bool require_file = true);

extern OCTINTERP_API octave_value_list
feval (const char *name,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern OCTINTERP_API octave_value_list
feval (const std::string& name,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern OCTINTERP_API octave_value_list
feval (octave_function *fcn,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern OCTINTERP_API octave_value_list
feval (const octave_value& val,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern OCTINTERP_API octave_value_list
feval (const octave_value_list& args, int nargout = 0);

extern OCTINTERP_API void
cleanup_statement_list (tree_statement_list **lst);

OCTAVE_END_NAMESPACE(octave)

#endif
