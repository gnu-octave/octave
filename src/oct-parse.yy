/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009  John W. Eaton
Copyright (C) 2009 David Grundberg
Copyright (C) 2009, 2010 VZLU Prague

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

// Parser for Octave.

// C decarations.

%{
#define YYDEBUG 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>

#include <iostream>
#include <map>
#include <sstream>

#include "Cell.h"
#include "Matrix.h"
#include "cmd-edit.h"
#include "cmd-hist.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"

#include "comment-list.h"
#include "defaults.h"
#include "defun.h"
#include "dirfns.h"
#include "dynamic-ld.h"
#include "error.h"
#include "input.h"
#include "lex.h"
#include "load-path.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "ov-null-mat.h"
#include "toplev.h"
#include "pager.h"
#include "parse.h"
#include "pt-all.h"
#include "pt-eval.h"
#include "symtab.h"
#include "token.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#if defined (GNULIB_NAMESPACE)
// Calls to the following functions appear in the generated output from
// Bison without the namespace tag.  Redefine them so we will use them
// via the gnulib namespace.
#define fclose GNULIB_NAMESPACE::fclose
#define fprintf GNULIB_NAMESPACE::fprintf
#define malloc GNULIB_NAMESPACE::malloc
#endif

// The current input line number.
int input_line_number = 1;

// The column of the current token.
int current_input_column = 1;

// Buffer for help text snagged from function files.
std::stack<std::string> help_buf;

// Buffer for comments appearing before a function statement.
static std::string fcn_comment_header;

// TRUE means we are using readline.
// (--no-line-editing)
bool line_editing = true;

// TRUE means we printed messages about reading startup files.
bool reading_startup_message_printed = false;

// TRUE means input is coming from startup file.
bool input_from_startup_file = false;

// = 0 currently outside any function.
// = 1 inside the primary function or a subfunction.
// > 1 means we are looking at a function definition that seems to be
//     inside a function. Note that the function still might not be a
//     nested function.
static int current_function_depth = 0;

// Maximum function depth detected. Just here to determine whether
// we have nested functions or just implicitly ended subfunctions.
static int max_function_depth = 0;

// FALSE if we are still at the primary function. Subfunctions can
// only be declared inside function files.
static int parsing_subfunctions = false;

// Have we found an explicit end to a function?
static bool endfunction_found = false;

// Keep track of symbol table information when parsing functions.
std::stack<symbol_table::scope_id> symtab_context;

// Name of the current class when we are parsing class methods or
// constructors.
std::string current_class_name;

// TRUE means we are in the process of autoloading a function.
static bool autoloading = false;

// TRUE means the current function file was found in a relative path
// element.
static bool fcn_file_from_relative_lookup = false;

// Pointer to the primary user function or user script function.
static octave_function *primary_fcn_ptr = 0;

// Scope where we install all subfunctions and nested functions. Only
// used while reading function files.
static symbol_table::scope_id primary_fcn_scope;

// List of autoloads (function -> file mapping).
static std::map<std::string, std::string> autoload_map;

// Forward declarations for some functions defined at the bottom of
// the file.

// Generic error messages.
static void
yyerror (const char *s);

// Error mesages for mismatched end tokens.
static void
end_error (const char *type, token::end_tok_type ettype, int l, int c);

// Check to see that end tokens are properly matched.
static bool
end_token_ok (token *tok, token::end_tok_type expected);

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.
static void
maybe_warn_assign_as_truth_value (tree_expression *expr);

// Maybe print a warning about switch labels that aren't constants.
static void
maybe_warn_variable_switch_label (tree_expression *expr);

// Finish building a range.
static tree_expression *
finish_colon_expression (tree_colon_expression *e);

// Build a constant.
static tree_constant *
make_constant (int op, token *tok_val);

// Build a function handle.
static tree_fcn_handle *
make_fcn_handle (token *tok_val);

// Build an anonymous function handle.
static tree_anon_fcn_handle *
make_anon_fcn_handle (tree_parameter_list *param_list, tree_statement *stmt);

// Build a binary expression.
static tree_expression *
make_binary_op (int op, tree_expression *op1, token *tok_val,
                tree_expression *op2);

// Build a boolean expression.
static tree_expression *
make_boolean_op (int op, tree_expression *op1, token *tok_val,
                 tree_expression *op2);

// Build a prefix expression.
static tree_expression *
make_prefix_op (int op, tree_expression *op1, token *tok_val);

// Build a postfix expression.
static tree_expression *
make_postfix_op (int op, tree_expression *op1, token *tok_val);

// Build an unwind-protect command.
static tree_command *
make_unwind_command (token *unwind_tok, tree_statement_list *body,
                     tree_statement_list *cleanup, token *end_tok,
                     octave_comment_list *lc, octave_comment_list *mc);

// Build a try-catch command.
static tree_command *
make_try_command (token *try_tok, tree_statement_list *body,
                  tree_statement_list *cleanup, token *end_tok,
                  octave_comment_list *lc, octave_comment_list *mc);

// Build a while command.
static tree_command *
make_while_command (token *while_tok, tree_expression *expr,
                    tree_statement_list *body, token *end_tok,
                    octave_comment_list *lc);

// Build a do-until command.
static tree_command *
make_do_until_command (token *until_tok, tree_statement_list *body,
                       tree_expression *expr, octave_comment_list *lc);

// Build a for command.
static tree_command *
make_for_command (token *for_tok, tree_argument_list *lhs,
                  tree_expression *expr, tree_statement_list *body,
                  token *end_tok, octave_comment_list *lc);

// Build a break command.
static tree_command *
make_break_command (token *break_tok);

// Build a continue command.
static tree_command *
make_continue_command (token *continue_tok);

// Build a return command.
static tree_command *
make_return_command (token *return_tok);

// Start an if command.
static tree_if_command_list *
start_if_command (tree_expression *expr, tree_statement_list *list);

// Finish an if command.
static tree_if_command *
finish_if_command (token *if_tok, tree_if_command_list *list,
                   token *end_tok, octave_comment_list *lc);

// Build an elseif clause.
static tree_if_clause *
make_elseif_clause (token *elseif_tok, tree_expression *expr,
                    tree_statement_list *list, octave_comment_list *lc);

// Finish a switch command.
static tree_switch_command *
finish_switch_command (token *switch_tok, tree_expression *expr,
                       tree_switch_case_list *list, token *end_tok,
                       octave_comment_list *lc);

// Build a switch case.
static tree_switch_case *
make_switch_case (token *case_tok, tree_expression *expr,
                  tree_statement_list *list, octave_comment_list *lc);

// Build an assignment to a variable.
static tree_expression *
make_assign_op (int op, tree_argument_list *lhs, token *eq_tok,
                tree_expression *rhs);

// Define a script.
static void
make_script (tree_statement_list *cmds, tree_statement *end_script);

// Begin defining a function.
static octave_user_function *
start_function (tree_parameter_list *param_list, tree_statement_list *body,
                tree_statement *end_function);

// Create a no-op statement for end_function.
static tree_statement *
make_end (const std::string& type, int l, int c);

// Do most of the work for defining a function.
static octave_user_function *
frob_function (const std::string& fname, octave_user_function *fcn);

// Finish defining a function.
static tree_function_def *
finish_function (tree_parameter_list *ret_list,
                 octave_user_function *fcn, octave_comment_list *lc);

// Reset state after parsing function.
static void
recover_from_parsing_function (void);

// Make an index expression.
static tree_index_expression *
make_index_expression (tree_expression *expr,
                       tree_argument_list *args, char type);

// Make an indirect reference expression.
static tree_index_expression *
make_indirect_ref (tree_expression *expr, const std::string&);

// Make an indirect reference expression with dynamic field name.
static tree_index_expression *
make_indirect_ref (tree_expression *expr, tree_expression *field);

// Make a declaration command.
static tree_decl_command *
make_decl_command (int tok, token *tok_val, tree_decl_init_list *lst);

// Validate argument list forming a matrix or cell row.
static tree_argument_list *
validate_matrix_row (tree_argument_list *row);

// Finish building a matrix list.
static tree_expression *
finish_matrix (tree_matrix *m);

// Finish building a cell list.
static tree_expression *
finish_cell (tree_cell *c);

// Maybe print a warning.  Duh.
static void
maybe_warn_missing_semi (tree_statement_list *);

// Set the print flag for a statement based on the separator type.
static tree_statement_list *
set_stmt_print_flag (tree_statement_list *, char, bool);

// Create a statement list.
static tree_statement_list *make_statement_list (tree_statement *stmt);

// Append a statement to an existing statement list.
static tree_statement_list *
append_statement_list (tree_statement_list *list, char sep,
                       tree_statement *stmt, bool warn_missing_semi);

// Finish building a statement.
template <class T>
static tree_statement *
make_statement (T *arg)
{
  octave_comment_list *comment = octave_comment_buffer::get_comment ();

  return new tree_statement (arg, comment);
}

#define ABORT_PARSE \
  do \
    { \
      global_command = 0; \
      yyerrok; \
      if (! symtab_context.empty ()) \
        { \
          symbol_table::set_scope (symtab_context.top ()); \
          symtab_context.pop (); \
        } \
      if (interactive || forced_interactive) \
        YYACCEPT; \
      else \
        YYABORT; \
    } \
  while (0)

%}

// Bison declarations.

// Don't add spaces around the = here; it causes some versions of
// bison to fail to properly recognize the directive.

%name-prefix="octave_"

%union
{
  // The type of the basic tokens returned by the lexer.
  token *tok_val;

  // Comment strings that we need to deal with mid-rule.
  octave_comment_list *comment_type;

  // Types for the nonterminals we generate.
  char sep_type;
  tree *tree_type;
  tree_matrix *tree_matrix_type;
  tree_cell *tree_cell_type;
  tree_expression *tree_expression_type;
  tree_constant *tree_constant_type;
  tree_fcn_handle *tree_fcn_handle_type;
  tree_anon_fcn_handle *tree_anon_fcn_handle_type;
  tree_identifier *tree_identifier_type;
  tree_index_expression *tree_index_expression_type;
  tree_colon_expression *tree_colon_expression_type;
  tree_argument_list *tree_argument_list_type;
  tree_parameter_list *tree_parameter_list_type;
  tree_command *tree_command_type;
  tree_if_command *tree_if_command_type;
  tree_if_clause *tree_if_clause_type;
  tree_if_command_list *tree_if_command_list_type;
  tree_switch_command *tree_switch_command_type;
  tree_switch_case *tree_switch_case_type;
  tree_switch_case_list *tree_switch_case_list_type;
  tree_decl_elt *tree_decl_elt_type;
  tree_decl_init_list *tree_decl_init_list_type;
  tree_decl_command *tree_decl_command_type;
  tree_statement *tree_statement_type;
  tree_statement_list *tree_statement_list_type;
  octave_user_function *octave_user_function_type;
  void *dummy_type;
}

// Tokens with line and column information.
%token <tok_val> '=' ':' '-' '+' '*' '/'
%token <tok_val> ADD_EQ SUB_EQ MUL_EQ DIV_EQ LEFTDIV_EQ POW_EQ
%token <tok_val> EMUL_EQ EDIV_EQ ELEFTDIV_EQ EPOW_EQ AND_EQ OR_EQ
%token <tok_val> LSHIFT_EQ RSHIFT_EQ LSHIFT RSHIFT
%token <tok_val> EXPR_AND_AND EXPR_OR_OR
%token <tok_val> EXPR_AND EXPR_OR EXPR_NOT
%token <tok_val> EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%token <tok_val> LEFTDIV EMUL EDIV ELEFTDIV EPLUS EMINUS
%token <tok_val> QUOTE TRANSPOSE
%token <tok_val> PLUS_PLUS MINUS_MINUS POW EPOW
%token <tok_val> NUM IMAG_NUM
%token <tok_val> STRUCT_ELT
%token <tok_val> NAME
%token <tok_val> END
%token <tok_val> DQ_STRING SQ_STRING
%token <tok_val> FOR WHILE DO UNTIL
%token <tok_val> IF ELSEIF ELSE
%token <tok_val> SWITCH CASE OTHERWISE
%token <tok_val> BREAK CONTINUE FUNC_RET
%token <tok_val> UNWIND CLEANUP
%token <tok_val> TRY CATCH
%token <tok_val> GLOBAL STATIC
%token <tok_val> FCN_HANDLE
%token <tok_val> PROPERTIES
%token <tok_val> METHODS
%token <tok_val> EVENTS
%token <tok_val> METAQUERY
%token <tok_val> SUPERCLASSREF
%token <tok_val> GET SET

// Other tokens.
%token END_OF_INPUT LEXICAL_ERROR
%token FCN SCRIPT_FILE FUNCTION_FILE CLASSDEF
// %token VARARGIN VARARGOUT
%token CLOSE_BRACE

// Nonterminals we construct.
%type <comment_type> stash_comment function_beg classdef_beg
%type <comment_type> properties_beg methods_beg events_beg
%type <sep_type> sep_no_nl opt_sep_no_nl sep opt_sep
%type <tree_type> input
%type <tree_constant_type> string constant magic_colon
%type <tree_anon_fcn_handle_type> anon_fcn_handle
%type <tree_fcn_handle_type> fcn_handle
%type <tree_matrix_type> matrix_rows matrix_rows1
%type <tree_cell_type> cell_rows cell_rows1
%type <tree_expression_type> matrix cell
%type <tree_expression_type> primary_expr postfix_expr prefix_expr binary_expr
%type <tree_expression_type> simple_expr colon_expr assign_expr expression
%type <tree_identifier_type> identifier fcn_name magic_tilde
%type <tree_identifier_type> superclass_identifier meta_identifier
%type <octave_user_function_type> function1 function2 classdef1
%type <tree_index_expression_type> word_list_cmd
%type <tree_colon_expression_type> colon_expr1
%type <tree_argument_list_type> arg_list word_list assign_lhs
%type <tree_argument_list_type> cell_or_matrix_row
%type <tree_parameter_list_type> param_list param_list1 param_list2
%type <tree_parameter_list_type> return_list return_list1
%type <tree_parameter_list_type> superclasses opt_superclasses
%type <tree_command_type> command select_command loop_command
%type <tree_command_type> jump_command except_command function
%type <tree_command_type> script_file classdef
%type <tree_command_type> function_file function_list
%type <tree_if_command_type> if_command
%type <tree_if_clause_type> elseif_clause else_clause
%type <tree_if_command_list_type> if_cmd_list1 if_cmd_list
%type <tree_switch_command_type> switch_command
%type <tree_switch_case_type> switch_case default_case
%type <tree_switch_case_list_type> case_list1 case_list
%type <tree_decl_elt_type> decl2
%type <tree_decl_init_list_type> decl1
%type <tree_decl_command_type> declaration
%type <tree_statement_type> statement function_end classdef_end
%type <tree_statement_list_type> simple_list simple_list1 list list1
%type <tree_statement_list_type> opt_list input1
// These types need to be specified.
%type <dummy_type> attr
%type <dummy_type> class_event
%type <dummy_type> class_property
%type <dummy_type> properties_list
%type <dummy_type> properties_block
%type <dummy_type> methods_list
%type <dummy_type> methods_block
%type <dummy_type> opt_attr_list
%type <dummy_type> attr_list
%type <dummy_type> events_list
%type <dummy_type> events_block
%type <dummy_type> class_body

// Precedence and associativity.
%left ';' ',' '\n'
%right '=' ADD_EQ SUB_EQ MUL_EQ DIV_EQ LEFTDIV_EQ POW_EQ EMUL_EQ EDIV_EQ ELEFTDIV_EQ EPOW_EQ OR_EQ AND_EQ LSHIFT_EQ RSHIFT_EQ
%left EXPR_OR_OR
%left EXPR_AND_AND
%left EXPR_OR
%left EXPR_AND
%left EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%left LSHIFT RSHIFT
%left ':'
%left '-' '+' EPLUS EMINUS
%left '*' '/' LEFTDIV EMUL EDIV ELEFTDIV
%left UNARY PLUS_PLUS MINUS_MINUS EXPR_NOT
%left POW EPOW QUOTE TRANSPOSE
%left '(' '.' '{'

// Where to start.
%start input

%%

// ==============================
// Statements and statement lists
// ==============================

input           : input1
                  {
                    global_command = $1;
                    promptflag = 1;
                    YYACCEPT;
                  }
                | function_file
                  {
                    YYACCEPT;
                  }
                | simple_list parse_error
                  { ABORT_PARSE; }
                | parse_error
                  { ABORT_PARSE; }
                ;

input1          : '\n'
                  { $$ = 0; }
                | END_OF_INPUT
                  {
                    parser_end_of_input = 1;
                    $$ = 0;
                  }
                | simple_list
                  { $$ = $1; }
                | simple_list '\n'
                  { $$ = $1; }
                | simple_list END_OF_INPUT
                  { $$ = $1; }
                ;

simple_list     : simple_list1 opt_sep_no_nl
                  { $$ = set_stmt_print_flag ($1, $2, false); }
                ;

simple_list1    : statement
                  { $$ = make_statement_list ($1); }
                | simple_list1 sep_no_nl statement
                  { $$ = append_statement_list ($1, $2, $3, false); }
                ;

opt_list        : // empty
                  { $$ = new tree_statement_list (); }
                | list
                  { $$ = $1; }
                ;

list            : list1 opt_sep
                  { $$ = set_stmt_print_flag ($1, $2, true); }
                ;

list1           : statement
                  { $$ = make_statement_list ($1); }
                | list1 sep statement
                  { $$ = append_statement_list ($1, $2, $3, true); }
                ;

statement       : expression
                  { $$ = make_statement ($1); }
                | command
                  { $$ = make_statement ($1); }
                | word_list_cmd
                  { $$ = make_statement ($1); }
                ;

// =================
// Word-list command
// =================

// These are not really like expressions since they can't appear on
// the RHS of an assignment.  But they are also not like commands (IF,
// WHILE, etc.

word_list_cmd   : identifier word_list
                  { $$ = make_index_expression ($1, $2, '('); }
                ;

word_list       : string
                  { $$ = new tree_argument_list ($1); }
                | word_list string
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

// ===========
// Expressions
// ===========

identifier      : NAME
                  {
                    symbol_table::symbol_record *sr = $1->sym_rec ();
                    $$ = new tree_identifier (*sr, $1->line (), $1->column ());
                  }
                ;

superclass_identifier
                : SUPERCLASSREF
                  { $$ = new tree_identifier ($1->line (), $1->column ()); }
                ;
            
meta_identifier : METAQUERY
                  { $$ = new tree_identifier ($1->line (), $1->column ()); }
                ;           

string          : DQ_STRING
                  { $$ = make_constant (DQ_STRING, $1); }
                | SQ_STRING
                  { $$ = make_constant (SQ_STRING, $1); }
                ;

constant        : NUM
                  { $$ = make_constant (NUM, $1); }
                | IMAG_NUM
                  { $$ = make_constant (IMAG_NUM, $1); }
                | string
                  { $$ = $1; }
                ;

matrix          : '[' ']'
                  {
                    $$ = new tree_constant (octave_null_matrix::instance);
                    lexer_flags.looking_at_matrix_or_assign_lhs = false;
                    lexer_flags.pending_local_variables.clear ();
                  }
                | '[' ';' ']'
                  {
                    $$ = new tree_constant (octave_null_matrix::instance);
                    lexer_flags.looking_at_matrix_or_assign_lhs = false;
                    lexer_flags.pending_local_variables.clear ();
                  }
                | '[' ',' ']'
                  {
                    $$ = new tree_constant (octave_null_matrix::instance);
                    lexer_flags.looking_at_matrix_or_assign_lhs = false;
                    lexer_flags.pending_local_variables.clear ();
                  }
                | '[' matrix_rows ']'
                  {
                    $$ = finish_matrix ($2);
                    lexer_flags.looking_at_matrix_or_assign_lhs = false;
                    lexer_flags.pending_local_variables.clear ();
                  }
                ;

matrix_rows     : matrix_rows1
                  { $$ = $1; }
                | matrix_rows1 ';'      // Ignore trailing semicolon.
                  { $$ = $1; }
                ;

matrix_rows1    : cell_or_matrix_row
                  { $$ = new tree_matrix ($1); }
                | matrix_rows1 ';' cell_or_matrix_row
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                ;

cell            : '{' '}'
                  { $$ = new tree_constant (octave_value (Cell ())); }
                | '{' ';' '}'
                  { $$ = new tree_constant (octave_value (Cell ())); }
                | '{' cell_rows '}'
                  { $$ = finish_cell ($2); }
                ;

cell_rows       : cell_rows1
                  { $$ = $1; }
                | cell_rows1 ';'        // Ignore trailing semicolon.
                  { $$ = $1; }
                ;

cell_rows1      : cell_or_matrix_row
                  { $$ = new tree_cell ($1); }
                | cell_rows1 ';' cell_or_matrix_row
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                ;

cell_or_matrix_row
                : arg_list
                  { $$ = validate_matrix_row ($1); }
                | arg_list ','  // Ignore trailing comma.
                  { $$ = validate_matrix_row ($1); }
                ;

fcn_handle      : '@' FCN_HANDLE
                  {
                    $$ = make_fcn_handle ($2);
                    lexer_flags.looking_at_function_handle--;
                  }
                ;

anon_fcn_handle : '@' param_list statement
                  { $$ = make_anon_fcn_handle ($2, $3); }
                ;
        
primary_expr    : identifier
                  { $$ = $1; }
                | constant
                  { $$ = $1; }
                | fcn_handle
                  { $$ = $1; }
                | matrix
                  { $$ = $1; }
                | cell
                  { $$ = $1; }
                | meta_identifier
                  { $$ = $1; }
                | superclass_identifier
                  { $$ = $1; }
                | '(' expression ')'
                  { $$ = $2->mark_in_parens (); }
                ;

magic_colon     : ':'
                  {
                    octave_value tmp (octave_value::magic_colon_t);
                    $$ = new tree_constant (tmp);
                  }
                ;

magic_tilde     : EXPR_NOT
                  {
                    $$ = new tree_black_hole ();
                  }
                ;

arg_list        : expression
                  { $$ = new tree_argument_list ($1); }
                | magic_colon
                  { $$ = new tree_argument_list ($1); }
                | magic_tilde
                  { $$ = new tree_argument_list ($1); }
                | arg_list ',' magic_colon
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                | arg_list ',' magic_tilde
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                | arg_list ',' expression
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                ;

indirect_ref_op : '.'
                  { lexer_flags.looking_at_indirect_ref = true; }
                ;

postfix_expr    : primary_expr
                  { $$ = $1; }
                | postfix_expr '(' ')'
                  { $$ = make_index_expression ($1, 0, '('); }
                | postfix_expr '(' arg_list ')'
                  { $$ = make_index_expression ($1, $3, '('); }
                | postfix_expr '{' '}'
                  { $$ = make_index_expression ($1, 0, '{'); }
                | postfix_expr '{' arg_list '}'
                  { $$ = make_index_expression ($1, $3, '{'); }
                | postfix_expr PLUS_PLUS
                  { $$ = make_postfix_op (PLUS_PLUS, $1, $2); }
                | postfix_expr MINUS_MINUS
                  { $$ = make_postfix_op (MINUS_MINUS, $1, $2); }
                | postfix_expr QUOTE
                  { $$ = make_postfix_op (QUOTE, $1, $2); }
                | postfix_expr TRANSPOSE
                  { $$ = make_postfix_op (TRANSPOSE, $1, $2); }
                | postfix_expr indirect_ref_op STRUCT_ELT
                  { $$ = make_indirect_ref ($1, $3->text ()); }
                | postfix_expr indirect_ref_op '(' expression ')'
                  { $$ = make_indirect_ref ($1, $4); }
                ;

prefix_expr     : postfix_expr
                  { $$ = $1; }
                | binary_expr
                  { $$ = $1; }
                | PLUS_PLUS prefix_expr %prec UNARY
                  { $$ = make_prefix_op (PLUS_PLUS, $2, $1); }
                | MINUS_MINUS prefix_expr %prec UNARY
                  { $$ = make_prefix_op (MINUS_MINUS, $2, $1); }
                | EXPR_NOT prefix_expr %prec UNARY
                  { $$ = make_prefix_op (EXPR_NOT, $2, $1); }
                | '+' prefix_expr %prec UNARY
                  { $$ = make_prefix_op ('+', $2, $1); }
                | '-' prefix_expr %prec UNARY
                  { $$ = make_prefix_op ('-', $2, $1); }
                ;

binary_expr     : prefix_expr POW prefix_expr
                  { $$ = make_binary_op (POW, $1, $2, $3); }
                | prefix_expr EPOW prefix_expr
                  { $$ = make_binary_op (EPOW, $1, $2, $3); }
                | prefix_expr '+' prefix_expr
                  { $$ = make_binary_op ('+', $1, $2, $3); }
                | prefix_expr '-' prefix_expr
                  { $$ = make_binary_op ('-', $1, $2, $3); }
                | prefix_expr '*' prefix_expr
                  { $$ = make_binary_op ('*', $1, $2, $3); }
                | prefix_expr '/' prefix_expr
                  { $$ = make_binary_op ('/', $1, $2, $3); }
                | prefix_expr EPLUS prefix_expr
                  { $$ = make_binary_op ('+', $1, $2, $3); }
                | prefix_expr EMINUS prefix_expr
                  { $$ = make_binary_op ('-', $1, $2, $3); }
                | prefix_expr EMUL prefix_expr
                  { $$ = make_binary_op (EMUL, $1, $2, $3); }
                | prefix_expr EDIV prefix_expr
                  { $$ = make_binary_op (EDIV, $1, $2, $3); }
                | prefix_expr LEFTDIV prefix_expr
                  { $$ = make_binary_op (LEFTDIV, $1, $2, $3); }
                | prefix_expr ELEFTDIV prefix_expr
                  { $$ = make_binary_op (ELEFTDIV, $1, $2, $3); }
                ;

colon_expr      : colon_expr1
                  { $$ = finish_colon_expression ($1); }
                ;

colon_expr1     : prefix_expr
                  { $$ = new tree_colon_expression ($1); }
                | colon_expr1 ':' prefix_expr
                  {
                    if (! ($$ = $1->append ($3)))
                      ABORT_PARSE;
                  }
                ;

simple_expr     : colon_expr
                  { $$ = $1; }
                | simple_expr LSHIFT simple_expr
                  { $$ = make_binary_op (LSHIFT, $1, $2, $3); }
                | simple_expr RSHIFT simple_expr
                  { $$ = make_binary_op (RSHIFT, $1, $2, $3); }
                | simple_expr EXPR_LT simple_expr
                  { $$ = make_binary_op (EXPR_LT, $1, $2, $3); }
                | simple_expr EXPR_LE simple_expr
                  { $$ = make_binary_op (EXPR_LE, $1, $2, $3); }
                | simple_expr EXPR_EQ simple_expr
                  { $$ = make_binary_op (EXPR_EQ, $1, $2, $3); }
                | simple_expr EXPR_GE simple_expr
                  { $$ = make_binary_op (EXPR_GE, $1, $2, $3); }
                | simple_expr EXPR_GT simple_expr
                  { $$ = make_binary_op (EXPR_GT, $1, $2, $3); }
                | simple_expr EXPR_NE simple_expr
                  { $$ = make_binary_op (EXPR_NE, $1, $2, $3); }
                | simple_expr EXPR_AND simple_expr
                  { $$ = make_binary_op (EXPR_AND, $1, $2, $3); }
                | simple_expr EXPR_OR simple_expr
                  { $$ = make_binary_op (EXPR_OR, $1, $2, $3); }
                | simple_expr EXPR_AND_AND simple_expr
                  { $$ = make_boolean_op (EXPR_AND_AND, $1, $2, $3); }
                | simple_expr EXPR_OR_OR simple_expr
                  { $$ = make_boolean_op (EXPR_OR_OR, $1, $2, $3); }
                ;

// Arrange for the lexer to return CLOSE_BRACE for `]' by looking ahead
// one token for an assignment op.

assign_lhs      : simple_expr
                  {
                    $$ = new tree_argument_list ($1);
                    $$->mark_as_simple_assign_lhs ();
                  }
                | '[' arg_list CLOSE_BRACE
                  {
                    $$ = $2;
                    lexer_flags.looking_at_matrix_or_assign_lhs = false;
                    for (std::set<std::string>::const_iterator p = lexer_flags.pending_local_variables.begin ();
                         p != lexer_flags.pending_local_variables.end ();
                         p++)
                      {
                        symbol_table::force_variable (*p);
                      }
                    lexer_flags.pending_local_variables.clear ();
                  }
                ;

assign_expr     : assign_lhs '=' expression
                  { $$ = make_assign_op ('=', $1, $2, $3); }
                | assign_lhs ADD_EQ expression
                  { $$ = make_assign_op (ADD_EQ, $1, $2, $3); }
                | assign_lhs SUB_EQ expression
                  { $$ = make_assign_op (SUB_EQ, $1, $2, $3); }
                | assign_lhs MUL_EQ expression
                  { $$ = make_assign_op (MUL_EQ, $1, $2, $3); }
                | assign_lhs DIV_EQ expression
                  { $$ = make_assign_op (DIV_EQ, $1, $2, $3); }
                | assign_lhs LEFTDIV_EQ expression
                  { $$ = make_assign_op (LEFTDIV_EQ, $1, $2, $3); }
                | assign_lhs POW_EQ expression
                  { $$ = make_assign_op (POW_EQ, $1, $2, $3); }
                | assign_lhs LSHIFT_EQ expression
                  { $$ = make_assign_op (LSHIFT_EQ, $1, $2, $3); }
                | assign_lhs RSHIFT_EQ expression
                  { $$ = make_assign_op (RSHIFT_EQ, $1, $2, $3); }
                | assign_lhs EMUL_EQ expression
                  { $$ = make_assign_op (EMUL_EQ, $1, $2, $3); }
                | assign_lhs EDIV_EQ expression
                  { $$ = make_assign_op (EDIV_EQ, $1, $2, $3); }
                | assign_lhs ELEFTDIV_EQ expression
                  { $$ = make_assign_op (ELEFTDIV_EQ, $1, $2, $3); }
                | assign_lhs EPOW_EQ expression
                  { $$ = make_assign_op (EPOW_EQ, $1, $2, $3); }
                | assign_lhs AND_EQ expression
                  { $$ = make_assign_op (AND_EQ, $1, $2, $3); }
                | assign_lhs OR_EQ expression
                  { $$ = make_assign_op (OR_EQ, $1, $2, $3); }
                ;

expression      : simple_expr
                  { $$ = $1; }
                | assign_expr
                  { $$ = $1; }
                | anon_fcn_handle
                  { $$ = $1; }
                ;

// ================================================
// Commands, declarations, and function definitions
// ================================================

command         : declaration
                  { $$ = $1; }
                | select_command
                  { $$ = $1; }
                | loop_command
                  { $$ = $1; }
                | jump_command
                  { $$ = $1; }
                | except_command
                  { $$ = $1; }
                | function
                  { $$ = $1; }
                | script_file
                  { $$ = $1; }
                | classdef
                  { $$ = $1; }
                ;

// =====================
// Declaration statemnts
// =====================

parsing_decl_list
                : // empty
                  { lexer_flags.looking_at_decl_list = true; }

declaration     : GLOBAL parsing_decl_list decl1
                  {
                    $$ = make_decl_command (GLOBAL, $1, $3);
                    lexer_flags.looking_at_decl_list = false;
                  }
                | STATIC parsing_decl_list decl1
                  {
                    $$ = make_decl_command (STATIC, $1, $3);
                    lexer_flags.looking_at_decl_list = false;
                  }
                ;

decl1           : decl2
                  { $$ = new tree_decl_init_list ($1); }
                | decl1 decl2
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

decl_param_init : // empty
                { lexer_flags.looking_at_initializer_expression = true; }

decl2           : identifier
                  { $$ = new tree_decl_elt ($1); }
                | identifier '=' decl_param_init expression
                  {
                    lexer_flags.looking_at_initializer_expression = false;
                    $$ = new tree_decl_elt ($1, $4);
                  }
                | magic_tilde
                  {
                    $$ = new tree_decl_elt ($1);
                  }
                ;

// ====================
// Selection statements
// ====================

select_command  : if_command
                  { $$ = $1; }
                | switch_command
                  { $$ = $1; }
                ;

// ============
// If statement
// ============

if_command      : IF stash_comment if_cmd_list END
                  {
                    if (! ($$ = finish_if_command ($1, $3, $4, $2)))
                      ABORT_PARSE;
                  }
                ;

if_cmd_list     : if_cmd_list1
                  { $$ = $1; }
                | if_cmd_list1 else_clause
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

if_cmd_list1    : expression opt_sep opt_list
                  { $$ = start_if_command ($1, $3); }
                | if_cmd_list1 elseif_clause
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

elseif_clause   : ELSEIF stash_comment opt_sep expression opt_sep opt_list
                  { $$ = make_elseif_clause ($1, $4, $6, $2); }
                ;

else_clause     : ELSE stash_comment opt_sep opt_list
                  { $$ = new tree_if_clause ($4, $2); }
                ;

// ================
// Switch statement
// ================

switch_command  : SWITCH stash_comment expression opt_sep case_list END
                  {
                    if (! ($$ = finish_switch_command ($1, $3, $5, $6, $2)))
                      ABORT_PARSE;
                  }
                ;

case_list       : // empty
                  { $$ = new tree_switch_case_list (); }
                | case_list1
                  { $$ = $1; }
                | case_list1 default_case
                  {
                    $1->append ($2);
                    $$ = $1;
                  }             
                ;

case_list1      : switch_case
                  { $$ = new tree_switch_case_list ($1); }
                | case_list1 switch_case
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

switch_case     : CASE stash_comment opt_sep expression opt_sep opt_list
                  { $$ = make_switch_case ($1, $4, $6, $2); }
                ;

default_case    : OTHERWISE stash_comment opt_sep opt_list
                  {
                    $$ = new tree_switch_case ($4, $2);
                  }
                ;

// =======
// Looping
// =======

loop_command    : WHILE stash_comment expression opt_sep opt_list END
                  {
                    if (! ($$ = make_while_command ($1, $3, $5, $6, $2)))
                      ABORT_PARSE;
                  }
                | DO stash_comment opt_sep opt_list UNTIL expression
                  {
                    if (! ($$ = make_do_until_command ($5, $4, $6, $2)))
                      ABORT_PARSE;
                  }
                | FOR stash_comment assign_lhs '=' expression opt_sep opt_list END
                  {
                    if (! ($$ = make_for_command ($1, $3, $5, $7, $8, $2)))
                      ABORT_PARSE;
                  }
                | FOR stash_comment '(' assign_lhs '=' expression ')' opt_sep opt_list END
                  {
                    if (! ($$ = make_for_command ($1, $4, $6, $9, $10, $2)))
                      ABORT_PARSE;
                  }
                ;

// =======
// Jumping
// =======

jump_command    : BREAK
                  {
                    if (! ($$ = make_break_command ($1)))
                      ABORT_PARSE;
                  }
                | CONTINUE
                  {
                    if (! ($$ = make_continue_command ($1)))
                      ABORT_PARSE;
                  }
                | FUNC_RET
                  {
                    if (! ($$ = make_return_command ($1)))
                      ABORT_PARSE;
                  }
                ;

// ==========
// Exceptions
// ==========

except_command  : UNWIND stash_comment opt_sep opt_list CLEANUP
                  stash_comment opt_sep opt_list END
                  {
                    if (! ($$ = make_unwind_command ($1, $4, $8, $9, $2, $6)))
                      ABORT_PARSE;
                  }
                | TRY stash_comment opt_sep opt_list CATCH
                  stash_comment opt_sep opt_list END
                  {
                    if (! ($$ = make_try_command ($1, $4, $8, $9, $2, $6)))
                      ABORT_PARSE;
                  }
                | TRY stash_comment opt_sep opt_list END
                  {
                    if (! ($$ = make_try_command ($1, $4, 0, $5, $2, 0)))
                      ABORT_PARSE;
                  }
                ;

// ===========================================
// Some `subroutines' for function definitions
// ===========================================

push_fcn_symtab : // empty
                  {
                    current_function_depth++;

                    if (max_function_depth < current_function_depth)
                      max_function_depth = current_function_depth;

                    symtab_context.push (symbol_table::current_scope ());
                    symbol_table::set_scope (symbol_table::alloc_scope ());

                    if (! reading_script_file && current_function_depth == 1
                        && ! parsing_subfunctions)
                      primary_fcn_scope = symbol_table::current_scope ();

                    if (reading_script_file && current_function_depth > 1)
                      yyerror ("nested functions not implemented in this context");
                  }
                ;

// ===========================
// List of function parameters
// ===========================

param_list_beg  : '('
                  {
                    lexer_flags.looking_at_parameter_list = true;

                    if (lexer_flags.looking_at_function_handle)
                      {
                        symtab_context.push (symbol_table::current_scope ());
                        symbol_table::set_scope (symbol_table::alloc_scope ());
                        lexer_flags.looking_at_function_handle--;
                      }
                  }
                ;

param_list_end  : ')'
                  {
                    lexer_flags.looking_at_parameter_list = false;
                    lexer_flags.looking_for_object_index = false;
                  }
                ;

param_list      : param_list_beg param_list1 param_list_end
                  {
                    lexer_flags.quote_is_transpose = false;
                    $$ = $2;
                  }
                | param_list_beg error
                  {
                    yyerror ("invalid parameter list");
                    $$ = 0;
                    ABORT_PARSE;
                  }
                ;

param_list1     : // empty
                  { $$ = 0; }
                | param_list2
                  {
                    $1->mark_as_formal_parameters ();
                    if ($1->validate (tree_parameter_list::in))
                      $$ = $1;
                    else
                      ABORT_PARSE;
                  }
                ;

param_list2     : decl2
                  { $$ = new tree_parameter_list ($1); }
                | param_list2 ',' decl2
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                ;

// ===================================
// List of function return value names
// ===================================

return_list     : '[' ']'
                  {
                    lexer_flags.looking_at_return_list = false;
                    $$ = new tree_parameter_list ();
                  }
                | return_list1
                  {
                    lexer_flags.looking_at_return_list = false;
                    if ($1->validate (tree_parameter_list::out))
                      $$ = $1;
                    else
                      ABORT_PARSE;
                  }
                | '[' return_list1 ']'
                  {
                    lexer_flags.looking_at_return_list = false;
                    if ($2->validate (tree_parameter_list::out))
                      $$ = $2;
                    else
                      ABORT_PARSE;
                  }
                ;

return_list1    : identifier
                  { $$ = new tree_parameter_list (new tree_decl_elt ($1)); }
                | return_list1 ',' identifier
                  {
                    $1->append (new tree_decl_elt ($3));
                    $$ = $1;
                  }
                ;

// ===========
// Script file
// ===========

script_file     : SCRIPT_FILE opt_list END_OF_INPUT
                  {
                    tree_statement *end_of_script
                      = make_end ("endscript", input_line_number,
                                  current_input_column);

                    make_script ($2, end_of_script);

                    $$ = 0;
                  }
                ;

// =============
// Function file
// =============

function_file   : FUNCTION_FILE function_list opt_sep END_OF_INPUT
                  { $$ = 0; }
                ;

function_list   : function
                | function_list sep function
                ;

// ===================
// Function definition
// ===================

function_beg    : push_fcn_symtab FCN stash_comment
                  {
                    $$ = $3;

                    if (reading_classdef_file || lexer_flags.parsing_classdef) 
                      lexer_flags.maybe_classdef_get_set_method = true; 
                  }
                ;

function        : function_beg function1
                  {
                    $$ = finish_function (0, $2, $1);
                    recover_from_parsing_function ();
                  }
                | function_beg return_list '=' function1
                  {
                    $$ = finish_function ($2, $4, $1);
                    recover_from_parsing_function ();
                  }
                ;

fcn_name        : identifier
                  {
                    std::string id_name = $1->name ();

                    lexer_flags.parsed_function_name = true;
                    lexer_flags.defining_func = false;
                    lexer_flags.maybe_classdef_get_set_method = false;
            
                    $$ = $1;
                  }
                | GET '.' identifier
                  {
                    lexer_flags.maybe_classdef_get_set_method = false;
                    $$ = $3;
                  }
                | SET '.' identifier
                  {
                    lexer_flags.maybe_classdef_get_set_method = false;
                    $$ = $3;
                  }
                ;

function1       : fcn_name function2
                  {
                    std::string fname = $1->name ();

                    delete $1;

                    if (! ($$ = frob_function (fname, $2)))
                      ABORT_PARSE;
                  }
                ;

function2       : param_list opt_sep opt_list function_end
                  { $$ = start_function ($1, $3, $4); }
                | opt_sep opt_list function_end
                  { $$ = start_function (0, $2, $3); }
                ;

function_end    : END
                  {
                    endfunction_found = true;
                    if (end_token_ok ($1, token::function_end))
                      $$ = make_end ("endfunction", $1->line (), $1->column ());
                    else
                      ABORT_PARSE;
                  }
                | END_OF_INPUT
                  {
// A lot of tests are based on the assumption that this is OK
//                  if (reading_script_file)
//                    {
//                      yyerror ("function body open at end of script");
//                      YYABORT;
//                    }

                    if (endfunction_found)
                      {
                        yyerror ("inconsistent function endings -- "
                                 "if one function is explicitly ended, "
                                 "so must all the others");
                        YYABORT;
                      }

                    if (! (reading_fcn_file || reading_script_file
                           || get_input_from_eval_string))
                      {
                        yyerror ("function body open at end of input");
                        YYABORT;
                      }

                    if (reading_classdef_file)
                      {
                        yyerror ("classdef body open at end of input");
                        YYABORT;
                      }

                    $$ = make_end ("endfunction", input_line_number,
                                   current_input_column);
                  }
                ;

// ========
// Classdef
// ========

classdef_beg    : CLASSDEF stash_comment
                  {
                    $$ = 0;
                    lexer_flags.parsing_classdef = true;
                  }
                ;

classdef_end    : END
                  {
                    lexer_flags.parsing_classdef = false;

                    if (end_token_ok ($1, token::classdef_end))
                      $$ = make_end ("endclassdef", $1->line (), $1->column ());
                    else
                      ABORT_PARSE;
                  }
                ;

classdef1       : classdef_beg opt_attr_list identifier opt_superclasses
                  { $$ = 0; }
                ;

classdef        : classdef1 '\n' class_body '\n' stash_comment classdef_end
                  { $$ = 0; }
                ;

opt_attr_list   : // empty
                  { $$ = 0; }
                | '(' attr_list ')'
                  { $$ = 0; }
                ;

attr_list       : attr
                  { $$ = 0; }
                | attr_list ',' attr
                  { $$ = 0; }
                ;

attr            : identifier
                  { $$ = 0; }
                | identifier '=' decl_param_init expression
                  { $$ = 0; }
                | EXPR_NOT identifier
                  { $$ = 0; }
                ;

opt_superclasses
                : // empty
                  { $$ = 0; }
                | superclasses
                  { $$ = 0; }
                ;

superclasses    : EXPR_LT identifier '.' identifier
                  { $$ = 0; }
                | EXPR_LT identifier
                  { $$ = 0; }
                | superclasses EXPR_AND identifier '.' identifier
                  { $$ = 0; }
                | superclasses EXPR_AND identifier
                  { $$ = 0; }
                ;

class_body      : properties_block
                  { $$ = 0; }
                | methods_block
                  { $$ = 0; }
                | events_block
                  { $$ = 0; }
                | class_body '\n' properties_block
                  { $$ = 0; }
                | class_body '\n' methods_block
                  { $$ = 0; }
                | class_body '\n' events_block
                  { $$ = 0; }
                ;

properties_beg  : PROPERTIES stash_comment
                  { $$ = 0; }
                ;

properties_block
                : properties_beg opt_attr_list '\n' properties_list '\n' END
                  { $$ = 0; }
                ;

properties_list
                : class_property
                  { $$ = 0; }
                | properties_list '\n' class_property
                  { $$ = 0; }
                ;

class_property  : identifier
                  { $$ = 0; }
                | identifier '=' decl_param_init expression ';'
                  { $$ = 0; }
                ;

methods_beg     : METHODS stash_comment
                  { $$ = 0; }
                ;

methods_block   : methods_beg opt_attr_list '\n' methods_list '\n' END
                  { $$ = 0; }
                ;

methods_list    : function
                  { $$ = 0; }
                | methods_list '\n' function
                  { $$ = 0; }
                ;

events_beg      : EVENTS stash_comment
                  { $$ = 0; }
                ;

events_block    : events_beg opt_attr_list '\n' events_list '\n' END
                  { $$ = 0; }
                ;

events_list     : class_event
                  { $$ = 0; }
                | events_list '\n' class_event
                  { $$ = 0; }
                ;

class_event     : identifier
                  { $$ = 0; }
                ;
 
// =============
// Miscellaneous
// =============

stash_comment   : // empty
                  { $$ = octave_comment_buffer::get_comment (); }
                ;

parse_error     : LEXICAL_ERROR
                  { yyerror ("parse error"); }
                | error
                ;

sep_no_nl       : ','
                  { $$ = ','; }
                | ';'
                  { $$ = ';'; }
                | sep_no_nl ','
                  { $$ = $1; }
                | sep_no_nl ';'
                  { $$ = $1; }
                ;

opt_sep_no_nl   : // empty
                  { $$ = 0; }
                | sep_no_nl
                  { $$ = $1; }
                ;

sep             : ','
                  { $$ = ','; }
                | ';'
                  { $$ = ';'; }
                | '\n'
                  { $$ = '\n'; }
                | sep ','
                  { $$ = $1; }
                | sep ';'
                  { $$ = $1; }
                | sep '\n'
                  { $$ = $1; }
                ;

opt_sep         : // empty
                  { $$ = 0; }
                | sep
                  { $$ = $1; }
                ;

%%

// Generic error messages.

static void
yyerror (const char *s)
{
  int err_col = current_input_column - 1;

  std::ostringstream output_buf;

  if (reading_fcn_file || reading_script_file || reading_classdef_file)
    output_buf << "parse error near line " << input_line_number
               << " of file " << curr_fcn_file_full_name;
  else
    output_buf << "parse error:";

  if (s && strcmp (s, "parse error") != 0)
    output_buf << "\n\n  " << s;

  output_buf << "\n\n";

  if (! current_input_line.empty ())
    {
      size_t len = current_input_line.length ();

      if (current_input_line[len-1] == '\n')
        current_input_line.resize (len-1);

      // Print the line, maybe with a pointer near the error token.

      output_buf << ">>> " << current_input_line << "\n";

      if (err_col == 0)
        err_col = len;

      for (int i = 0; i < err_col + 3; i++)
        output_buf << " ";

      output_buf << "^";
    }

  output_buf << "\n";

  std::string msg = output_buf.str ();

  parse_error ("%s", msg.c_str ());
}

// Error mesages for mismatched end tokens.

static void
end_error (const char *type, token::end_tok_type ettype, int l, int c)
{
  static const char *fmt
    = "`%s' command matched by `%s' near line %d column %d";

  switch (ettype)
    {
    case token::simple_end:
      error (fmt, type, "end", l, c);
      break;

    case token::for_end:
      error (fmt, type, "endfor", l, c);
      break;

    case token::function_end:
      error (fmt, type, "endfunction", l, c);
      break;

    case token::classdef_end:
      error (fmt, type, "endclassdef", l, c);
      break;

    case token::if_end:
      error (fmt, type, "endif", l, c);
      break;

    case token::switch_end:
      error (fmt, type, "endswitch", l, c); 
      break;

    case token::while_end:
      error (fmt, type, "endwhile", l, c); 
      break;

    case token::try_catch_end:
      error (fmt, type, "end_try_catch", l, c); 
      break;

    case token::unwind_protect_end:
      error (fmt, type, "end_unwind_protect", l, c); 
      break;

    default:
      panic_impossible ();
      break;
    }
}

// Check to see that end tokens are properly matched.

static bool
end_token_ok (token *tok, token::end_tok_type expected)
{
  bool retval = true;

  token::end_tok_type ettype = tok->ettype ();

  if (ettype != expected && ettype != token::simple_end)
    {
      retval = false;

      yyerror ("parse error");

      int l = tok->line ();
      int c = tok->column ();

      switch (expected)
        {
        case token::classdef_end:
          end_error ("classdef", ettype, l, c);
          break;

        case token::for_end:
          end_error ("for", ettype, l, c);
          break;

        case token::function_end:
          end_error ("function", ettype, l, c);
          break;

        case token::if_end:
          end_error ("if", ettype, l, c);
          break;

        case token::try_catch_end:
          end_error ("try", ettype, l, c);
          break;

        case token::switch_end:
          end_error ("switch", ettype, l, c);
          break;

        case token::unwind_protect_end:
          end_error ("unwind_protect", ettype, l, c);
          break;

        case token::while_end:
          end_error ("while", ettype, l, c);
          break;

        default:
          panic_impossible ();
          break;
        }
    }

  return retval;
}

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.

static void
maybe_warn_assign_as_truth_value (tree_expression *expr)
{
  if (expr->is_assignment_expression ()
      && expr->paren_count () < 2)
    {
      if (curr_fcn_file_full_name.empty ())
        warning_with_id
          ("Octave:assign-as-truth-value",
           "suggest parenthesis around assignment used as truth value");
      else
        warning_with_id
          ("Octave:assign-as-truth-value",
           "suggest parenthesis around assignment used as truth value near line %d, column %d in file `%s'",
           expr->line (), expr->column (), curr_fcn_file_full_name.c_str ());
    }
}

// Maybe print a warning about switch labels that aren't constants.

static void
maybe_warn_variable_switch_label (tree_expression *expr)
{
  if (! expr->is_constant ())
    {
      if (curr_fcn_file_full_name.empty ())
        warning_with_id ("Octave:variable-switch-label",
                         "variable switch label");
      else
        warning_with_id
          ("Octave:variable-switch-label",
           "variable switch label near line %d, column %d in file `%s'",
           expr->line (), expr->column (), curr_fcn_file_full_name.c_str ());
    }
}

static tree_expression *
fold (tree_binary_expression *e)
{
  tree_expression *retval = e;

  unwind_protect frame;

  frame.protect_var (error_state);
  frame.protect_var (warning_state);

  frame.protect_var (discard_error_messages);
  frame.protect_var (discard_warning_messages);

  discard_error_messages = true;
  discard_warning_messages = true;

  tree_expression *op1 = e->lhs ();
  tree_expression *op2 = e->rhs ();

  octave_value::binary_op op_type = e->op_type ();

  if (op1->is_constant () && op2->is_constant ()
      && (! ((warning_enabled ("Octave:associativity-change")
              && (op_type == POW || op_type == EPOW))
             || (warning_enabled ("Octave:precedence-change")
                 && (op_type == EXPR_OR || op_type == EXPR_OR_OR)))))
    {
      octave_value tmp = e->rvalue1 ();

      if (! (error_state || warning_state))
        {
          tree_constant *tc_retval
            = new tree_constant (tmp, op1->line (), op1->column ());

          std::ostringstream buf;

          tree_print_code tpc (buf);

          e->accept (tpc);

          tc_retval->stash_original_text (buf.str ());

          delete e;

          retval = tc_retval;
        }
    }

  return retval;
}

static tree_expression *
fold (tree_unary_expression *e)
{
  tree_expression *retval = e;

  unwind_protect frame;

  frame.protect_var (error_state);
  frame.protect_var (warning_state);

  frame.protect_var (discard_error_messages);
  frame.protect_var (discard_warning_messages);

  discard_error_messages = true;
  discard_warning_messages = true;

  tree_expression *op = e->operand ();

  if (op->is_constant ())
    {
      octave_value tmp = e->rvalue1 ();

      if (! (error_state || warning_state))
        {
          tree_constant *tc_retval
            = new tree_constant (tmp, op->line (), op->column ());

          std::ostringstream buf;

          tree_print_code tpc (buf);

          e->accept (tpc);

          tc_retval->stash_original_text (buf.str ());

          delete e;

          retval = tc_retval;
        }
    }

  return retval;
}

// Finish building a range.

static tree_expression *
finish_colon_expression (tree_colon_expression *e)
{
  tree_expression *retval = e;

  unwind_protect frame;

  frame.protect_var (error_state);
  frame.protect_var (warning_state);

  frame.protect_var (discard_error_messages);
  frame.protect_var (discard_warning_messages);

  discard_error_messages = true;
  discard_warning_messages = true;

  tree_expression *base = e->base ();
  tree_expression *limit = e->limit ();
  tree_expression *incr = e->increment ();

  if (base)
    {
      if (limit)
        {
          if (base->is_constant () && limit->is_constant ()
              && (! incr || (incr && incr->is_constant ())))
            {
              octave_value tmp = e->rvalue1 ();

              if (! (error_state || warning_state))
                {
                  tree_constant *tc_retval
                    = new tree_constant (tmp, base->line (), base->column ());

                  std::ostringstream buf;

                  tree_print_code tpc (buf);

                  e->accept (tpc);

                  tc_retval->stash_original_text (buf.str ());

                  delete e;

                  retval = tc_retval;
                }
            }
        }
      else
        {
          e->preserve_base ();
          delete e;

          // FIXME -- need to attempt constant folding here
          // too (we need a generic way to do that).
          retval = base;
        }
    }

  return retval;
}

// Make a constant.

static tree_constant *
make_constant (int op, token *tok_val)
{
  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_constant *retval = 0;

  switch (op)
    {
    case NUM:
      {
        octave_value tmp (tok_val->number ());
        retval = new tree_constant (tmp, l, c);
        retval->stash_original_text (tok_val->text_rep ());
      }
      break;

    case IMAG_NUM:
      {
        octave_value tmp (Complex (0.0, tok_val->number ()));
        retval = new tree_constant (tmp, l, c);
        retval->stash_original_text (tok_val->text_rep ());
      }
      break;

    case DQ_STRING:
    case SQ_STRING:
      {
        std::string txt = tok_val->text ();

        char delim = op == DQ_STRING ? '"' : '\'';
        octave_value tmp (txt, delim);

        if (txt.empty ())
          {
            if (op == DQ_STRING)
              tmp = octave_null_str::instance;
            else
              tmp = octave_null_sq_str::instance;
          }

        retval = new tree_constant (tmp, l, c);

        if (op == DQ_STRING)
          txt = undo_string_escapes (txt);

        // FIXME -- maybe this should also be handled by
        // tok_val->text_rep () for character strings?
        retval->stash_original_text (delim + txt + delim);
      }
      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
}

// Make a function handle.

static tree_fcn_handle *
make_fcn_handle (token *tok_val)
{
  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_fcn_handle *retval = new tree_fcn_handle (tok_val->text (), l, c);

  return retval;
}

// Make an anonymous function handle.

static tree_anon_fcn_handle *
make_anon_fcn_handle (tree_parameter_list *param_list, tree_statement *stmt)
{
  // FIXME -- need to get these from the location of the @ symbol.

  int l = -1;
  int c = -1;

  tree_parameter_list *ret_list = 0;

  symbol_table::scope_id fcn_scope = symbol_table::current_scope ();

  if (symtab_context.empty ())
    panic_impossible ();

  symbol_table::set_scope (symtab_context.top ());

  symtab_context.pop ();

  stmt->set_print_flag (false);

  tree_statement_list *body = new tree_statement_list (stmt);

  body->mark_as_anon_function_body ();

  tree_anon_fcn_handle *retval
    = new tree_anon_fcn_handle (param_list, ret_list, body, fcn_scope, l, c);

  return retval;
}

static void
maybe_warn_associativity_change (tree_expression *op)
{
  if (op->paren_count () == 0 && op->is_binary_expression ())
    {
      tree_binary_expression *e
        = dynamic_cast<tree_binary_expression *> (op);

      octave_value::binary_op op_type = e->op_type ();

      if (op_type == octave_value::op_pow
          || op_type == octave_value::op_el_pow)
        {
          std::string op_str = octave_value::binary_op_as_string (op_type);

          if (curr_fcn_file_full_name.empty ())
            warning_with_id
              ("Octave:associativity-change",
               "meaning may have changed due to change in associativity for %s operator",
               op_str.c_str ());
          else
            warning_with_id
              ("Octave:associativity-change",
               "meaning may have changed due to change in associativity for %s operator near line %d, column %d in file `%s'",
               op_str.c_str (), op->line (), op->column (),
               curr_fcn_file_full_name.c_str ());
        }
    }
}

// Build a binary expression.

static tree_expression *
make_binary_op (int op, tree_expression *op1, token *tok_val,
                tree_expression *op2)
{
  octave_value::binary_op t = octave_value::unknown_binary_op;

  switch (op)
    {
    case POW:
      t = octave_value::op_pow;
      maybe_warn_associativity_change (op1);
      break;

    case EPOW:
      t = octave_value::op_el_pow;
      maybe_warn_associativity_change (op1);
      break;

    case '+':
      t = octave_value::op_add;
      break;

    case '-':
      t = octave_value::op_sub;
      break;

    case '*':
      t = octave_value::op_mul;
      break;

    case '/':
      t = octave_value::op_div;
      break;

    case EMUL:
      t = octave_value::op_el_mul;
      break;

    case EDIV:
      t = octave_value::op_el_div;
      break;

    case LEFTDIV:
      t = octave_value::op_ldiv;
      break;

    case ELEFTDIV:
      t = octave_value::op_el_ldiv;
      break;

    case LSHIFT:
      t = octave_value::op_lshift;
      break;

    case RSHIFT:
      t = octave_value::op_rshift;
      break;

    case EXPR_LT:
      t = octave_value::op_lt;
      break;

    case EXPR_LE:
      t = octave_value::op_le;
      break;

    case EXPR_EQ:
      t = octave_value::op_eq;
      break;

    case EXPR_GE:
      t = octave_value::op_ge;
      break;

    case EXPR_GT:
      t = octave_value::op_gt;
      break;

    case EXPR_NE:
      t = octave_value::op_ne;
      break;

    case EXPR_AND:
      t = octave_value::op_el_and;
      break;

    case EXPR_OR:
      t = octave_value::op_el_or;
      if (op2->paren_count () == 0 && op2->is_binary_expression ())
        {
          tree_binary_expression *e
            = dynamic_cast<tree_binary_expression *> (op2);

          if (e->op_type () == octave_value::op_el_and)
            {
              if (curr_fcn_file_full_name.empty ())
                warning_with_id
                  ("Octave:precedence-change",
                   "meaning may have changed due to change in precedence for & and | operators");
              else
                warning_with_id
                  ("Octave:precedence-change",
                   "meaning may have changed due to change in precedence for & and | operators near line %d, column %d in file `%s'",
                   op2->line (), op2->column (),
                   curr_fcn_file_full_name.c_str ());
            }
        }
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_binary_expression *e
    = maybe_compound_binary_expression (op1, op2, l, c, t);

  return fold (e);
}

// Build a boolean expression.

static tree_expression *
make_boolean_op (int op, tree_expression *op1, token *tok_val,
                 tree_expression *op2)
{
  tree_boolean_expression::type t;

  switch (op)
    {
    case EXPR_AND_AND:
      t = tree_boolean_expression::bool_and;
      break;

    case EXPR_OR_OR:
      t = tree_boolean_expression::bool_or;
      if (op2->paren_count () == 0 && op2->is_boolean_expression ())
        {
          tree_boolean_expression *e
            = dynamic_cast<tree_boolean_expression *> (op2);

          if (e->op_type () == tree_boolean_expression::bool_and)
            warning_with_id
              ("Octave:precedence-change",
               "meaning may have changed due to change in precedence for && and || operators");
        }
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_boolean_expression *e
    = new tree_boolean_expression (op1, op2, l, c, t);

  return fold (e);
}

// Build a prefix expression.

static tree_expression *
make_prefix_op (int op, tree_expression *op1, token *tok_val)
{
  octave_value::unary_op t = octave_value::unknown_unary_op;

  switch (op)
    {
    case EXPR_NOT:
      t = octave_value::op_not;
      break;

    case '+':
      t = octave_value::op_uplus;
      break;

    case '-':
      t = octave_value::op_uminus;
      break;

    case PLUS_PLUS:
      t = octave_value::op_incr;
      break;

    case MINUS_MINUS:
      t = octave_value::op_decr;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_prefix_expression *e
    = new tree_prefix_expression (op1, l, c, t);

  return fold (e);
}

// Build a postfix expression.

static tree_expression *
make_postfix_op (int op, tree_expression *op1, token *tok_val)
{
  octave_value::unary_op t = octave_value::unknown_unary_op;

  switch (op)
    {
    case QUOTE:
      t = octave_value::op_hermitian;
      break;

    case TRANSPOSE:
      t = octave_value::op_transpose;
      break;

    case PLUS_PLUS:
      t = octave_value::op_incr;
      break;

    case MINUS_MINUS:
      t = octave_value::op_decr;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_postfix_expression *e
    = new tree_postfix_expression (op1, l, c, t);

  return fold (e);
}

// Build an unwind-protect command.

static tree_command *
make_unwind_command (token *unwind_tok, tree_statement_list *body,
                     tree_statement_list *cleanup, token *end_tok,
                     octave_comment_list *lc, octave_comment_list *mc)
{
  tree_command *retval = 0;

  if (end_token_ok (end_tok, token::unwind_protect_end))
    {
      octave_comment_list *tc = octave_comment_buffer::get_comment ();

      int l = unwind_tok->line ();
      int c = unwind_tok->column ();

      retval = new tree_unwind_protect_command (body, cleanup,
                                                lc, mc, tc, l, c);
    }

  return retval;
}

// Build a try-catch command.

static tree_command *
make_try_command (token *try_tok, tree_statement_list *body,
                  tree_statement_list *cleanup, token *end_tok,
                  octave_comment_list *lc, octave_comment_list *mc)
{
  tree_command *retval = 0;

  if (end_token_ok (end_tok, token::try_catch_end))
    {
      octave_comment_list *tc = octave_comment_buffer::get_comment ();

      int l = try_tok->line ();
      int c = try_tok->column ();

      retval = new tree_try_catch_command (body, cleanup,
                                           lc, mc, tc, l, c);
    }

  return retval;
}

// Build a while command.

static tree_command *
make_while_command (token *while_tok, tree_expression *expr,
                    tree_statement_list *body, token *end_tok,
                    octave_comment_list *lc)
{
  tree_command *retval = 0;

  maybe_warn_assign_as_truth_value (expr);

  if (end_token_ok (end_tok, token::while_end))
    {
      octave_comment_list *tc = octave_comment_buffer::get_comment ();

      lexer_flags.looping--;

      int l = while_tok->line ();
      int c = while_tok->column ();

      retval = new tree_while_command (expr, body, lc, tc, l, c);
    }

  return retval;
}

// Build a do-until command.

static tree_command *
make_do_until_command (token *until_tok, tree_statement_list *body,
                       tree_expression *expr, octave_comment_list *lc)
{
  tree_command *retval = 0;

  maybe_warn_assign_as_truth_value (expr);

  octave_comment_list *tc = octave_comment_buffer::get_comment ();

  lexer_flags.looping--;

  int l = until_tok->line ();
  int c = until_tok->column ();

  retval = new tree_do_until_command (expr, body, lc, tc, l, c);

  return retval;
}

// Build a for command.

static tree_command *
make_for_command (token *for_tok, tree_argument_list *lhs,
                  tree_expression *expr, tree_statement_list *body,
                  token *end_tok, octave_comment_list *lc)
{
  tree_command *retval = 0;

  if (end_token_ok (end_tok, token::for_end))
    {
      octave_comment_list *tc = octave_comment_buffer::get_comment ();

      lexer_flags.looping--;

      int l = for_tok->line ();
      int c = for_tok->column ();

      if (lhs->length () == 1)
        {
          tree_expression *tmp = lhs->remove_front ();

          retval = new tree_simple_for_command (tmp, expr, body,
                                                lc, tc, l, c);

          delete lhs;
        }
      else
        retval = new tree_complex_for_command (lhs, expr, body,
                                               lc, tc, l, c);
    }

  return retval;
}

// Build a break command.

static tree_command *
make_break_command (token *break_tok)
{
  tree_command *retval = 0;

  int l = break_tok->line ();
  int c = break_tok->column ();

  retval = new tree_break_command (l, c);

  return retval;
}

// Build a continue command.

static tree_command *
make_continue_command (token *continue_tok)
{
  tree_command *retval = 0;

  int l = continue_tok->line ();
  int c = continue_tok->column ();

  retval = new tree_continue_command (l, c);

  return retval;
}

// Build a return command.

static tree_command *
make_return_command (token *return_tok)
{
  tree_command *retval = 0;

  int l = return_tok->line ();
  int c = return_tok->column ();

  retval = new tree_return_command (l, c);

  return retval;
}

// Start an if command.

static tree_if_command_list *
start_if_command (tree_expression *expr, tree_statement_list *list)
{
  maybe_warn_assign_as_truth_value (expr);

  tree_if_clause *t = new tree_if_clause (expr, list);

  return new tree_if_command_list (t);
}

// Finish an if command.

static tree_if_command *
finish_if_command (token *if_tok, tree_if_command_list *list,
                   token *end_tok, octave_comment_list *lc)
{
  tree_if_command *retval = 0;

  if (end_token_ok (end_tok, token::if_end))
    {
      octave_comment_list *tc = octave_comment_buffer::get_comment ();

      int l = if_tok->line ();
      int c = if_tok->column ();

      if (list && ! list->empty ())
        {
          tree_if_clause *elt = list->front ();

          if (elt)
            {
              elt->line (l);
              elt->column (c);
            }
        }

      retval = new tree_if_command (list, lc, tc, l, c);
    }

  return retval;
}

// Build an elseif clause.

static tree_if_clause *
make_elseif_clause (token *elseif_tok, tree_expression *expr,
                    tree_statement_list *list, octave_comment_list *lc)
{
  maybe_warn_assign_as_truth_value (expr);

  int l = elseif_tok->line ();
  int c = elseif_tok->column ();

  return new tree_if_clause (expr, list, lc, l, c);
}

// Finish a switch command.

static tree_switch_command *
finish_switch_command (token *switch_tok, tree_expression *expr,
                       tree_switch_case_list *list, token *end_tok,
                       octave_comment_list *lc)
{
  tree_switch_command *retval = 0;

  if (end_token_ok (end_tok, token::switch_end))
    {
      octave_comment_list *tc = octave_comment_buffer::get_comment ();

      int l = switch_tok->line ();
      int c = switch_tok->column ();

      if (list && ! list->empty ())
        {
          tree_switch_case *elt = list->front ();

          if (elt)
            {
              elt->line (l);
              elt->column (c);
            }
        }

      retval = new tree_switch_command (expr, list, lc, tc, l, c);
    }

  return retval;
}

// Build a switch case.

static tree_switch_case *
make_switch_case (token *case_tok, tree_expression *expr,
                  tree_statement_list *list, octave_comment_list *lc)
{
  maybe_warn_variable_switch_label (expr);

  int l = case_tok->line ();
  int c = case_tok->column ();

  return new tree_switch_case (expr, list, lc, l, c);
}

// Build an assignment to a variable.

static tree_expression *
make_assign_op (int op, tree_argument_list *lhs, token *eq_tok,
                tree_expression *rhs)
{
  tree_expression *retval = 0;

  octave_value::assign_op t = octave_value::unknown_assign_op;

  switch (op)
    {
    case '=':
      t = octave_value::op_asn_eq;
      break;

    case ADD_EQ:
      t = octave_value::op_add_eq;
      break;

    case SUB_EQ:
      t = octave_value::op_sub_eq;
      break;

    case MUL_EQ:
      t = octave_value::op_mul_eq;
      break;

    case DIV_EQ:
      t = octave_value::op_div_eq;
      break;

    case LEFTDIV_EQ:
      t = octave_value::op_ldiv_eq;
      break;

    case POW_EQ:
      t = octave_value::op_pow_eq;
      break;

    case LSHIFT_EQ:
      t = octave_value::op_lshift_eq;
      break;

    case RSHIFT_EQ:
      t = octave_value::op_rshift_eq;
      break;

    case EMUL_EQ:
      t = octave_value::op_el_mul_eq;
      break;

    case EDIV_EQ:
      t = octave_value::op_el_div_eq;
      break;

    case ELEFTDIV_EQ:
      t = octave_value::op_el_ldiv_eq;
      break;

    case EPOW_EQ:
      t = octave_value::op_el_pow_eq;
      break;

    case AND_EQ:
      t = octave_value::op_el_and_eq;
      break;

    case OR_EQ:
      t = octave_value::op_el_or_eq;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = eq_tok->line ();
  int c = eq_tok->column ();

  if (lhs->is_simple_assign_lhs ())
    {
      tree_expression *tmp = lhs->remove_front ();

      retval = new tree_simple_assignment (tmp, rhs, false, l, c, t);

      delete lhs;
    }
  else if (t == octave_value::op_asn_eq)
    return new tree_multi_assignment (lhs, rhs, false, l, c);
  else
    yyerror ("computed multiple assignment not allowed");

  return retval;
}

// Define a script.

static void
make_script (tree_statement_list *cmds, tree_statement *end_script)
{
  std::string doc_string;

  if (! help_buf.empty ())
    {
      doc_string = help_buf.top ();
      help_buf.pop ();
    }

  if (! cmds)
    cmds = new tree_statement_list ();

  cmds->append (end_script);

  octave_user_script *script
    = new octave_user_script (curr_fcn_file_full_name, curr_fcn_file_name,
                              cmds, doc_string);

  octave_time now;

  script->stash_fcn_file_time (now);

  primary_fcn_ptr = script;

  // Unmark any symbols that may have been tagged as local variables
  // while parsing (for example, by force_local_variable in lex.l).

  symbol_table::unmark_forced_variables ();
}

// Begin defining a function.

static octave_user_function *
start_function (tree_parameter_list *param_list, tree_statement_list *body,
                tree_statement *end_fcn_stmt)
{
  // We'll fill in the return list later.

  if (! body)
    body = new tree_statement_list ();

  body->append (end_fcn_stmt);

  octave_user_function *fcn
    = new octave_user_function (symbol_table::current_scope (),
                                param_list, 0, body);

  if (fcn)
    {
      octave_comment_list *tc = octave_comment_buffer::get_comment ();

      fcn->stash_trailing_comment (tc);
    }

  return fcn;
}

static tree_statement *
make_end (const std::string& type, int l, int c)
{
  return make_statement (new tree_no_op_command (type, l, c));
}

// Do most of the work for defining a function.

static octave_user_function *
frob_function (const std::string& fname, octave_user_function *fcn)
{
  std::string id_name = fname;

  // If input is coming from a file, issue a warning if the name of
  // the file does not match the name of the function stated in the
  // file.  Matlab doesn't provide a diagnostic (it ignores the stated
  // name).
  if (! autoloading && reading_fcn_file
      && (current_function_depth == 1
          && ! (parsing_subfunctions || lexer_flags.parsing_class_method)))
  {
    // FIXME -- should curr_fcn_file_name already be
    // preprocessed when we get here?  It seems to only be a
    // problem with relative file names.

    std::string nm = curr_fcn_file_name;

    size_t pos = nm.find_last_of (file_ops::dir_sep_chars ());

    if (pos != std::string::npos)
      nm = curr_fcn_file_name.substr (pos+1);

    if (nm != id_name)
      {
        warning_with_id
          ("Octave:function-name-clash",
           "function name `%s' does not agree with function file name `%s'",
           id_name.c_str (), curr_fcn_file_full_name.c_str ());

        id_name = nm;
      }
  }

  if (reading_fcn_file || reading_classdef_file || autoloading)
    {
      octave_time now;

      fcn->stash_fcn_file_name (curr_fcn_file_full_name);
      fcn->stash_fcn_file_time (now);
      fcn->mark_as_system_fcn_file ();

      if (fcn_file_from_relative_lookup)
        fcn->mark_relative ();

      if (current_function_depth > 1 || parsing_subfunctions)
        {
          fcn->stash_parent_fcn_name (curr_fcn_file_name);
          fcn->stash_parent_fcn_scope (primary_fcn_scope);
        }

      if (lexer_flags.parsing_class_method)
        {
          if (current_class_name == id_name)
            fcn->mark_as_class_constructor ();
          else
            fcn->mark_as_class_method ();

          fcn->stash_dispatch_class (current_class_name);
        }

      std::string nm = fcn->fcn_file_name ();

      file_stat fs (nm);

      if (fs && fs.is_newer (now))
        warning_with_id ("Octave:future-time-stamp",
                         "time stamp for `%s' is in the future", nm.c_str ());
    }
  else if (! (input_from_tmp_history_file || input_from_startup_file)
           && reading_script_file
           && curr_fcn_file_name == id_name)
    {
      warning ("function `%s' defined within script file `%s'",
               id_name.c_str (), curr_fcn_file_full_name.c_str ());
    }

  fcn->stash_function_name (id_name);

  if (! help_buf.empty () && current_function_depth == 1
      && ! parsing_subfunctions)
    {
      fcn->document (help_buf.top ());

      help_buf.pop ();
    }

  if (reading_fcn_file && current_function_depth == 1
      && ! parsing_subfunctions)
    primary_fcn_ptr = fcn;
  
  return fcn;
}

static tree_function_def *
finish_function (tree_parameter_list *ret_list,
                 octave_user_function *fcn, octave_comment_list *lc)
{
  tree_function_def *retval = 0;

  if (ret_list)
    ret_list->mark_as_formal_parameters ();

  if (fcn)
    {
      std::string nm = fcn->name ();
      std::string file = fcn->fcn_file_name ();

      std::string tmp = nm;
      if (! file.empty ())
        tmp += ": " + file;

      symbol_table::cache_name (fcn->scope (), tmp);

      if (lc)
        fcn->stash_leading_comment (lc);

      fcn->define_ret_list (ret_list);

      if (current_function_depth > 1 || parsing_subfunctions)
        {
          // FIXME -- is this flag used to determine if the function is a
          // _subfunction_ somewhere?
          fcn->mark_as_nested_function ();

          symbol_table::install_subfunction (nm, octave_value (fcn),
                                             primary_fcn_scope);
        }

      if (! reading_fcn_file)
        {
          // We are either reading a script file or defining a function
          // at the command line, so this definition creates a
          // tree_function object that is placed in the parse tree.
          // Otherwise, it is just inserted in the symbol table,
          // either as a subfunction (see above), or as the primary
          // function for the file, via primary_fcn_ptr (see also
          // load_fcn_from_file,, parse_fcn_file, and
          // symbol_table::fcn_info::fcn_info_rep::find_user_function).

          retval = new tree_function_def (fcn);
        }

      // Unmark any symbols that may have been tagged as local
      // variables while parsing (for example, by force_local_variable
      // in lex.l).

      symbol_table::unmark_forced_variables (fcn->scope ());
    }

  return retval;
}

static void
recover_from_parsing_function (void)
{
  if (symtab_context.empty ())
    panic_impossible ();

  symbol_table::set_scope (symtab_context.top ());
  symtab_context.pop ();

  if (reading_fcn_file && current_function_depth == 1
      && ! parsing_subfunctions)
    parsing_subfunctions = true;

  current_function_depth--;

  lexer_flags.parsed_function_name = false;
  lexer_flags.looking_at_return_list = false;
  lexer_flags.looking_at_parameter_list = false;
}

// Make an index expression.

static tree_index_expression *
make_index_expression (tree_expression *expr, tree_argument_list *args,
                       char type)
{
  tree_index_expression *retval = 0;
  
  if (args && args->has_magic_tilde ())
    {
      yyerror ("invalid use of empty argument (~) in index expression");
      return retval;
    }

  int l = expr->line ();
  int c = expr->column ();

  expr->mark_postfix_indexed ();

  if (expr->is_index_expression ())
    {
      tree_index_expression *tmp = static_cast<tree_index_expression *> (expr);

      tmp->append (args, type);

      retval = tmp;
    }
  else
    retval = new tree_index_expression (expr, args, l, c, type);

  return retval;
}

// Make an indirect reference expression.

static tree_index_expression *
make_indirect_ref (tree_expression *expr, const std::string& elt)
{
  tree_index_expression *retval = 0;

  int l = expr->line ();
  int c = expr->column ();

  if (expr->is_index_expression ())
    {
      tree_index_expression *tmp = static_cast<tree_index_expression *> (expr);

      tmp->append (elt);

      retval = tmp;
    }
  else
    retval = new tree_index_expression (expr, elt, l, c);

  lexer_flags.looking_at_indirect_ref = false;

  return retval;
}

// Make an indirect reference expression with dynamic field name.

static tree_index_expression *
make_indirect_ref (tree_expression *expr, tree_expression *elt)
{
  tree_index_expression *retval = 0;

  int l = expr->line ();
  int c = expr->column ();

  if (expr->is_index_expression ())
    {
      tree_index_expression *tmp = static_cast<tree_index_expression *> (expr);

      tmp->append (elt);

      retval = tmp;
    }
  else
    retval = new tree_index_expression (expr, elt, l, c);

  lexer_flags.looking_at_indirect_ref = false;

  return retval;
}

// Make a declaration command.

static tree_decl_command *
make_decl_command (int tok, token *tok_val, tree_decl_init_list *lst)
{
  tree_decl_command *retval = 0;

  int l = tok_val->line ();
  int c = tok_val->column ();

  switch (tok)
    {
    case GLOBAL:
      retval = new tree_global_command (lst, l, c);
      break;

    case STATIC:
      if (current_function_depth > 0)
        retval = new tree_static_command (lst, l, c);
      else
        {
          if (reading_script_file)
            warning ("ignoring persistent declaration near line %d of file `%s'",
                     l, curr_fcn_file_full_name.c_str ());
          else
            warning ("ignoring persistent declaration near line %d", l);
        }
      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
}

static tree_argument_list *
validate_matrix_row (tree_argument_list *row)
{
  if (row && row->has_magic_tilde ())
    yyerror ("invalid use of tilde (~) in matrix expression");
  return row;
}

// Finish building a matrix list.

static tree_expression *
finish_matrix (tree_matrix *m)
{
  tree_expression *retval = m;

  unwind_protect frame;

  frame.protect_var (error_state);
  frame.protect_var (warning_state);

  frame.protect_var (discard_error_messages);
  frame.protect_var (discard_warning_messages);

  discard_error_messages = true;
  discard_warning_messages = true;

  if (m->all_elements_are_constant ())
    {
      octave_value tmp = m->rvalue1 ();

      if (! (error_state || warning_state))
        {
          tree_constant *tc_retval
            = new tree_constant (tmp, m->line (), m->column ());

          std::ostringstream buf;

          tree_print_code tpc (buf);

          m->accept (tpc);

          tc_retval->stash_original_text (buf.str ());

          delete m;

          retval = tc_retval;
        }
    }

  return retval;
}

// Finish building a cell list.

static tree_expression *
finish_cell (tree_cell *c)
{
  return finish_matrix (c);
}

static void
maybe_warn_missing_semi (tree_statement_list *t)
{
  if (current_function_depth > 0)
    {
      tree_statement *tmp = t->back();

      if (tmp->is_expression ())
        warning_with_id
          ("Octave:missing-semicolon",
           "missing semicolon near line %d, column %d in file `%s'",
            tmp->line (), tmp->column (), curr_fcn_file_full_name.c_str ());
    }
}

static tree_statement_list *
set_stmt_print_flag (tree_statement_list *list, char sep,
                     bool warn_missing_semi)
{
  tree_statement *tmp = list->back ();

  switch (sep)
    {
    case ';':
      tmp->set_print_flag (false);
      break;

    case 0:
    case ',':
    case '\n':
      tmp->set_print_flag (true);
      if (warn_missing_semi)
        maybe_warn_missing_semi (list);
      break;

    default:
      warning ("unrecognized separator type!");
      break;
    }

  // Even if a statement is null, we add it to the list then remove it
  // here so that the print flag is applied to the correct statement.

  if (tmp->is_null_statement ())
    {
      list->pop_back ();
      delete tmp;
    }

  return list;
}

static tree_statement_list *
make_statement_list (tree_statement *stmt)
{
  return new tree_statement_list (stmt);
}

static tree_statement_list *
append_statement_list (tree_statement_list *list, char sep,
                       tree_statement *stmt, bool warn_missing_semi)
{
  set_stmt_print_flag (list, sep, warn_missing_semi);

  list->append (stmt);

  return list;
}

static void
safe_fclose (FILE *f)
{
  // FIXME -- comments at the end of an input file are
  // discarded (otherwise, they would be appended to the next
  // statement, possibly from the command line or another file, which
  // can be quite confusing).

  octave_comment_list *tc = octave_comment_buffer::get_comment ();

  delete tc;

  if (f)
    fclose (static_cast<FILE *> (f));
}

static bool
looks_like_copyright (const std::string& s)
{
  bool retval = false;

  if (! s.empty ())
    {
      size_t offset = s.find_first_not_of (" \t");
  
      retval = (s.substr (offset, 9) == "Copyright");
    }

  return retval;
}

static int
text_getc (FILE *f)
{
  int c = getc (f);

  // Convert CRLF into just LF and single CR into LF.

  if (c == '\r')
    {
      c = getc (f);

      if (c != '\n')
        {
          ungetc (c, f);
          c = '\n';
        }
    }

  if (c == '\n')
    input_line_number++;

  return c;
}

class
stdio_stream_reader : public stream_reader
{
public:
  stdio_stream_reader (FILE *f_arg) : stream_reader (), f (f_arg) { }

  int getc (void) { return ::text_getc (f); }
  int ungetc (int c)
  {
    if (c == '\n')
      input_line_number--;

    return ::ungetc (c, f);
  }
  
private:
  FILE *f;
};

static bool
skip_white_space (stream_reader& reader)
{
  int c = 0;

  while ((c = reader.getc ()) != EOF)
    {
      switch (c)
        {
        case ' ':
        case '\t':
          current_input_column++;
          break;

        case '\n':
          current_input_column = 0;
          break;

        default:
          current_input_column--;
          reader.ungetc (c);
          goto done;
        }
    }

 done:

  return (c == EOF);
}

static bool
looking_at_classdef_keyword (FILE *ffile)
{
  bool status = false;

  long pos = ftell (ffile);

  char buf [10];
  fgets (buf, 10, ffile);
  size_t len = strlen (buf);
  if (len > 8 && strncmp (buf, "classdef", 8) == 0
      && ! (isalnum (buf[8]) || buf[8] == '_'))
    status = true;

  fseek (ffile, pos, SEEK_SET);

  return status;
 }

static std::string
gobble_leading_white_space (FILE *ffile, bool& eof)
{
  std::string help_txt;

  eof = false;

  // TRUE means we have already cached the help text.
  bool have_help_text = false;

  std::string txt;

  stdio_stream_reader stdio_reader (ffile);

  while (true)
    {
      eof = skip_white_space (stdio_reader);

      if (eof)
        break;

      txt = grab_comment_block (stdio_reader, true, eof);

      if (txt.empty ())
        break;

      if (! (have_help_text || looks_like_copyright (txt)))
        {
          help_txt = txt;
          have_help_text = true;
        }

      octave_comment_buffer::append (txt);

      if (eof)
        break;
    }

  return help_txt;
}

static bool
looking_at_function_keyword (FILE *ffile)
{
  bool status = false;

  long pos = ftell (ffile);

  char buf [10];
  fgets (buf, 10, ffile);
  size_t len = strlen (buf);
  if (len > 8 && strncmp (buf, "function", 8) == 0
      && ! (isalnum (buf[8]) || buf[8] == '_'))
    status = true;

  fseek (ffile, pos, SEEK_SET);

  return status;
}

static octave_function *
parse_fcn_file (const std::string& ff, const std::string& dispatch_type,
                bool force_script = false, bool require_file = true,
                const std::string& warn_for = std::string ())
{
  unwind_protect frame;

  octave_function *fcn_ptr = 0;

  // Open function file and parse.

  FILE *in_stream = command_editor::get_input_stream ();

  frame.add_fcn (command_editor::set_input_stream, in_stream);

  frame.protect_var (ff_instream);

  frame.protect_var (input_line_number);
  frame.protect_var (current_input_column);
  frame.protect_var (reading_fcn_file);
  frame.protect_var (line_editing);
  frame.protect_var (current_class_name);
  frame.protect_var (current_function_depth);
  frame.protect_var (max_function_depth);
  frame.protect_var (parsing_subfunctions);
  frame.protect_var (endfunction_found);

  input_line_number = 1;
  current_input_column = 1;
  reading_fcn_file = true;
  line_editing = false;
  current_class_name = dispatch_type;
  current_function_depth = 0;
  max_function_depth = 0;
  parsing_subfunctions = false;
  endfunction_found = false;

  // The next four lines must be in this order.
  frame.add_fcn (command_history::ignore_entries, ! Vsaving_history);

  // FIXME -- we shouldn't need both the
  // command_history object and the
  // Vsaving_history variable...
  command_history::ignore_entries ();

  frame.protect_var (Vsaving_history);

  Vsaving_history = false;

  FILE *ffile = get_input_from_file (ff, 0);

  frame.add_fcn (safe_fclose, ffile);

  if (ffile)
    {
      bool eof;

      std::string help_txt = gobble_leading_white_space (ffile, eof);

      if (! eof)
        {
          std::string file_type;

          frame.protect_var (get_input_from_eval_string);
          frame.protect_var (parser_end_of_input);
          frame.protect_var (reading_fcn_file);
          frame.protect_var (reading_script_file);
          frame.protect_var (reading_classdef_file);
          frame.protect_var (Vecho_executing_commands);


          get_input_from_eval_string = false;
          parser_end_of_input = false;

          if (! force_script && looking_at_function_keyword (ffile))
            {
              file_type = "function";

              Vecho_executing_commands = ECHO_OFF;

              reading_classdef_file = false;
              reading_fcn_file = true;
              reading_script_file = false;
            }
          else if (! force_script && looking_at_classdef_keyword (ffile))
            {
              file_type = "classdef";

              Vecho_executing_commands = ECHO_OFF;

              reading_classdef_file = true;
              reading_fcn_file = false;
              reading_script_file = false;
            }
          else
            {
              file_type = "script";

              Vecho_executing_commands = ECHO_OFF;

              reading_classdef_file = false;
              reading_fcn_file = false;
              reading_script_file = true;
            }

          YY_BUFFER_STATE old_buf = current_buffer ();
          YY_BUFFER_STATE new_buf = create_buffer (ffile);

          frame.add_fcn (switch_to_buffer, old_buf);
          frame.add_fcn (delete_buffer, new_buf);

          switch_to_buffer (new_buf);

          frame.protect_var (primary_fcn_ptr);
          primary_fcn_ptr = 0;

          reset_parser ();

          // Do this with an unwind-protect cleanup function so that
          // the forced variables will be unmarked in the event of an
          // interrupt. 
          symbol_table::scope_id scope = symbol_table::top_scope ();
          frame.add_fcn (symbol_table::unmark_forced_variables, scope);

          if (! help_txt.empty ())
            help_buf.push (help_txt);

          if (reading_script_file)
            prep_lexer_for_script_file ();
          else
            prep_lexer_for_function_file ();

          lexer_flags.parsing_class_method = ! dispatch_type.empty ();

          int status = yyparse ();

          fcn_ptr = primary_fcn_ptr;

          if (reading_fcn_file && endfunction_found && max_function_depth > 1)
            warning_with_id ("Octave:nested-functions-coerced",
                             "nested functions are coerced into subfunctions "
                             "in file %s", ff.c_str ());

          if (status != 0)
            error ("parse error while reading %s file %s",
                   file_type.c_str(), ff.c_str ());
        }
    }
  else if (require_file)
    error ("no such file, `%s'", ff.c_str ());
  else if (! warn_for.empty ())
    error ("%s: unable to open file `%s'", warn_for.c_str (), ff.c_str ());    

  return fcn_ptr;
}

std::string
get_help_from_file (const std::string& nm, bool& symbol_found,
                    std::string& file)
{
  std::string retval;

  file = fcn_file_in_path (nm);

  if (! file.empty ())
    {
      symbol_found = true;

      FILE *fptr = fopen (file.c_str (), "r");

      if (fptr)
        {
          unwind_protect frame;
          frame.add_fcn (safe_fclose, fptr);

          bool eof;
          retval = gobble_leading_white_space (fptr, eof);

          if (retval.empty ())
            {
              octave_function *fcn = parse_fcn_file (file, "");

              if (fcn)
                {
                  retval = fcn->doc_string ();

                  delete fcn;
                }
            }
        }
    }

  return retval;
}

std::string
get_help_from_file (const std::string& nm, bool& symbol_found)
{
  std::string file;
  return get_help_from_file (nm, symbol_found, file);
}

std::string
lookup_autoload (const std::string& nm)
{
  std::string retval;

  typedef std::map<std::string, std::string>::const_iterator am_iter;

  am_iter p = autoload_map.find (nm);

  if (p != autoload_map.end ())
    retval = load_path::find_file (p->second);

  return retval;
}

string_vector 
autoloaded_functions (void)
{
  string_vector names (autoload_map.size());

  octave_idx_type i = 0;
  typedef std::map<std::string, std::string>::const_iterator am_iter;
  for (am_iter p = autoload_map.begin (); p != autoload_map.end (); p++)
    names[i++] = p->first;

  return names;
}

string_vector
reverse_lookup_autoload (const std::string& nm)
{
  string_vector names;

  typedef std::map<std::string, std::string>::const_iterator am_iter;
  for (am_iter p = autoload_map.begin (); p != autoload_map.end (); p++)
    if (nm == p->second)
      names.append (p->first);

  return names;
}

octave_function *
load_fcn_from_file (const std::string& file_name, const std::string& dir_name,
                    const std::string& dispatch_type,
                    const std::string& fcn_name, bool autoload)
{
  octave_function *retval = 0;

  unwind_protect frame;

  std::string nm = file_name;

  size_t nm_len = nm.length ();

  std::string file;

  frame.protect_var (fcn_file_from_relative_lookup);

  fcn_file_from_relative_lookup = false;

  file = nm;

  if ((nm_len > 4 && nm.substr (nm_len-4) == ".oct")
      || (nm_len > 4 && nm.substr (nm_len-4) == ".mex")
      || (nm_len > 2 && nm.substr (nm_len-2) == ".m"))
    {
      nm = octave_env::base_pathname (file);
      nm = nm.substr (0, nm.find_last_of ('.'));
    }

  if (autoload)
    {
      frame.protect_var (autoloading);
      autoloading = true;
    }

  fcn_file_from_relative_lookup = ! octave_env::absolute_pathname (file);

  file = octave_env::make_absolute (file);

  int len = file.length ();

  if (len > 4 && file.substr (len-4, len-1) == ".oct")
    {
      if (autoload && ! fcn_name.empty ())
        nm = fcn_name;

      retval = octave_dynamic_loader::load_oct (nm, file, fcn_file_from_relative_lookup);
    }
  else if (len > 4 && file.substr (len-4, len-1) == ".mex")
    {
      // Temporarily load m-file version of mex-file, if it exists,
      // to get the help-string to use.
      frame.protect_var (curr_fcn_file_name);
      frame.protect_var (curr_fcn_file_full_name);

      curr_fcn_file_name = nm;
      curr_fcn_file_full_name = file.substr (0, len - 2);

      octave_function *tmpfcn = parse_fcn_file (file.substr (0, len - 2), 
                                                dispatch_type, autoloading, 
                                                false);

      retval = octave_dynamic_loader::load_mex (nm, file, fcn_file_from_relative_lookup);

      if (tmpfcn)
        retval->document (tmpfcn->doc_string ());
      delete tmpfcn;
    }
  else if (len > 2)
    {
      // These are needed by yyparse.

      frame.protect_var (curr_fcn_file_name);
      frame.protect_var (curr_fcn_file_full_name);

      curr_fcn_file_name = nm;
      curr_fcn_file_full_name = file;

      retval = parse_fcn_file (file, dispatch_type, autoloading);
    }

  if (retval)
    {
      retval->stash_dir_name (dir_name);

      if (retval->is_user_function ())
        {
          symbol_table::scope_id id = retval->scope ();

          symbol_table::stash_dir_name_for_subfunctions (id, dir_name);
        }
    }

  return retval;
}

DEFUN (autoload, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} autoload (@var{function}, @var{file})\n\
Define @var{function} to autoload from @var{file}.\n\
\n\
The second argument, @var{file}, should be an absolute file name or\n\
a file name in the same directory as the function or script from which\n\
the autoload command was run.  @var{file} should not depend on the\n\
Octave load path.\n\
\n\
Normally, calls to @code{autoload} appear in PKG_ADD script files that\n\
are evaluated when a directory is added to the Octave's load path.  To\n\
avoid having to hardcode directory names in @var{file}, if @var{file}\n\
is in the same directory as the PKG_ADD script then\n\
\n\
@example\n\
autoload (\"foo\", \"bar.oct\");\n\
@end example\n\
\n\
will load the function @code{foo} from the file @code{bar.oct}.  The above\n\
when @code{bar.oct} is not in the same directory or uses like\n\
\n\
@example\n\
autoload (\"foo\", file_in_loadpath (\"bar.oct\"))\n\
@end example\n\
\n\
@noindent\n\
are strongly discouraged, as their behavior might be unpredictable.\n\
\n\
With no arguments, return a structure containing the current autoload map.\n\
@seealso{PKG_ADD}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      Cell func_names (dim_vector (autoload_map.size (), 1));
      Cell file_names (dim_vector (autoload_map.size (), 1));

      octave_idx_type i = 0;
      typedef std::map<std::string, std::string>::const_iterator am_iter;
      for (am_iter p = autoload_map.begin (); p != autoload_map.end (); p++)
        {
          func_names(i) = p->first;
          file_names(i) = p->second;

          i++;
        }

      Octave_map m;

      m.assign ("function", func_names);
      m.assign ("file", file_names);

      retval = m;
    }
  else if (nargin == 2)
    {
      string_vector argv = args.make_argv ("autoload");

      if (! error_state)
        {
          std::string nm = argv[2];

          if (! octave_env::absolute_pathname (nm))
            {
              octave_user_code *fcn = octave_call_stack::caller_user_code ();

              bool found = false;

              if (fcn)
                {
                  std::string fname = fcn->fcn_file_name ();

                  if (! fname.empty ())
                    {
                      fname = octave_env::make_absolute (fname);
                      fname = fname.substr (0, fname.find_last_of (file_ops::dir_sep_str ()) + 1);

                      file_stat fs (fname + nm);

                      if (fs.exists ())
                        {
                          nm = fname + nm;
                          found = true;
                        }
                    }
                }
              if (! found)
                warning_with_id ("Octave:autoload-relative-file-name",
                                 "autoload: `%s' is not an absolute file name",
                                 nm.c_str ());
            }
          autoload_map[argv[1]] = nm;
        }
    }
  else
    print_usage ();

  return retval;
}

void
source_file (const std::string& file_name, const std::string& context,
             bool verbose, bool require_file, const std::string& warn_for)
{
  // Map from absolute name of script file to recursion level.  We
  // use a map instead of simply placing a limit on recursion in the
  // source_file function so that two mutually recursive scripts
  // written as
  //
  //   foo1.m:
  //   ------
  //   foo2
  //
  //   foo2.m:
  //   ------
  //   foo1
  //
  // and called with
  //
  //   foo1
  //
  // (for example) will behave the same if they are written as
  //
  //   foo1.m:
  //   ------
  //   source ("foo2.m")
  //
  //   foo2.m:
  //   ------
  //   source ("foo1.m")
  //
  // and called with
  //
  //   source ("foo1.m")
  //
  // (for example).

  static std::map<std::string, int> source_call_depth;

  std::string file_full_name = file_ops::tilde_expand (file_name);

  file_full_name = octave_env::make_absolute (file_full_name);

  unwind_protect frame;

  frame.protect_var (curr_fcn_file_name);
  frame.protect_var (curr_fcn_file_full_name);

  curr_fcn_file_name = file_name;
  curr_fcn_file_full_name = file_full_name;

  if (source_call_depth.find (file_full_name) == source_call_depth.end ())
    source_call_depth[file_full_name] = -1;

  frame.protect_var (source_call_depth[file_full_name]);

  source_call_depth[file_full_name]++;

  if (source_call_depth[file_full_name] >= Vmax_recursion_depth)
    {
      error ("max_recursion_depth exceeded");
      return;
    }

  if (! context.empty ())
    {
      if (context == "caller")
        octave_call_stack::goto_caller_frame ();
      else if (context == "base")
        octave_call_stack::goto_base_frame ();
      else
        error ("source: context must be \"caller\" or \"base\"");

      if (! error_state)
        frame.add_fcn (octave_call_stack::pop);
    }      

  if (! error_state)
    {
      octave_function *fcn = parse_fcn_file (file_full_name, "", true,
                                             require_file, warn_for);

      if (! error_state)
        {
          if (fcn && fcn->is_user_script ())
            {
              octave_value_list args;

              if (verbose)
                {
                  std::cout << "executing commands from " << file_full_name << " ... ";
                  reading_startup_message_printed = true;
                  std::cout.flush ();
                }

              fcn->do_multi_index_op (0, args);

              if (verbose)
                std::cout << "done." << std::endl;

              delete fcn;
            }
        }
      else
        error ("source: error sourcing file `%s'",
               file_full_name.c_str ());
    }
}

DEFUN (mfilename, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mfilename ()\n\
@deftypefnx {Built-in Function} {} mfilename (@code{\"fullpath\"})\n\
@deftypefnx {Built-in Function} {} mfilename (@code{\"fullpathext\"})\n\
Return the name of the currently executing file.  At the top-level,\n\
return the empty string.  Given the argument @code{\"fullpath\"},\n\
include the directory part of the file name, but not the extension.\n\
Given the argument @code{\"fullpathext\"}, include the directory part\n\
of the file name and the extension.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 1)
    {
      print_usage ();
      return retval;
    }

  std::string arg;

  if (nargin == 1)
    {
      arg = args(0).string_value ();

      if (error_state)
        {
          error ("mfilename: expecting argument to be a character string");
          return retval;
        }
    }

  std::string fname;

  octave_user_code *fcn = octave_call_stack::caller_user_code ();

  if (fcn)
    {
      fname = fcn->fcn_file_name ();

      if (fname.empty ())
        fname = fcn->name ();
    }

  if (arg == "fullpathext")
    retval = fname;
  else
    {
      size_t dpos = fname.rfind (file_ops::dir_sep_char ());
      size_t epos = fname.rfind ('.');

      if (epos <= dpos)
        epos = std::string::npos;

      fname = (epos != std::string::npos) ? fname.substr (0, epos) : fname;

      if (arg == "fullpath")
        retval = fname;
      else
        retval = (dpos != std::string::npos) ? fname.substr (dpos+1) : fname;
    }

  return retval;
}


DEFUN (source, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} source (@var{file})\n\
Parse and execute the contents of @var{file}.  This is equivalent to\n\
executing commands from a script file, but without requiring the file to\n\
be named @file{@var{file}.m}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string file_name = args(0).string_value ();

      if (! error_state)
        {
          std::string context;

          if (nargin == 2)
            context = args(1).string_value ();

          if (! error_state)
            source_file (file_name, context);
          else
            error ("source: expecting context to be character string");
        }
      else
        error ("source: expecting file name as argument");
    }
  else
    print_usage ();

  return retval;
}

// Evaluate an Octave function (built-in or interpreted) and return
// the list of result values.  NAME is the name of the function to
// call.  ARGS are the arguments to the function.  NARGOUT is the
// number of output arguments expected. 

octave_value_list
feval (const std::string& name, const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  octave_value fcn = symbol_table::find_function (name, args);

  if (fcn.is_defined ())
    retval = fcn.do_multi_index_op (nargout, args);
  else
    {
      maybe_missing_function_hook (name);
      if (! error_state)
        error ("feval: function `%s' not found", name.c_str ());
    }

  return retval;
}

octave_value_list
feval (octave_function *fcn, const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  if (fcn)
    retval = fcn->do_multi_index_op (nargout, args);

  return retval;
}

static octave_value_list
get_feval_args (const octave_value_list& args)
{
  octave_value_list retval = args.slice (1, args.length () - 1);

  string_vector arg_names = args.name_tags ();

  if (arg_names.length () > 1)
    {
      string_vector tmp_arg_names = arg_names.linear_slice (1, args.length () - 1);

      retval.stash_name_tags (tmp_arg_names);
    }

  return retval;
}


// Evaluate an Octave function (built-in or interpreted) and return
// the list of result values.  The first element of ARGS should be a
// string containing the name of the function to call, then the rest
// are the actual arguments to the function.  NARGOUT is the number of
// output arguments expected.

octave_value_list
feval (const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      octave_value f_arg = args(0);

      if (f_arg.is_string ())
        {
          std::string name = f_arg.string_value ();

          if (! error_state)
            {
              octave_value_list tmp_args = get_feval_args (args);

              retval = feval (name, tmp_args, nargout);
            }
        }
      else if (f_arg.is_function_handle () || f_arg.is_inline_function ())
        {
          const octave_value_list tmp_args = get_feval_args (args);

          retval = f_arg.do_multi_index_op (nargout, tmp_args);
        }
      else
        error ("feval: first argument must be a string, inline function or a function handle");
    }

  return retval;
}

DEFUN (feval, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} feval (@var{name}, @dots{})\n\
Evaluate the function named @var{name}.  Any arguments after the first\n\
are passed on to the named function.  For example,\n\
\n\
@example\n\
feval (\"acos\", -1)\n\
     @result{} 3.1416\n\
@end example\n\
\n\
@noindent\n\
calls the function @code{acos} with the argument @samp{-1}.\n\
\n\
The function @code{feval} is necessary in order to be able to write\n\
functions that call user-supplied functions, because Octave does not\n\
have a way to declare a pointer to a function (like C) or to declare a\n\
special kind of variable that can be used to hold the name of a function\n\
(like @code{EXTERNAL} in Fortran).  Instead, you must refer to functions\n\
by name, and use @code{feval} to call them.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    retval = feval (args, nargout);
  else
    print_usage ();

  return retval;
}

octave_value_list
eval_string (const std::string& s, bool silent, int& parse_status, int nargout)
{
  octave_value_list retval;

  unwind_protect frame;

  frame.protect_var (input_line_number);
  frame.protect_var (current_input_column);
  frame.protect_var (get_input_from_eval_string);
  frame.protect_var (input_from_eval_string_pending);
  frame.protect_var (parser_end_of_input);
  frame.protect_var (line_editing);
  frame.protect_var (current_eval_string);
  frame.protect_var (current_function_depth);
  frame.protect_var (max_function_depth);
  frame.protect_var (parsing_subfunctions);
  frame.protect_var (endfunction_found);
  frame.protect_var (reading_fcn_file);
  frame.protect_var (reading_script_file);
  frame.protect_var (reading_classdef_file);

  input_line_number = 1;
  current_input_column = 1;
  get_input_from_eval_string = true;
  input_from_eval_string_pending = true;
  parser_end_of_input = false;
  line_editing = false;
  current_function_depth = 0;
  max_function_depth = 0;
  parsing_subfunctions = false;
  endfunction_found = false;
  reading_fcn_file = false;
  reading_script_file = false;
  reading_classdef_file = false;

  current_eval_string = s;

  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer (0);

  frame.add_fcn (switch_to_buffer, old_buf);
  frame.add_fcn (delete_buffer, new_buf);

  switch_to_buffer (new_buf);

  do
    {
      reset_parser ();

      frame.protect_var (global_command);

      // Do this with an unwind-protect cleanup function so that the
      // forced variables will be unmarked in the event of an
      // interrupt.
      symbol_table::scope_id scope = symbol_table::top_scope ();
      frame.add_fcn (symbol_table::unmark_forced_variables, scope);

      parse_status = yyparse ();

      tree_statement_list *command_list = global_command;

      // Unmark forced variables.
      // Restore previous value of global_command.
      frame.run_top (2);

      if (parse_status == 0)
        {
          if (command_list)
            {
              tree_statement *stmt = 0;

              if (command_list->length () == 1
                  && (stmt = command_list->front ())
                  && stmt->is_expression ())
                {
                  tree_expression *expr = stmt->expression ();

                  if (silent)
                    expr->set_print_flag (false);

                  bool do_bind_ans = false;

                  if (expr->is_identifier ())
                    {
                      tree_identifier *id
                        = dynamic_cast<tree_identifier *> (expr);

                      do_bind_ans = (! id->is_variable ());
                    }
                  else
                    do_bind_ans = (! expr->is_assignment_expression ());

                  retval = expr->rvalue (nargout);

                  if (do_bind_ans && ! (error_state || retval.empty ()))
                    bind_ans (retval(0), expr->print_result ());

                  if (nargout == 0)
                    retval = octave_value_list ();
                }
              else if (nargout == 0)
                command_list->accept (*current_evaluator);
              else
                error ("eval: invalid use of statement list");

              delete command_list;

              command_list = 0;

              if (error_state
                  || tree_return_command::returning
                  || tree_break_command::breaking
                  || tree_continue_command::continuing)
                break;
            }
          else if (parser_end_of_input)
            break;
        }
    }
  while (parse_status == 0);

  return retval;
}

octave_value
eval_string (const std::string& s, bool silent, int& parse_status)
{
  octave_value retval;

  octave_value_list tmp = eval_string (s, silent, parse_status, 1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

static octave_value_list
eval_string (const octave_value& arg, bool silent, int& parse_status,
             int nargout)
{
  std::string s = arg.string_value ();

  if (error_state)
    {
      error ("eval: expecting std::string argument");
      return octave_value (-1);
    }

  return eval_string (s, silent, parse_status, nargout);
}

DEFUN (eval, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} eval (@var{try}, @var{catch})\n\
Parse the string @var{try} and evaluate it as if it were an Octave\n\
program.  If that fails, evaluate the optional string @var{catch}.\n\
The string @var{try} is evaluated in the current context,\n\
so any results remain available after @code{eval} returns.\n\
\n\
The following example makes the variable @var{a} with the approximate\n\
value 3.1416 available.\n\
\n\
@example\n\
eval(\"a = acos(-1);\");\n\
@end example\n\
\n\
If an error occurs during the evaluation of @var{try} the @var{catch}\n\
string is evaluated, as the following example shows:\n\
\n\
@example\n\
eval ('error (\"This is a bad example\");',\n\
      'printf (\"This error occurred:\\n%s\\n\", lasterr ());');\n\
     @print{} This error occurred:\n\
        This is a bad example\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      unwind_protect frame;

      if (nargin > 1)
        {
          frame.protect_var (buffer_error_messages);
          buffer_error_messages++;
        }

      int parse_status = 0;

      octave_value_list tmp = eval_string (args(0), nargout > 0,
                                           parse_status, nargout);

      if (nargin > 1 && (parse_status != 0 || error_state))
        {
          error_state = 0;

          // Set up for letting the user print any messages from
          // errors that occurred in the first part of this eval().

          buffer_error_messages--;

          tmp = eval_string (args(1), nargout > 0, parse_status, nargout);

          if (nargout > 0)
            retval = tmp;
        }
      else if (nargout > 0)
        retval = tmp;
    }
  else
    print_usage ();

  return retval;
}

/*

%% test/octave.test/eval/eval-1.m
%!#test
%! x = 1;
%! assert(eval ("x"),1);

%% test/octave.test/eval/eval-2.m
%!test
%! x = 1;
%! assert(eval ("x;"));

%% test/octave.test/eval/eval-3.m
%!test
%! x = 1;
%! assert(eval ("x;"),1);

%% FIXME
%% Disable this test as adding the ";" is redundant with eval-1 and
%% in any case is a syntax error with assert
%% test/octave.test/eval/eval-4.m
%!#test
%! x = 1;
%! assert(eval ("x");,1);

%% test/octave.test/eval/eval-5.m
%!test
%! eval ("flipud = 2;");
%! assert(flipud,2);

%% test/octave.test/eval/eval-6.m
%!function y = f ()
%!  eval ("flipud = 2;");
%!  y = flipud;
%!test
%! assert(f,2);

%% test/octave.test/eval/eval-7.m
%!#test
%! eval ("x = 1");
%! assert(x,1);

%% test/octave.test/eval/eval-8.m
%!test
%! eval ("x = 1;")
%! assert(x,1);

%% test/octave.test/eval/eval-9.m
%!test
%! eval ("x = 1;");
%! assert(x,1);

%% test/octave.test/eval/eval-10.m
%!#test
%! eval ("x = 1")
%! assert(x,1);

%% test/octave.test/eval/eval-11.m
%!test
%! x = 1;
%! y = eval ("x");
%! assert(y,1);

%% test/octave.test/eval/eval-12.m
%!test
%! x = 1;
%! y = eval ("x;");
%! assert(y,1);

%% test/octave.test/eval/eval-13.m
%!test
%! x = 1;
%! y = eval ("x;");
%! assert(y,1);

%% test/octave.test/eval/eval-14.m
%!test
%! x = 1;
%! y = eval ("x");
%! assert(y,1);

*/

DEFUN (assignin, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} assignin (@var{context}, @var{varname}, @var{value})\n\
Assign @var{value} to @var{varname} in context @var{context}, which\n\
may be either @code{\"base\"} or @code{\"caller\"}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 3)
    {
      std::string context = args(0).string_value ();

      if (! error_state)
        {
          unwind_protect frame;

          if (context == "caller")
            octave_call_stack::goto_caller_frame ();
          else if (context == "base")
            octave_call_stack::goto_base_frame ();
          else
            error ("assignin: context must be \"caller\" or \"base\"");

          if (! error_state)
            {
              frame.add_fcn (octave_call_stack::pop);

              std::string nm = args(1).string_value ();

              if (! error_state)
                {
                  if (valid_identifier (nm))
                    symbol_table::varref (nm) = args(2);
                  else
                    error ("assignin: invalid variable name");
                }
              else
                error ("assignin: expecting variable name as second argument");
            }
        }
      else
        error ("assignin: expecting string as first argument");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (evalin, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} evalin (@var{context}, @var{try}, @var{catch})\n\
Like @code{eval}, except that the expressions are evaluated in the\n\
context @var{context}, which may be either @code{\"caller\"} or\n\
@code{\"base\"}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 1)
    {
      std::string context = args(0).string_value ();

      if (! error_state)
        {
          unwind_protect frame;

          if (context == "caller")
            octave_call_stack::goto_caller_frame ();
          else if (context == "base")
            octave_call_stack::goto_base_frame ();
          else
            error ("evalin: context must be \"caller\" or \"base\"");

          if (! error_state)
            {
              frame.add_fcn (octave_call_stack::pop);

              if (nargin > 2)
                {
                  frame.protect_var (buffer_error_messages);
                  buffer_error_messages++;
                }

              int parse_status = 0;

              octave_value_list tmp = eval_string (args(1), nargout > 0,
                                                   parse_status, nargout);

              if (nargout > 0)
                retval = tmp;

              if (nargin > 2 && (parse_status != 0 || error_state))
                {
                  error_state = 0;

                  // Set up for letting the user print any messages from
                  // errors that occurred in the first part of this eval().

                  buffer_error_messages--;

                  tmp = eval_string (args(2), nargout > 0,
                                     parse_status, nargout);

                  retval = (nargout > 0) ? tmp : octave_value_list ();
                }
            }
        }
      else
        error ("evalin: expecting string as first argument");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__parser_debug_flag__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{old_val} =} __parser_debug_flag__ (@var{new_val}))\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  bool debug_flag = octave_debug;

  retval = set_internal_variable (debug_flag, args, nargout,
                                  "__parser_debug_flag__");

  octave_debug = debug_flag;

  return retval;
}
