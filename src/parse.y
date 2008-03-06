/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2003, 2004, 2005, 2006, 2007, 2008 John W. Eaton

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

#ifdef YYBYACC
#include <cstdlib>
#endif

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
#include "toplev.h"
#include "pager.h"
#include "parse.h"
#include "pt-all.h"
#include "symtab.h"
#include "token.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// The current input line number.
int input_line_number = 0;

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

// TRUE means that we are in the process of evaluating a function
// body.  The parser might be called in that case if we are looking at
// an eval() statement.
bool evaluating_function_body = false;

// Keep a count of how many END tokens we expect.
int end_tokens_expected = 0;

// Keep track of symbol table information when parsing functions.
std::stack<symbol_table::scope_id> symtab_context;

// Name of parent function when parsing function files that might
// contain nested functions.
std::string parent_function_name;

// Name of the current class when we are parsing class methods or
// constructors.
std::string current_class_name;

// TRUE means we are in the process of autoloading a function.
static bool autoloading = false;

// TRUE means the current function file was found in a relative path
// element.
static bool fcn_file_from_relative_lookup = false;

// If nonzero, this is a pointer to the function we just finished
// parsing.
static octave_function *curr_fcn_ptr = 0;

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
make_do_until_command (token *do_tok, tree_statement_list *body,
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
make_elseif_clause (tree_expression *expr, tree_statement_list *list,
		    octave_comment_list *lc);

// Finish a switch command.
static tree_switch_command *
finish_switch_command (token *switch_tok, tree_expression *expr,
		       tree_switch_case_list *list, token *end_tok,
		       octave_comment_list *lc);

// Build a switch case.
static tree_switch_case *
make_switch_case (tree_expression *expr, tree_statement_list *list,
		  octave_comment_list *lc);

// Build an assignment to a variable.
static tree_expression *
make_assign_op (int op, tree_argument_list *lhs, token *eq_tok,
		tree_expression *rhs);

// Begin defining a function.
static octave_user_function *
start_function (tree_parameter_list *param_list, tree_statement_list *body);

// Do most of the work for defining a function.
static octave_user_function *
frob_function (const std::string& fname, octave_user_function *fcn);

// Finish defining a function.
static void
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
static void
set_stmt_print_flag (tree_statement_list *, char, bool);

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

// Other tokens.
%token END_OF_INPUT LEXICAL_ERROR
%token FCN VARARGIN VARARGOUT
%token CLOSE_BRACE

// Nonterminals we construct.
%type <comment_type> stash_comment function_beg
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
%type <tree_identifier_type> identifier fcn_name
%type <octave_user_function_type> function1 function2
%type <tree_index_expression_type> word_list_cmd
%type <tree_colon_expression_type> colon_expr1
%type <tree_argument_list_type> arg_list word_list assign_lhs
%type <tree_argument_list_type> cell_or_matrix_row
%type <tree_parameter_list_type> param_list param_list1 param_list2
%type <tree_parameter_list_type> return_list return_list1
%type <tree_command_type> command select_command loop_command
%type <tree_command_type> jump_command except_command function
%type <tree_if_command_type> if_command
%type <tree_if_clause_type> elseif_clause else_clause
%type <tree_if_command_list_type> if_cmd_list1 if_cmd_list
%type <tree_switch_command_type> switch_command
%type <tree_switch_case_type> switch_case default_case
%type <tree_switch_case_list_type> case_list1 case_list
%type <tree_decl_elt_type> decl2
%type <tree_decl_init_list_type> decl1
%type <tree_decl_command_type> declaration
%type <tree_statement_type> statement
%type <tree_statement_list_type> simple_list simple_list1 list list1
%type <tree_statement_list_type> opt_list input1

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

input		: input1
		  {
		    global_command = $1;
		    promptflag = 1;
		    YYACCEPT;
		  }
		| simple_list parse_error
		  { ABORT_PARSE; }
		| parse_error
		  { ABORT_PARSE; }
		;

input1		: '\n'
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

simple_list	: simple_list1 opt_sep_no_nl
		  {
		    set_stmt_print_flag ($1, $2, false);
		    $$ = $1;
		  }
		;

simple_list1	: statement
		  { $$ = new tree_statement_list ($1); }
		| simple_list1 sep_no_nl statement
		  {
		    set_stmt_print_flag ($1, $2, false);
		    $1->append ($3);
		    $$ = $1;
		  }
		;

opt_list	: // empty
		  { $$ = new tree_statement_list (); }
		| list
		  { $$ = $1; }
		;

list		: list1 opt_sep
		  {
		    set_stmt_print_flag ($1, $2, true);
		    $$ = $1;
		  }
		;

list1		: statement
		  {
		    lexer_flags.beginning_of_function = false;
		    $$ = new tree_statement_list ($1);
		  }
		| list1 sep statement
		  {
		    set_stmt_print_flag ($1, $2, true);
		    $1->append ($3);
		    $$ = $1;
		  }
		;

statement	: expression
		  {
		    octave_comment_list *comment
		      = octave_comment_buffer::get_comment ();

		    $$ = new tree_statement ($1, comment);
		  }
		| command
		  {
		    octave_comment_list *comment
		      = octave_comment_buffer::get_comment ();

		    $$ = new tree_statement ($1, comment);
		  }
		;

// ===========
// Expressions
// ===========

identifier	: NAME
		  {
		    symbol_table::symbol_record *sr = $1->sym_rec ();
		    $$ = new tree_identifier (*sr, $1->line (), $1->column ());
		  }
		;

string		: DQ_STRING
		  { $$ = make_constant (DQ_STRING, $1); }
		| SQ_STRING
		  { $$ = make_constant (SQ_STRING, $1); }
		;

constant	: NUM
		  { $$ = make_constant (NUM, $1); }
		| IMAG_NUM
		  { $$ = make_constant (IMAG_NUM, $1); }
		| string
		  { $$ = $1; }
		;

matrix		: '[' ']'
		  {
		    $$ = new tree_constant (octave_value (Matrix ()));
		    lexer_flags.looking_at_matrix_or_assign_lhs = false;
		  }
		| '[' ';' ']'
		  {
		    $$ = new tree_constant (octave_value (Matrix ()));
		    lexer_flags.looking_at_matrix_or_assign_lhs = false;
		  }
		| '[' matrix_rows ']'
		  {
		    $$ = finish_matrix ($2);
		    lexer_flags.looking_at_matrix_or_assign_lhs = false;
		  }
		;

matrix_rows	: matrix_rows1
		  { $$ = $1; }
		| matrix_rows1 ';'	// Ignore trailing semicolon.
		  { $$ = $1; }
		;

matrix_rows1	: cell_or_matrix_row
		  { $$ = new tree_matrix ($1); }
		| matrix_rows1 ';' cell_or_matrix_row
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
		;

cell		: '{' '}'
		  { $$ = new tree_constant (octave_value (Cell ())); }
		| '{' ';' '}'
		  { $$ = new tree_constant (octave_value (Cell ())); }
		| '{' cell_rows '}'
		  { $$ = finish_cell ($2); }
		;

cell_rows	: cell_rows1
		  { $$ = $1; }
		| cell_rows1 ';'	// Ignore trailing semicolon.
		  { $$ = $1; }
		;

cell_rows1	: cell_or_matrix_row
		  { $$ = new tree_cell ($1); }
		| cell_rows1 ';' cell_or_matrix_row
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
		;

cell_or_matrix_row
		: arg_list
		  { $$ = $1; }
		| arg_list ','	// Ignore trailing comma.
		  { $$ = $1; }
		;

fcn_handle	: '@' FCN_HANDLE
		  {
		    $$ = make_fcn_handle ($2);
		    lexer_flags.looking_at_function_handle--;
		  }
		;

anon_fcn_handle	: '@' param_list statement
		  { $$ = make_anon_fcn_handle ($2, $3); }
		;

primary_expr	: identifier
		  { $$ = $1; }
		| constant
		  { $$ = $1; }
		| fcn_handle
		  { $$ = $1; }
		| matrix
		  { $$ = $1; }
		| cell
		  { $$ = $1; }
		| '(' expression ')'
		  { $$ = $2->mark_in_parens (); }
		;

magic_colon	: ':'
		  {
		    octave_value tmp (octave_value::magic_colon_t);
		    $$ = new tree_constant (tmp);
		  }
		;

arg_list	: expression
		  { $$ = new tree_argument_list ($1); }
		| magic_colon
		  { $$ = new tree_argument_list ($1); }
		| arg_list ',' magic_colon
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

indirect_ref_op	: '.'
		  { lexer_flags.looking_at_indirect_ref = true; }
		;

// Two more rules for lexical feedback.  To avoid reduce/reduce
// conflicts, We use begin_obj_idx after every postfix_expr on the RHS
// of a rule, then cancel that as soon as possible for cases when we
// are not actually parsing an index expression.  Since all of those
// cases are simple tokens that don't involve examining the value of 
// lexer_flags.looking_at_object_index, I think we should be OK.

begin_obj_idx	: // empty
		  { lexer_flags.looking_at_object_index++; }
		;

cancel_obj_idx	: // empty
		  { lexer_flags.looking_at_object_index--; }
		;

postfix_expr	: primary_expr
		  { $$ = $1; }
		| postfix_expr begin_obj_idx '(' ')'
		  {
		    $$ = make_index_expression ($1, 0, '(');
		    lexer_flags.looking_at_object_index--;
		  }
		| postfix_expr begin_obj_idx '(' arg_list ')'
		  {
		    $$ = make_index_expression ($1, $4, '(');
		    lexer_flags.looking_at_object_index--;
		  }
		| postfix_expr begin_obj_idx '{' '}'
		  {
		    $$ = make_index_expression ($1, 0, '{');
		    lexer_flags.looking_at_object_index--;
		  }
		| postfix_expr begin_obj_idx '{' arg_list '}'
		  {
		    $$ = make_index_expression ($1, $4, '{');
		    lexer_flags.looking_at_object_index--;
		  }
		| postfix_expr begin_obj_idx PLUS_PLUS cancel_obj_idx
		  { $$ = make_postfix_op (PLUS_PLUS, $1, $3); }
		| postfix_expr begin_obj_idx MINUS_MINUS cancel_obj_idx
		  { $$ = make_postfix_op (MINUS_MINUS, $1, $3); }
		| postfix_expr begin_obj_idx QUOTE cancel_obj_idx
		  { $$ = make_postfix_op (QUOTE, $1, $3); }
		| postfix_expr begin_obj_idx TRANSPOSE cancel_obj_idx
		  { $$ = make_postfix_op (TRANSPOSE, $1, $3); }
		| postfix_expr begin_obj_idx indirect_ref_op cancel_obj_idx STRUCT_ELT
		  { $$ = make_indirect_ref ($1, $5->text ()); }
		| postfix_expr begin_obj_idx indirect_ref_op cancel_obj_idx '(' expression ')'
		  { $$ = make_indirect_ref ($1, $6); }
		;

prefix_expr	: postfix_expr begin_obj_idx cancel_obj_idx
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

binary_expr	: prefix_expr POW prefix_expr
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

colon_expr	: colon_expr1
		  { $$ = finish_colon_expression ($1); }
		;

colon_expr1	: prefix_expr
		  { $$ = new tree_colon_expression ($1); }
		| colon_expr1 ':' prefix_expr
		  {
		    if (! ($$ = $1->append ($3)))
		      ABORT_PARSE;
		  }
		;

simple_expr	: colon_expr
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

assign_lhs	: simple_expr
		  {
		    $$ = new tree_argument_list ($1);
		    $$->mark_as_simple_assign_lhs ();
		  }
		| '[' arg_list CLOSE_BRACE
		  {
		    $$ = $2;
		    lexer_flags.looking_at_matrix_or_assign_lhs = false;
		  }
		;

assign_expr	: assign_lhs '=' expression
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

word_list_cmd	: identifier word_list
		  { $$ = make_index_expression ($1, $2, '('); }
		;

word_list	: string
		  { $$ = new tree_argument_list ($1); }
		| word_list string
		  {
		    $1->append ($2);
		    $$ = $1;
		  }
		;

expression	: simple_expr
		  { $$ = $1; }
		| word_list_cmd
		  { $$ = $1; }
		| assign_expr
		  { $$ = $1; }
		| anon_fcn_handle
		  { $$ = $1; }
		;

// ================================================
// Commands, declarations, and function definitions
// ================================================

command		: declaration
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
		;

// =====================
// Declaration statemnts
// =====================

declaration	: GLOBAL decl1
		  { $$ = make_decl_command (GLOBAL, $1, $2); }
		| STATIC decl1
		  { $$ = make_decl_command (STATIC, $1, $2); }
		;

decl1		: decl2
		  { $$ = new tree_decl_init_list ($1); }
		| decl1 decl2
		  {
		    $1->append ($2);
		    $$ = $1;
		  }
		;

decl2		: identifier
		  { $$ = new tree_decl_elt ($1); }
		| identifier '=' expression
		  { $$ = new tree_decl_elt ($1, $3); }
		;

// ====================
// Selection statements
// ====================

select_command	: if_command
		  { $$ = $1; }
		| switch_command
		  { $$ = $1; }
		;

// ============
// If statement
// ============

if_command	: IF stash_comment if_cmd_list END
		  {
		    if (! ($$ = finish_if_command ($1, $3, $4, $2)))
		      ABORT_PARSE;
		  }
		;

if_cmd_list	: if_cmd_list1
		  { $$ = $1; }
		| if_cmd_list1 else_clause
		  {
		    $1->append ($2);
		    $$ = $1;
		  }
		;

if_cmd_list1	: expression opt_sep opt_list
		  { $$ = start_if_command ($1, $3); }
		| if_cmd_list1 elseif_clause
		  {
		    $1->append ($2);
		    $$ = $1;
		  }
		;

elseif_clause	: ELSEIF stash_comment opt_sep expression opt_sep opt_list
		  { $$ = make_elseif_clause ($4, $6, $2); }
		;

else_clause	: ELSE stash_comment opt_sep opt_list
		  {
		    $$ = new tree_if_clause ($4, $2);
		  }
		;

// ================
// Switch statement
// ================

switch_command	: SWITCH stash_comment expression opt_sep case_list END
		  {
		    if (! ($$ = finish_switch_command ($1, $3, $5, $6, $2)))
		      ABORT_PARSE;
		  }
		;

case_list	: // empty
		  { $$ = new tree_switch_case_list (); }
		| case_list1
		  { $$ = $1; }
		| case_list1 default_case
		  {
		    $1->append ($2);
		    $$ = $1;
		  }		
		;

case_list1	: switch_case
		  { $$ = new tree_switch_case_list ($1); }
		| case_list1 switch_case
		  {
		    $1->append ($2);
		    $$ = $1;
		  }
		;

switch_case	: CASE stash_comment opt_sep expression opt_sep opt_list
		  { $$ = make_switch_case ($4, $6, $2); }
		;

default_case	: OTHERWISE stash_comment opt_sep opt_list
		  {
		    $$ = new tree_switch_case ($4, $2);
		  }
		;

// =======
// Looping
// =======

loop_command	: WHILE stash_comment expression opt_sep opt_list END
		  {
		    if (! ($$ = make_while_command ($1, $3, $5, $6, $2)))
		      ABORT_PARSE;
		  }
		| DO stash_comment opt_sep opt_list UNTIL expression
		  {
		    if (! ($$ = make_do_until_command ($1, $4, $6, $2)))
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

jump_command	: BREAK
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

except_command	: UNWIND stash_comment opt_sep opt_list CLEANUP
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

push_fcn_symtab	: // empty
		  {
		    symtab_context.push (symbol_table::current_scope ());
		    symbol_table::set_scope (symbol_table::alloc_scope ());

		    if (! lexer_flags.parsing_nested_function)
		      symbol_table::set_parent_scope (symbol_table::current_scope ());
		  }
		;

// ===========================
// List of function parameters
// ===========================

param_list_beg	: '('
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

param_list_end	: ')'
		  { lexer_flags.looking_at_parameter_list = false; }
		;

param_list	: param_list_beg param_list1 param_list_end
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

param_list1	: // empty
		  { $$ = 0; }
		| param_list2
		  {
		    $1->mark_as_formal_parameters ();
		    $$ = $1;
		  }
		| VARARGIN
		  {
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| param_list2 ',' VARARGIN
		  {
		    $1->mark_as_formal_parameters ();
		    $1->mark_varargs ();
		    $$ = $1;
		  }
		;

param_list2	: decl2
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

return_list	: '[' ']'
		  {
		    lexer_flags.looking_at_return_list = false;
		    $$ = new tree_parameter_list ();
		  }
		| '[' VARARGOUT ']'
		  {
		    lexer_flags.looking_at_return_list = false;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| VARARGOUT
		  {
		    lexer_flags.looking_at_return_list = false;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| return_list1
		  {
		    lexer_flags.looking_at_return_list = false;
		    $$ = $1;
		  }
		| '[' return_list1 ']'
		  {
		    lexer_flags.looking_at_return_list = false;
		    $$ = $2;
		  }
		| '[' return_list1 ',' VARARGOUT ']'
		  {
		    lexer_flags.looking_at_return_list = false;
		    $2->mark_varargs ();
		    $$ = $2;
		  }
		;

return_list1	: identifier
		  { $$ = new tree_parameter_list (new tree_decl_elt ($1)); }
		| return_list1 ',' identifier
		  {
		    $1->append (new tree_decl_elt ($3));
		    $$ = $1;
		  }
		;

// ===================
// Function definition
// ===================

function_beg	: push_fcn_symtab FCN stash_comment
		  { $$ = $3; }
		;

function	: function_beg function1
		  {
		    $2->stash_leading_comment ($1);
		    recover_from_parsing_function ();
		    $$ = 0;
		  }
		| function_beg return_list '=' function1
		  {
		    finish_function ($2, $4, $1);
		    recover_from_parsing_function ();
		    $$ = 0;
		  }
		;

fcn_name	: identifier
		  {
		    std::string id_name = $1->name ();

		    if (reading_fcn_file
		        && ! lexer_flags.parsing_nested_function)
		      parent_function_name = (curr_fcn_file_name == id_name)
			? id_name : curr_fcn_file_name;

		    lexer_flags.parsed_function_name = true;

		    $$ = $1;
		  }
		;

function1	: fcn_name function2
		  {
		    std::string fname = $1->name ();

		    delete $1;

		    if (! ($$ = frob_function (fname, $2)))
		      ABORT_PARSE;
		  }
		;

function2	: param_list opt_sep opt_list function_end
		  { $$ = start_function ($1, $3); }
		| opt_sep opt_list function_end
		  { $$ = start_function (0, $2); }
		;

function_end	: END
		  {
		    if (! end_token_ok ($1, token::function_end))
		      ABORT_PARSE;
		  }
		| END_OF_INPUT
		  {
		    if (lexer_flags.parsing_nested_function)
		      lexer_flags.parsing_nested_function = -1;

		    if (! (reading_fcn_file || reading_script_file
			   || get_input_from_eval_string))
		      YYABORT;
		  }
		;

// =============
// Miscellaneous
// =============

stash_comment	: // empty
		  { $$ = octave_comment_buffer::get_comment (); }
		;

parse_error	: LEXICAL_ERROR
		  { yyerror ("parse error"); }
		| error
		;

sep_no_nl	: ','
		  { $$ = ','; }
		| ';'
		  { $$ = ';'; }
		| sep_no_nl ','
		  { $$ = $1; }
		| sep_no_nl ';'
		  { $$ = $1; }
		;

opt_sep_no_nl	: // empty
		  { $$ = 0; }
		| sep_no_nl
		  { $$ = $1; }
		;

sep		: ','
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

opt_sep		: // empty
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

  if (reading_fcn_file || reading_script_file)
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
      warning_with_id
        ("Octave:assign-as-truth-value",
         "suggest parenthesis around assignment used as truth value");
    }
}

// Maybe print a warning about switch labels that aren't constants.

static void
maybe_warn_variable_switch_label (tree_expression *expr)
{
  if (! expr->is_constant ())
    warning_with_id ("Octave:variable-switch-label",
    		     "variable switch label");
}

static tree_expression *
fold (tree_binary_expression *e)
{
  tree_expression *retval = e;

  unwind_protect::begin_frame ("fold_binary_expression");

  unwind_protect_int (error_state);
  unwind_protect_int (warning_state);

  unwind_protect_bool (discard_error_messages);
  unwind_protect_bool (discard_warning_messages);

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
      octave_value tmp = e->rvalue ();

      if (! (error_state || warning_state))
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  std::ostringstream buf;

	  tree_print_code tpc (buf);

	  e->accept (tpc);

	  tc_retval->stash_original_text (buf.str ());

	  delete e;

	  retval = tc_retval;
	}
    }

  unwind_protect::run_frame ("fold_binary_expression");

  return retval;
}

static tree_expression *
fold (tree_unary_expression *e)
{
  tree_expression *retval = e;

  unwind_protect::begin_frame ("fold_unary_expression");

  unwind_protect_int (error_state);
  unwind_protect_int (warning_state);

  unwind_protect_bool (discard_error_messages);
  unwind_protect_bool (discard_warning_messages);

  discard_error_messages = true;
  discard_warning_messages = true;

  tree_expression *op = e->operand ();

  if (op->is_constant ())
    {
      octave_value tmp = e->rvalue ();

      if (! (error_state || warning_state))
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  std::ostringstream buf;

	  tree_print_code tpc (buf);

	  e->accept (tpc);

	  tc_retval->stash_original_text (buf.str ());

	  delete e;

	  retval = tc_retval;
	}
    }

  unwind_protect::run_frame ("fold_unary_expression");

  return retval;
}

// Finish building a range.

static tree_expression *
finish_colon_expression (tree_colon_expression *e)
{
  tree_expression *retval = e;

  unwind_protect::begin_frame ("finish_colon_expression");

  unwind_protect_int (error_state);
  unwind_protect_int (warning_state);

  unwind_protect_bool (discard_error_messages);
  unwind_protect_bool (discard_warning_messages);

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
	      octave_value tmp = e->rvalue ();

	      if (! (error_state || warning_state))
		{
		  tree_constant *tc_retval = new tree_constant (tmp);

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

  unwind_protect::run_frame ("finish_colon_expression");

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
	octave_value tmp (tok_val->text (), op == DQ_STRING ? '"' : '\'');
	retval = new tree_constant (tmp, l, c);
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

  body->mark_as_function_body ();

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

	  warning_with_id
	    ("Octave:associativity-change",
	     "meaning may have changed due to change in associativity for %s operator", op_str.c_str ());
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
	    warning_with_id
	      ("Octave:precedence-change",
	       "meaning may have changed due to change in precedence for & and | operators");
        }
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_binary_expression *e
    = new tree_binary_expression (op1, op2, l, c, t);

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
make_do_until_command (token *do_tok, tree_statement_list *body,
		       tree_expression *expr, octave_comment_list *lc)
{
  tree_command *retval = 0;

  maybe_warn_assign_as_truth_value (expr);

  octave_comment_list *tc = octave_comment_buffer::get_comment ();

  lexer_flags.looping--;

  int l = do_tok->line ();
  int c = do_tok->column ();

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

  if (lexer_flags.looping || lexer_flags.defining_func
      || reading_script_file || evaluating_function_body
      || evaluating_looping_command)
    retval = new tree_break_command (l, c);
  else
    retval = new tree_no_op_command ("break", l, c);

  return retval;
}

// Build a continue command.

static tree_command *
make_continue_command (token *continue_tok)
{
  tree_command *retval = 0;

  int l = continue_tok->line ();
  int c = continue_tok->column ();

  if (lexer_flags.looping || evaluating_looping_command)
    retval = new tree_continue_command (l, c);
  else
    retval = new tree_no_op_command ("continue", l, c);

  return retval;
}

// Build a return command.

static tree_command *
make_return_command (token *return_tok)
{
  tree_command *retval = 0;

  int l = return_tok->line ();
  int c = return_tok->column ();

  if (lexer_flags.defining_func || reading_script_file
      || evaluating_function_body)
    retval = new tree_return_command (l, c);
  else
    retval = new tree_no_op_command ("return", l, c);

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

      retval = new tree_if_command (list, lc, tc, l, c);
    }

  return retval;
}

// Build an elseif clause.

static tree_if_clause *
make_elseif_clause (tree_expression *expr, tree_statement_list *list,
		    octave_comment_list *lc)
{
  maybe_warn_assign_as_truth_value (expr);

  return new tree_if_clause (expr, list, lc);
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

      retval = new tree_switch_command (expr, list, lc, tc, l, c);
    }

  return retval;
}

// Build a switch case.

static tree_switch_case *
make_switch_case (tree_expression *expr, tree_statement_list *list,
		  octave_comment_list *lc)
{
  maybe_warn_variable_switch_label (expr);

  return new tree_switch_case (expr, list, lc);
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
  else
    return new tree_multi_assignment (lhs, rhs, false, l, c, t);

  return retval;
}

// Begin defining a function.

static octave_user_function *
start_function (tree_parameter_list *param_list, tree_statement_list *body)
{
  body->mark_as_function_body ();

  // We'll fill in the return list later.

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

// Do most of the work for defining a function.

static octave_user_function *
frob_function (const std::string& fname, octave_user_function *fcn)
{
  std::string id_name = fname;

  // If input is coming from a file, issue a warning if the name of
  // the file does not match the name of the function stated in the
  // file.  Matlab doesn't provide a diagnostic (it ignores the stated
  // name).

  if (reading_fcn_file || autoloading)
    {
      if (! (autoloading
	     || lexer_flags.parsing_nested_function
	     || lexer_flags.parsing_class_method))
	{
	  // FIXME -- should curr_fcn_file_name already be
	  // preprocessed when we get here?  It seems to only be a
	  // problem with relative file names.

	  std::string nm = curr_fcn_file_name;

	  size_t pos = nm.find_last_of (file_ops::dir_sep_chars);

	  if (pos != NPOS)
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

      octave_time now;

      fcn->stash_fcn_file_name (curr_fcn_file_full_name);
      fcn->stash_fcn_file_time (now);
      fcn->mark_as_system_fcn_file ();

      if (fcn_file_from_relative_lookup)
	fcn->mark_relative ();

      if (lexer_flags.parsing_nested_function)
        fcn->stash_parent_fcn_name (parent_function_name);

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

  if (! help_buf.empty ())
    {
      fcn->document (help_buf.top ());

      help_buf.pop ();
    }

  if (lexer_flags.parsing_nested_function)
    {
      std::string nm = fcn->name ();

      fcn->mark_as_nested_function ();

      symbol_table::install_subfunction (nm, octave_value (fcn));

      if (lexer_flags.parsing_nested_function < 0)
	{
	  lexer_flags.parsing_nested_function = 0;
	  symbol_table::reset_parent_scope ();
	}
    }
  else if (! reading_fcn_file)
    {
      std::string nm = fcn->name ();

      symbol_table::install_cmdline_function (nm, octave_value (fcn));

      // Make sure that any variable with the same name as the new
      // function is cleared.

      symbol_table::varref (nm) = octave_value ();
    }
  else
    curr_fcn_ptr = fcn;

  return fcn;
}

// Finish defining a function.

static void
finish_function (tree_parameter_list *ret_list,
		 octave_user_function *fcn, octave_comment_list *lc)
{
  ret_list->mark_as_formal_parameters ();

  fcn->stash_leading_comment (lc);

  fcn->define_ret_list (ret_list);
}

static void
recover_from_parsing_function (void)
{
  if (symtab_context.empty ())
    panic_impossible ();

  symbol_table::set_scope (symtab_context.top ());
  symtab_context.pop ();

  lexer_flags.defining_func = false;
  lexer_flags.beginning_of_function = false;
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
      if (lexer_flags.defining_func)
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

// Finish building a matrix list.

static tree_expression *
finish_matrix (tree_matrix *m)
{
  tree_expression *retval = m;

  unwind_protect::begin_frame ("finish_matrix");

  unwind_protect_int (error_state);
  unwind_protect_int (warning_state);

  unwind_protect_bool (discard_error_messages);
  unwind_protect_bool (discard_warning_messages);

  discard_error_messages = true;
  discard_warning_messages = true;

  if (m->all_elements_are_constant ())
    {
      octave_value tmp = m->rvalue ();

      if (! (error_state || warning_state))
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  std::ostringstream buf;

	  tree_print_code tpc (buf);

	  m->accept (tpc);

	  tc_retval->stash_original_text (buf.str ());

	  delete m;

	  retval = tc_retval;
	}
    }

  unwind_protect::run_frame ("finish_matrix");

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
  if (lexer_flags.defining_func)
    {
      tree_statement *tmp = t->back();

      if (tmp->is_expression ())
	warning_with_id
	  ("Octave:missing-semicolon",
	   "missing semicolon near line %d, column %d in file `%s'",
	    tmp->line (), tmp->column (), curr_fcn_file_full_name.c_str ());
    }
}

static void
set_stmt_print_flag (tree_statement_list *list, char sep,
		     bool warn_missing_semi)
{
  switch (sep)
    {
    case ';':
      {
	tree_statement *tmp = list->back ();
	tmp->set_print_flag (0);
      }
      break;

    case 0:
    case ',':
    case '\n':
      if (warn_missing_semi)
	maybe_warn_missing_semi (list);
      break;

    default:
      warning ("unrecognized separator type!");
      break;
    }
}

void
parse_and_execute (FILE *f)
{
  unwind_protect::begin_frame ("parse_and_execute");

  unwind_protect_ptr (global_command);

  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer (f);

  unwind_protect::add (restore_input_buffer, old_buf);
  unwind_protect::add (delete_input_buffer, new_buf);

  switch_to_buffer (new_buf);

  unwind_protect_bool (line_editing);
  unwind_protect_bool (get_input_from_eval_string);
  unwind_protect_bool (parser_end_of_input);

  line_editing = false;
  get_input_from_eval_string = false;
  parser_end_of_input = false;

  int retval;
  do
    {
      reset_parser ();

      retval = yyparse ();

      if (retval == 0)
        {
          if (global_command)
	    {
	      global_command->eval ();

	      delete global_command;

	      global_command = 0;

	      OCTAVE_QUIT;

	      bool quit = (tree_return_command::returning
			   || tree_break_command::breaking);

	      if (tree_return_command::returning)
		tree_return_command::returning = 0;

	      if (tree_break_command::breaking)
		tree_break_command::breaking--;

	      if (error_state)
		{
		  error ("near line %d of file `%s'", input_line_number,
			 curr_fcn_file_full_name.c_str ());

		  break;
		}

	      if (quit)
		break;
	    }
	  else if (parser_end_of_input)
	    break;
        }
    }
  while (retval == 0);

  unwind_protect::run_frame ("parse_and_execute");
}

static void
safe_fclose (void *f)
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

void
parse_and_execute (const std::string& s, bool verbose, const char *warn_for)
{
  unwind_protect::begin_frame ("parse_and_execute_2");

  unwind_protect_bool (reading_script_file);
  unwind_protect_str (curr_fcn_file_full_name);

  reading_script_file = true;
  curr_fcn_file_full_name = s;

  FILE *f = get_input_from_file (s, 0);

  if (f)
    {
      unwind_protect::add (safe_fclose, f);

      octave_user_script *script = new octave_user_script (s, s, "");
      octave_call_stack::push (script);
      unwind_protect::add (octave_call_stack::unwind_pop_script, 0);

      unwind_protect_int (input_line_number);
      unwind_protect_int (current_input_column);

      input_line_number = 0;
      current_input_column = 1;

      if (verbose)
	{
	  std::cout << "reading commands from " << s << " ... ";
	  reading_startup_message_printed = true;
	  std::cout.flush ();
	}

      parse_and_execute (f);

      if (verbose)
	std::cout << "done." << std::endl;
    }
  else if (warn_for)
    error ("%s: unable to open file `%s'", warn_for, s.c_str ());

  unwind_protect::run_frame ("parse_and_execute_2");
}

static bool
looks_like_copyright (const std::string& s)
{
  // Perhaps someday we will want to do more here, so leave this as a
  // separate function.

  return (s.substr (0, 9) == "Copyright");
}

static int
text_getc (FILE *f)
{
  int c = getc (f);

  // Convert CRLF into just LF.

  if (c == '\r')
    {
      c = getc (f);

      if (c != '\n')
	{
	  ungetc (c, f);
	  c = '\r';
	}
    }

  return c;
}

// Eat whitespace and comments from FFILE, returning the text of the
// comments read if it doesn't look like a copyright notice.  If
// IN_PARTS, consider each block of comments separately; otherwise,
// grab them all at once.  If UPDATE_POS is TRUE, line and column
// number information is updated.  If SAVE_COPYRIGHT is TRUE, then
// comments that are recognized as a copyright notice are saved in the
// comment buffer.  If SKIP_CODE is TRUE, then ignore code, otherwise
// stop at the first non-whitespace character that is not part of a
// comment.

// FIXME -- skipping code will fail for something like this:
//
//   function foo (x)
//     fprintf ('%d\n', x);
//
//   % This is the help for foo.
//
// because we recognize the '%' in the fprintf format as a comment
// character.  Fixing this will probably require actually parsing the
// file properly.

// FIXME -- grab_help_text() in lex.l duplicates some of this
// code!

static std::string
gobble_leading_white_space (FILE *ffile, bool in_parts,
			    bool update_pos, bool save_copyright,
			    bool skip_code)
{
  std::string help_txt;

  // TRUE means we have already seen the first block of comments.
  bool first_comments_seen = false;

  // TRUE means we are at the beginning of a comment block.
  bool begin_comment = false;

  // TRUE means we have already cached the help text.
  bool have_help_text = false;

  // TRUE means we are currently reading a comment block.
  bool in_comment = false;

  // TRUE means we should discard the first space from the input
  // (used to strip leading spaces from the help text).
  bool discard_space = true;

  int c;

  while ((c = text_getc (ffile)) != EOF)
    {
      if (update_pos)
	current_input_column++;

      if (begin_comment)
	{
	  if (c == '%' || c == '#')
	    continue;
	  else if (discard_space && c == ' ')
	    {
	      discard_space = false;
	      continue;
	    }
	  else
	    begin_comment = false;
	}

      if (in_comment)
	{
	  if (! have_help_text)
	    {
	      first_comments_seen = true;
	      help_txt += static_cast<char> (c);
	    }

	  if (c == '\n')
	    {
	      if (update_pos)
		{
		  input_line_number++;
		  current_input_column = 0;
		}

	      in_comment = false;
	      discard_space = true;

	      if (in_parts)
		{
		  if ((c = text_getc (ffile)) != EOF)
		    {
		      if (update_pos)
			current_input_column--;
		      ungetc (c, ffile);
		      if (c == '\n')
			break;
		    }
		  else
		    break;
		}
	    }
	}
      else
	{
	  switch (c)
	    {
	    case ' ':
	    case '\t':
	      if (first_comments_seen)
		have_help_text = true;
	      break;

	    case '%':
	    case '#':
	      begin_comment = true;
	      in_comment = true;
	      break;

	    case '\n':
	      if (first_comments_seen)
		have_help_text = true;
	      if (update_pos)
		{
		  input_line_number++;
		  current_input_column = 0;
		}
	      continue;

	    default:
	      if (skip_code)
		continue;
	      else
		{
		  if (update_pos)
		    current_input_column--;
		  ungetc (c, ffile);
		  goto done;
		}
	    }
	}
    }

 done:

  if (! help_txt.empty ())
    {
      if (looks_like_copyright (help_txt))
	{
	  if (save_copyright)
	    octave_comment_buffer::append (help_txt);

	  help_txt.resize (0);
	}

      if (in_parts && help_txt.empty ())
	help_txt = gobble_leading_white_space (ffile, in_parts, update_pos,
					       false, skip_code);
    }

  return help_txt;
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
	  unwind_protect::add (safe_fclose, fptr);

	  retval = gobble_leading_white_space (fptr, true, true, false, true);

	  unwind_protect::run ();
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

static int
is_function_file (FILE *ffile)
{
  int status = 0;

  long pos = ftell (ffile);

  gobble_leading_white_space (ffile, false, false, false, false);

  char buf [10];
  fgets (buf, 10, ffile);
  int len = strlen (buf);
  if (len > 8 && strncmp (buf, "function", 8) == 0
      && ! (isalnum (buf[8]) || buf[8] == '_'))
    status = 1;

  fseek (ffile, pos, SEEK_SET);

  return status;
}

static int
is_function_file (const std::string& fname)
{
  int retval = 0;

  FILE *fid = fopen (fname.c_str (), "r");

  if (fid)
    {
      retval = is_function_file (fid);

      fclose (fid);
    }

  return retval;
}

static void
restore_command_history (void *)
{
  command_history::ignore_entries (! Vsaving_history);
}

static void
restore_input_stream (void *f)
{
  command_editor::set_input_stream (static_cast<FILE *> (f));
}

typedef octave_function * octave_function_ptr;

static octave_function *
parse_fcn_file (const std::string& ff, const std::string& dispatch_type,
		bool exec_script, bool force_script = false)
{
  unwind_protect::begin_frame ("parse_fcn_file");

  octave_function *fcn_ptr = 0;

  // Open function file and parse.

  bool old_reading_fcn_file_state = reading_fcn_file;

  FILE *in_stream = command_editor::get_input_stream ();

  unwind_protect::add (restore_input_stream, in_stream);

  unwind_protect_ptr (ff_instream);

  unwind_protect_int (input_line_number);
  unwind_protect_int (current_input_column);
  unwind_protect_int (end_tokens_expected);
  unwind_protect_bool (reading_fcn_file);
  unwind_protect_bool (line_editing);
  unwind_protect_str (parent_function_name);
  unwind_protect_str (current_class_name);

  input_line_number = 0;
  current_input_column = 1;
  end_tokens_expected = 0;
  reading_fcn_file = true;
  line_editing = false;
  parent_function_name = "";
  current_class_name = dispatch_type;

  FILE *ffile = get_input_from_file (ff, 0);

  unwind_protect::add (safe_fclose, ffile);

  if (ffile)
    {
      // Check to see if this file defines a function or is just a
      // list of commands.

      if (! force_script && is_function_file (ffile))
	{
	  // FIXME -- we shouldn't need both the
	  // command_history object and the
	  // Vsaving_history variable...
	  command_history::ignore_entries ();

	  unwind_protect::add (restore_command_history, 0);

	  unwind_protect_int (Vecho_executing_commands);
	  unwind_protect_bool (Vsaving_history);
	  unwind_protect_bool (reading_fcn_file);
	  unwind_protect_bool (get_input_from_eval_string);
	  unwind_protect_bool (parser_end_of_input);

	  Vecho_executing_commands = ECHO_OFF;
	  Vsaving_history = false;
	  reading_fcn_file = true;
	  get_input_from_eval_string = false;
	  parser_end_of_input = false;

	  YY_BUFFER_STATE old_buf = current_buffer ();
	  YY_BUFFER_STATE new_buf = create_buffer (ffile);

	  unwind_protect::add (restore_input_buffer, old_buf);
	  unwind_protect::add (delete_input_buffer, new_buf);

	  switch_to_buffer (new_buf);

	  unwind_protect_ptr (curr_fcn_ptr);
	  curr_fcn_ptr = 0;

	  reset_parser ();

	  std::string txt
	    = gobble_leading_white_space (ffile, true, true, true, false);

	  help_buf.push (txt);

	  octave_comment_buffer::append (txt);

	  // FIXME -- this should not be necessary.
	  gobble_leading_white_space (ffile, false, true, false, false);

	  lexer_flags.parsing_class_method = ! dispatch_type.empty ();

	  int status = yyparse ();

	  fcn_ptr = curr_fcn_ptr;

	  if (status != 0)
	    error ("parse error while reading function file %s", ff.c_str ());
	}
      else if (exec_script)
	{
	  // The value of `reading_fcn_file' will be restored to the
	  // proper value when we unwind from this frame.
	  reading_fcn_file = old_reading_fcn_file_state;

	  // FIXME -- we shouldn't need both the
	  // command_history object and the
	  // Vsaving_history variable...
	  command_history::ignore_entries ();

	  unwind_protect::add (restore_command_history, 0);

	  unwind_protect_bool (Vsaving_history);
	  unwind_protect_bool (reading_script_file);

	  Vsaving_history = false;
	  reading_script_file = true;

	  octave_user_script *script = new octave_user_script (ff, ff, "");
	  octave_call_stack::push (script);
	  unwind_protect::add (octave_call_stack::unwind_pop_script, 0);

	  parse_and_execute (ffile);
	}
    }
  else
    error ("no such file, `%s'", ff.c_str ());

  unwind_protect::run_frame ("parse_fcn_file");

  return fcn_ptr;
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

  unwind_protect::begin_frame ("load_fcn_from_file");

  std::string nm = file_name;

  size_t nm_len = nm.length ();

  std::string file;

  unwind_protect_bool (fcn_file_from_relative_lookup);

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
      unwind_protect_bool (autoloading);
      autoloading = true;
    }

  if (! file.empty ())
    {
      fcn_file_from_relative_lookup = ! octave_env::absolute_pathname (file);

      file = octave_env::make_absolute (file, octave_env::getcwd ());
    }

  int len = file.length ();

  if (len > 4 && file.substr (len-4, len-1) == ".oct")
    {
      if (autoload && ! fcn_name.empty ())
	nm = fcn_name;

      retval = octave_dynamic_loader::load_oct (nm, file, fcn_file_from_relative_lookup);
    }
  else if (len > 4 && file.substr (len-4, len-1) == ".mex")
    retval = octave_dynamic_loader::load_mex (nm, file, fcn_file_from_relative_lookup);
  else if (len > 2)
    {
      if (is_function_file (file))
	{
	  // These are needed by yyparse.

	  unwind_protect_str (curr_fcn_file_name);
	  unwind_protect_str (curr_fcn_file_full_name);

	  curr_fcn_file_name = nm;
	  curr_fcn_file_full_name = file;

	  retval = parse_fcn_file (file, dispatch_type, false, autoloading);
	}
      else
	retval = new octave_user_script (file, fcn_name);
    }

  if (retval)
    retval->stash_dir_name (dir_name);

  unwind_protect::run_frame ("load_fcn_from_file");

  return retval;
}

DEFCMD (autoload, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} autoload (@var{function}, @var{file})\n\
Define @var{function} to autoload from @var{file}.\n\
\n\
The second argument, @var{file}, should be an absolute file name or\n\
a file name in the same directory as the function or script from which\n\
the autoload command was run. @var{file} should not depend on the\n\
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
will load the function @code{foo} from the file @code{bar.oct}. The above\n\
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
      Cell func_names (dim_vector (autoload_map.size ()), 1);
      Cell file_names (dim_vector (autoload_map.size ()), 1);

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
	      octave_function *fcn = 
		octave_call_stack::caller_user_script_or_function ();
	      bool found = false;
	      if (fcn)
		{
		  std::string fname = fcn->fcn_file_name ();
		  if (! fname.empty ())
		    {
		      fname = octave_env::make_absolute (fname,
			octave_env::getcwd ());
		      fname = fname.substr (0, 
			fname.find_last_of (file_ops::dir_sep_str) + 1);
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
source_file (const std::string& file_name, const std::string& context)
{
  std::string file_full_name = file_ops::tilde_expand (file_name);

  unwind_protect::begin_frame ("source_file");

  unwind_protect_str (curr_fcn_file_name);
  unwind_protect_str (curr_fcn_file_full_name);

  curr_fcn_file_name = file_name;
  curr_fcn_file_full_name = file_full_name;

  if (! context.empty ())
    {
      if (context == "caller")
	symbol_table::push_scope (symbol_table::current_caller_scope ());
      else if (context == "base")
	symbol_table::push_scope (symbol_table::top_scope ());
      else
	error ("source: context must be \"caller\" or \"base\"");

      if (! error_state)
	unwind_protect::add (symbol_table::pop_scope);
    }      

  if (! error_state)
    {
      parse_fcn_file (file_full_name, "", true, true);

      if (error_state)
	error ("source: error sourcing file `%s'",
	       file_full_name.c_str ());
    }

  unwind_protect::run_frame ("source_file");
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

  octave_function *fcn = octave_call_stack::caller_user_script_or_function ();

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
      size_t dpos = fname.rfind (file_ops::dir_sep_char);
      size_t epos = fname.rfind ('.');

      if (epos <= dpos)
        epos = NPOS;

      fname = (epos != NPOS) ? fname.substr (0, epos) : fname;

      if (arg == "fullpath")
	retval = fname;
      else
        retval = (dpos != NPOS) ? fname.substr (dpos+1) : fname;
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
    error ("feval: function `%s' not found", name.c_str ());

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
  int tmp_nargin = args.length () - 1;

  octave_value_list retval (tmp_nargin, octave_value ());

  for (int i = 0; i < tmp_nargin; i++)
    retval(i) = args(i+1);

  string_vector arg_names = args.name_tags ();

  if (! arg_names.empty ())
    {
      // tmp_nargin and arg_names.length () - 1 may differ if
      // we are passed all_va_args.

      int n = arg_names.length () - 1;

      int len = n > tmp_nargin ? tmp_nargin : n;

      string_vector tmp_arg_names (len);

      for (int i = 0; i < len; i++)
	tmp_arg_names(i) = arg_names(i+1);

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
      else
	{
	  octave_function *fcn = f_arg.function_value ();

	  if (fcn)
	    {
	      octave_value_list tmp_args = get_feval_args (args);

	      retval = feval (fcn, tmp_args, nargout);
	    }
	}
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

  unwind_protect::begin_frame ("eval_string");

  unwind_protect_bool (get_input_from_eval_string);
  unwind_protect_bool (input_from_eval_string_pending);
  unwind_protect_bool (parser_end_of_input);
  unwind_protect_bool (line_editing);
  unwind_protect_str (current_eval_string);

  get_input_from_eval_string = true;
  input_from_eval_string_pending = true;
  parser_end_of_input = false;
  line_editing = false;

  current_eval_string = s;

  unwind_protect_ptr (global_command);

  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer (0);

  unwind_protect::add (restore_input_buffer, old_buf);
  unwind_protect::add (delete_input_buffer, new_buf);

  switch_to_buffer (new_buf);

  do
    {
      reset_parser ();

      parse_status = yyparse ();

      tree_statement_list *command = global_command;

      if (parse_status == 0)
        {
	  if (command)
	    {
	      retval = command->eval (silent, nargout);

	      delete command;

	      command = 0;

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

  unwind_protect::run_frame ("eval_string");

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
string is evaluated, as the following example shows.\n\
\n\
@example\n\
eval ('error (\"This is a bad example\");',\n\
      'printf (\"This error occurred:\\n%s\", lasterr ());');\n\
     @print{} This error occurred:\n\
        error: This is a bad example\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      unwind_protect::begin_frame ("Feval");

      if (nargin > 1)
	{
	  unwind_protect_int (buffer_error_messages);
	  buffer_error_messages++;
	}

      int parse_status = 0;

      octave_value_list tmp = eval_string (args(0), nargout > 0,
					   parse_status, nargout);

      if (nargout > 0)
	retval = tmp;

      if (nargin > 1 && (parse_status != 0 || error_state))
	{
	  error_state = 0;

	  // Set up for letting the user print any messages from
	  // errors that occurred in the first part of this eval().

	  buffer_error_messages--;

	  eval_string (args(1), 0, parse_status, nargout);

	  retval = octave_value_list ();
	}

      unwind_protect::run_frame ("Feval");
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
	  symbol_table::scope_id scope = -1;

	  if (context == "caller")
	    {
	      if (symbol_table::current_scope () == symbol_table::current_caller_scope ())
		{
		  error ("assignin: assignment in caller not implemented yet for direct recursion");
		  return retval;
		}
	      else
		scope = symbol_table::current_caller_scope ();
	    }
	  else if (context == "base")
	    scope = symbol_table::top_scope ();
	  else
	    error ("assignin: context must be \"caller\" or \"base\"");

	  if (! error_state)
	    {
	      std::string nm = args(1).string_value ();

	      if (! error_state)
		{
		  if (valid_identifier (nm))
		    symbol_table::varref (nm, scope) = args(2);
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
	  unwind_protect::begin_frame ("Fevalin");

	  if (context == "caller")
	    {
	      if (symbol_table::current_scope () == symbol_table::current_caller_scope ())
		{
		  error ("evalin: evaluation in caller not implemented yet for direct recursion");
		  return retval;
		}
	      else
		symbol_table::push_scope (symbol_table::current_caller_scope ());
	    }
	  else if (context == "base")
	    symbol_table::push_scope (symbol_table::top_scope ());
	  else
	    error ("evalin: context must be \"caller\" or \"base\"");

	  if (! error_state)
	    {
	      unwind_protect::add (symbol_table::pop_scope);

	      if (nargin > 2)
	        {
		  unwind_protect_int (buffer_error_messages);
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

		  eval_string (args(2), 0, parse_status, nargout);

		  retval = octave_value_list ();
		}
	    }

	  unwind_protect::run_frame ("Fevalin");
	}
      else
        error ("evalin: expecting string as first argument");
    }
  else
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: text ***
;;; End: ***
*/
