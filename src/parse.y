/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// Parser for Octave.

// C decarations.

%{
#define YYDEBUG 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef YYBYACC
#include <cstdlib>
#endif

#include <strstream.h>

#include "Matrix.h"

#include "defun.h"
#include "error.h"
#include "input.h"
#include "lex.h"
#include "oct-hist.h"
#include "ov-usr-fcn.h"
#include "toplev.h"
#include "pager.h"
#include "parse.h"
#include "pt-arg-list.h"
#include "pt-assign.h"
#include "pt-base.h"
#include "pt-binop.h"
#include "pt-cmd.h"
#include "pt-colon.h"
#include "pt-const.h"
#include "pt-decl.h"
#include "pt-except.h"
#include "pt-exp.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-indir.h"
#include "pt-jump.h"
#include "pt-loop.h"
#include "pt-mat.h"
#include "pt-misc.h"
#include "pt-plot.h"
#include "pt-pr-code.h"
#include "pt-select.h"
#include "pt-stmt.h"
#include "pt-unop.h"
#include "pt-pr-code.h"
#include "pt-walk.h"
#include "symtab.h"
#include "token.h"
#include "utils.h"
#include "variables.h"

// If TRUE, generate a warning for the assignment in things like
//
//   octave> if (a = 2 < n)
//
// but not
//
//   octave> if ((a = 2) < n)
//
static bool Vwarn_assign_as_truth_value;

// If TRUE, generate a warning for variable swich labels.
static bool Vwarn_variable_switch_label;

// If TRUE, generate warning if declared function name disagrees with
// the name of the file in which it is defined.
static bool Vwarn_function_name_clash;

// If TRUE, generate warning if a statement in a function is not
// terminated with a semicolon.  Useful for checking functions that
// should only produce output using explicit printing statements.
static bool Vwarn_missing_semicolon;

// Temporary symbol table pointer used to cope with bogus function syntax.
symbol_table *tmp_local_sym_tab = 0;

// The current input line number.
int input_line_number = 0;

// The column of the current token.
int current_input_column = 1;

// Buffer for help text snagged from function files.
string help_buf;

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

// Create a plot command.
static tree_plot_command *
make_plot_command (token *tok, plot_limits *range, subplot_list *list);

// Finish building a range.
static tree_expression *
finish_colon_expression (tree_colon_expression *e);

// Build a constant.
static tree_constant *
make_constant (int op, token *tok_val);

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
		     tree_statement_list *cleanup, token *end_tok);

// Build a try-catch command.
static tree_command *
make_try_command (token *try_tok, tree_statement_list *body,
		  tree_statement_list *cleanup, token *end_tok);

// Build a while command.
static tree_command *
make_while_command (token *while_tok, tree_expression *expr,
		    tree_statement_list *body, token *end_tok);

// Build a for command.
static tree_command *
make_for_command (token *for_tok, tree_argument_list *lhs,
		  tree_expression *expr, tree_statement_list *body,
		  token *end_tok);

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
finish_if_command (token *if_tok, tree_if_command_list *list, token *end_tok);

// Build an elseif clause.
static tree_if_clause *
make_elseif_clause (tree_expression *expr, tree_statement_list *list);

// Finish a switch command.
static tree_switch_command *
finish_switch_command (token *switch_tok, tree_expression *expr,
		       tree_switch_case_list *list, token *end_tok);

// Build a switch case.
static tree_switch_case *
make_switch_case (tree_expression *expr, tree_statement_list *list);

// Build an assignment to a variable.
static tree_expression *
make_assign_op (int op, tree_argument_list *lhs, token *eq_tok,
		tree_expression *rhs);

// Begin defining a function.
static octave_user_function *
start_function (tree_parameter_list *param_list, tree_statement_list *body);

// Do most of the work for defining a function.
static octave_user_function *
frob_function (tree_identifier *id, octave_user_function *fcn);

// Finish defining a function.
static octave_user_function *
finish_function (tree_identifier *id, octave_user_function *fcn);

// Finish defining a function a different way.
static octave_user_function *
finish_function (tree_parameter_list *ret_list, octave_user_function *fcn);

// Reset state after parsing function.
static void
recover_from_parsing_function (void);

// Make an index expression.
static tree_index_expression *
make_index_expression (tree_expression *expr, tree_argument_list *args);

// Make an indirect reference expression.
static tree_indirect_ref *
make_indirect_ref (tree_expression *expr, const string&);

// Make a declaration command.
static tree_decl_command *
make_decl_command (int tok, token *tok_val, tree_decl_init_list *lst);

// Finish building a matrix list.
static tree_expression *
finish_matrix (tree_matrix *m);

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
      if (interactive || forced_interactive) \
	YYACCEPT; \
      else \
	YYABORT; \
    } \
  while (0)

%}

// Bison declarations.

%union
{
  // The type of the basic tokens returned by the lexer.
  token *tok_val;

  // Types for the nonterminals we generate.
  char sep_type;
  tree *tree_type;
  tree_matrix *tree_matrix_type;
  tree_expression *tree_expression_type;
  tree_constant *tree_constant_type;
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
  tree_plot_command *tree_plot_command_type;
  subplot *subplot_type;
  subplot_list *subplot_list_type;
  plot_limits *plot_limits_type;
  plot_range *plot_range_type;
  subplot_using *subplot_using_type;
  subplot_style *subplot_style_type;
  octave_user_function *octave_user_function_type;
}

// Tokens with line and column information.
%token <tok_val> '=' ':' '-' '+' '*' '/'
%token <tok_val> ADD_EQ SUB_EQ MUL_EQ DIV_EQ EMUL_EQ EDIV_EQ AND_EQ OR_EQ
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
%token <tok_val> PLOT
%token <tok_val> TEXT STYLE
%token <tok_val> FOR WHILE
%token <tok_val> IF ELSEIF ELSE
%token <tok_val> SWITCH CASE OTHERWISE
%token <tok_val> BREAK CONTINUE FUNC_RET
%token <tok_val> UNWIND CLEANUP
%token <tok_val> TRY CATCH
%token <tok_val> GLOBAL STATIC

// Other tokens.
%token END_OF_INPUT LEXICAL_ERROR
%token FCN ELLIPSIS ALL_VA_ARGS
%token USING TITLE WITH COLON OPEN_BRACE CLOSE_BRACE CLEAR

// Nonterminals we construct.
%type <sep_type> sep_no_nl opt_sep_no_nl sep opt_sep
%type <tree_type> input
%type <tree_constant_type> constant magic_colon
%type <tree_matrix_type> rows rows1
%type <tree_expression_type> title matrix
%type <tree_expression_type> primary_expr postfix_expr prefix_expr binary_expr
%type <tree_expression_type> simple_expr colon_expr assign_expr expression
%type <tree_identifier_type> identifier
%type <octave_user_function_type> function1 function2 function3
%type <tree_index_expression_type> word_list_cmd
%type <tree_colon_expression_type> colon_expr1
%type <tree_argument_list_type> arg_list word_list assign_lhs matrix_row
%type <tree_parameter_list_type> param_list param_list1
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
%type <tree_statement_list_type> opt_list input1 function4
%type <tree_plot_command_type> plot_command 
%type <subplot_type> plot_command2 plot_options
%type <subplot_list_type> plot_command1
%type <plot_limits_type> ranges
%type <plot_range_type> ranges1 
%type <subplot_using_type> using using1 
%type <subplot_style_type> style

// Precedence and associativity.
%left ';' ',' '\n'
%right '=' ADD_EQ SUB_EQ MUL_EQ DIV_EQ EMUL_EQ EDIV_EQ OR_EQ AND_EQ LSHIFT_EQ RSHIFT_EQ
%left EXPR_AND_AND EXPR_OR_OR
%left EXPR_AND EXPR_OR
%left EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%left LSHIFT RSHIFT
%left ':'
%left '-' '+' EPLUS EMINUS
%left '*' '/' LEFTDIV EMUL EDIV ELEFTDIV
%left QUOTE TRANSPOSE
%left UNARY PLUS_PLUS MINUS_MINUS EXPR_NOT
%right POW EPOW
%left '(' '.'

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
		| END_OF_INPUT
		  {
		    global_command = 0;
		    promptflag = 1;
		    YYABORT;
		  }
		| simple_list parse_error
		  { ABORT_PARSE; }
		| parse_error
		  { ABORT_PARSE; }
		;

input1		: '\n'
		  { $$ = 0; }
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
		  { $$ = new tree_statement ($1); }
		| command
		  { $$ = new tree_statement ($1); }
		| PLOT CLEAR
		  {
		    symbol_record *sr = lookup_by_name ("clearplot", 0);
		    tree_identifier *id = new tree_identifier (sr);
		    $$ = new tree_statement (id);
		  }
		;

// ===========
// Expressions
// ===========

identifier	: NAME
		  {
		    $$ = new tree_identifier
		      ($1->sym_rec (), $1->line (), $1->column ());
		  }
		;

constant	: NUM
		  { $$ = make_constant (NUM, $1); }
		| IMAG_NUM
		  { $$ = make_constant (IMAG_NUM, $1); }
		| TEXT
		  { $$ = make_constant (TEXT, $1); }
		;

matrix		: '[' ']'
		  { $$ = new tree_constant (octave_value (Matrix ())); }
		| '[' ';' ']'
		  { $$ = new tree_constant (octave_value (Matrix ())); }
		| '[' rows ']'
		  { $$ = finish_matrix ($2); }
		;

rows		: rows1
		  { $$ = $1; }
		| rows1 ';'	// Ignore trailing semicolon.
		  { $$ = $1; }
		;

rows1		: matrix_row
		  { $$ = new tree_matrix ($1); }
		| rows1 ';' matrix_row
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
		;

matrix_row	: arg_list
		  { $$ = $1; }
		| arg_list ','	// Ignore trailing comma.
		  { $$ = $1; }
		;

primary_expr	: identifier
		  { $$ = $1; }
		| constant
		  { $$ = $1; }
		| matrix
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
		| ALL_VA_ARGS
		  {
		    octave_value tmp (octave_value::all_va_args_t);
		    tree_constant *all_va_args = new tree_constant (tmp);
		    $$ = new tree_argument_list (all_va_args);
		  }
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
		| arg_list ',' ALL_VA_ARGS
		  {
		    octave_value tmp (octave_value::all_va_args_t);
		    tree_constant *all_va_args = new tree_constant (tmp);
		    $1->append (all_va_args);
		    $$ = $1;
		  }
		;

parsing_indir	: // empty
		  { lexer_flags.looking_at_indirect_ref = true; }
		;

postfix_expr	: primary_expr
		  { $$ = $1; }
		| postfix_expr '(' ')'
		  { $$ = make_index_expression ($1, 0); }
		| postfix_expr '(' arg_list ')'
		  { $$ = make_index_expression ($1, $3); }
		| postfix_expr PLUS_PLUS
		  { $$ = make_postfix_op (PLUS_PLUS, $1, $2); }
		| postfix_expr MINUS_MINUS
		  { $$ = make_postfix_op (MINUS_MINUS, $1, $2); }
		| postfix_expr QUOTE
		  { $$ = make_postfix_op (QUOTE, $1, $2); }
		| postfix_expr TRANSPOSE
		  { $$ = make_postfix_op (TRANSPOSE, $1, $2); }
		| postfix_expr '.' parsing_indir STRUCT_ELT
		  { $$ = make_indirect_ref ($1, $4->text ()); }
		;

prefix_expr	: postfix_expr
		  { $$ = $1; }
		| PLUS_PLUS prefix_expr %prec UNARY
		  { $$ = make_prefix_op (PLUS_PLUS, $2, $1); }
		| MINUS_MINUS prefix_expr %prec UNARY
		  { $$ = make_prefix_op (MINUS_MINUS, $2, $1); }
		| EXPR_NOT prefix_expr %prec UNARY
		  { $$ = make_prefix_op (EXPR_NOT, $2, $1); }
		| '+' prefix_expr %prec UNARY
		  { $$ = $2; }
		| '-' prefix_expr %prec UNARY
		  { $$ = make_prefix_op ('-', $2, $1); }
		;

binary_expr	: prefix_expr
		  { $$ = $1; }
		| binary_expr POW binary_expr
		  { $$ = make_binary_op (POW, $1, $2, $3); }
		| binary_expr EPOW binary_expr
		  { $$ = make_binary_op (EPOW, $1, $2, $3); }
		| binary_expr '+' binary_expr
		  { $$ = make_binary_op ('+', $1, $2, $3); }
		| binary_expr '-' binary_expr
		  { $$ = make_binary_op ('-', $1, $2, $3); }
		| binary_expr '*' binary_expr
		  { $$ = make_binary_op ('*', $1, $2, $3); }
		| binary_expr '/' binary_expr
		  { $$ = make_binary_op ('/', $1, $2, $3); }
		| binary_expr EPLUS binary_expr
		  { $$ = make_binary_op ('+', $1, $2, $3); }
		| binary_expr EMINUS binary_expr
		  { $$ = make_binary_op ('-', $1, $2, $3); }
		| binary_expr EMUL binary_expr
		  { $$ = make_binary_op (EMUL, $1, $2, $3); }
		| binary_expr EDIV binary_expr
		  { $$ = make_binary_op (EDIV, $1, $2, $3); }
		| binary_expr LEFTDIV binary_expr
		  { $$ = make_binary_op (LEFTDIV, $1, $2, $3); }
		| binary_expr ELEFTDIV binary_expr
		  { $$ = make_binary_op (ELEFTDIV, $1, $2, $3); }
		;

colon_expr	: colon_expr1
		  { $$ = finish_colon_expression ($1); }
		;

colon_expr1	: binary_expr
		  { $$ = new tree_colon_expression ($1); }
		| colon_expr1 ':' binary_expr
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
		  { $$ = new tree_argument_list ($1); }
		| '[' arg_list CLOSE_BRACE
		  { $$ = $2; }
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
		| assign_lhs LSHIFT_EQ expression
		  { $$ = make_assign_op (LSHIFT_EQ, $1, $2, $3); }
		| assign_lhs RSHIFT_EQ expression
		  { $$ = make_assign_op (RSHIFT_EQ, $1, $2, $3); }
		| assign_lhs EMUL_EQ expression
		  { $$ = make_assign_op (EMUL_EQ, $1, $2, $3); }
		| assign_lhs EDIV_EQ expression
		  { $$ = make_assign_op (EDIV_EQ, $1, $2, $3); }
		| assign_lhs AND_EQ expression
		  { $$ = make_assign_op (AND_EQ, $1, $2, $3); }
		| assign_lhs OR_EQ expression
		  { $$ = make_assign_op (OR_EQ, $1, $2, $3); }
		;

word_list_cmd	: identifier word_list
		  { $$ = make_index_expression ($1, $2); }
		;

word_list	: TEXT
		  {
		    tree_constant *tmp = make_constant (TEXT, $1);
		    $$ = new tree_argument_list (tmp);
		  }
		| word_list TEXT
		  {
		    tree_constant *tmp = make_constant (TEXT, $2);
		    $1->append (tmp);
		    $$ = $1;
		  }
		;

expression	: simple_expr
		  { $$ = $1; }
		| word_list_cmd
		  { $$ = $1; }
		| assign_expr
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
		| plot_command
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

if_command	: IF if_cmd_list END
		  {
		    if (! ($$ = finish_if_command ($1, $2, $3)))
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

elseif_clause	: ELSEIF opt_sep expression opt_sep opt_list
		  { $$ = make_elseif_clause ($3, $5); }
		;

else_clause	: ELSE opt_sep opt_list
		  { $$ = new tree_if_clause ($3); }
		;

// ================
// Switch statement
// ================

switch_command	: SWITCH expression opt_sep case_list END
		  {
		    if (! ($$ = finish_switch_command ($1, $2, $4, $5)))
		      ABORT_PARSE;
		  }
		;

case_list	: case_list1
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

switch_case	: CASE opt_sep expression opt_sep list
		  { $$ = make_switch_case ($3, $5); }
		;

default_case	: OTHERWISE opt_sep opt_list
		  { $$ = new tree_switch_case ($3); }
		;

// =======
// Looping
// =======

loop_command	: WHILE expression opt_sep opt_list END
		  {
		    if (! ($$ = make_while_command ($1, $2, $4, $5)))
		      ABORT_PARSE;
		  }
		| FOR assign_lhs '=' expression opt_sep opt_list END
		  {
		    if (! ($$ = make_for_command ($1, $2, $4, $6, $7)))
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

except_command	: UNWIND opt_sep opt_list CLEANUP opt_sep opt_list END
		  {
		    if (! ($$ = make_unwind_command ($1, $3, $6, $7)))
		      ABORT_PARSE;
		  }
		| TRY opt_sep opt_list CATCH opt_sep opt_list END
		  {
		    if (! ($$ = make_try_command ($1, $3, $6, $7)))
		      ABORT_PARSE;
		  }
		;

// ===========================================
// Some `subroutines' for function definitions
// ===========================================

global_symtab	: // empty
		  { curr_sym_tab = global_sym_tab; }
		;

local_symtab	: // empty
		  { curr_sym_tab = tmp_local_sym_tab; }
		;

in_return_list	: // empty
		  { lexer_flags.looking_at_return_list = true; }
		;

parsed_fcn_name	: // empty
		  { lexer_flags.parsed_function_name = true; }
		;

// ===========================
// List of function parameters
// ===========================

param_list_beg	: '('
		  { lexer_flags.looking_at_parameter_list = true; }
		;

param_list_end	: ')'
		  { lexer_flags.looking_at_parameter_list = false; }
		;

param_list	: param_list_beg param_list_end
		  {
		    lexer_flags.quote_is_transpose = false;
		    $$ = 0;
		  }
		| param_list_beg ELLIPSIS param_list_end
		  {
		    lexer_flags.quote_is_transpose = false;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| param_list1 param_list_end
		  {
		    lexer_flags.quote_is_transpose = false;
		    $1->mark_as_formal_parameters ();
		    $$ = $1;
		  }
		| param_list1 ',' ELLIPSIS param_list_end
		  {
		    lexer_flags.quote_is_transpose = false;
		    $1->mark_as_formal_parameters ();
		    $1->mark_varargs ();
		    $$ = $1;
		  }
		;

param_list1	: param_list_beg identifier
		  { $$ = new tree_parameter_list ($2); }
		| param_list1 ',' identifier
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
		| param_list_beg error
		  {
		    yyerror ("invalid parameter list");
		    $$ = 0;
		    ABORT_PARSE;
		  }
		| param_list1 ',' error
		  {
		    yyerror ("invalid parameter list");
		    $$ = 0;
		    ABORT_PARSE;
		  }
		;

// ===================================
// List of function return value names
// ===================================

return_list_beg	: '[' local_symtab in_return_list
		;

return_list	: return_list_beg return_list_end
		  {
		    lexer_flags.looking_at_return_list = false;
		    $$ = new tree_parameter_list ();
		  }
		| return_list_beg ELLIPSIS return_list_end
		  {
		    lexer_flags.looking_at_return_list = false;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| return_list_beg return_list1 return_list_end
		  {
		    lexer_flags.looking_at_return_list = false;
		    $$ = $2;
		  }
		| return_list_beg return_list1 ',' ELLIPSIS return_list_end
		  {
		    lexer_flags.looking_at_return_list = false;
		    $2->mark_varargs ();
		    $$ = $2;
		  }
		;

return_list1	: identifier
		  { $$ = new tree_parameter_list ($1); }
		| return_list1 ',' identifier
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
		;

return_list_end	: global_symtab ']'
		;

// ===================
// Function definition
// ===================

function_beg	: FCN global_symtab
		;

function	: function_beg function2
		  {
		    recover_from_parsing_function ();
		    $$ = 0;
		  }
		| function_beg identifier function1
		  {
		    finish_function ($2, $3);
		    recover_from_parsing_function ();
		    $$ = 0;
		  }
		| function_beg return_list function1
		  {
		    finish_function ($2, $3);
		    recover_from_parsing_function ();
		    $$ = 0;
		  }
		;

function1	: global_symtab '=' function2
		  { $$ = $3; }
		;

function2	: identifier local_symtab parsed_fcn_name function3
		  {
		    if (! ($$ = frob_function ($1, $4)))
		      ABORT_PARSE;
		  }
		;

function3	: param_list function4
		  { $$ = start_function ($1, $2); }
		| function4
		  { $$ = start_function (0, $1); }
		;

function4	: opt_sep opt_list function_end
		  { $$ = $2; }
		;

function_end	: END
		  {
		    if (end_token_ok ($1, token::function_end))
		      {
			if (reading_fcn_file)
			  check_for_garbage_after_fcn_def ();
		      }
		    else
		      ABORT_PARSE;
		  }
		| END_OF_INPUT
		  {
		    if (! (reading_fcn_file || reading_script_file))
		      YYABORT;
		  }
		;

// ========
// Plotting
// ========

plot_command	: PLOT plot_command1
		  {
		    if (! ($$ = make_plot_command ($1, 0, $2)))
		      ABORT_PARSE;
		  }
		| PLOT ranges plot_command1
		  {
		    if (! ($$ = make_plot_command ($1, $2, $3)))
		      ABORT_PARSE;
		  }
		;

ranges		: ranges1
		  { $$ = new plot_limits ($1); }
		| ranges1 ranges1
		  { $$ = new plot_limits ($1, $2); }
		| ranges1 ranges1 ranges1
		  { $$ = new plot_limits ($1, $2, $3); }
		;

ranges1		: OPEN_BRACE expression COLON expression CLOSE_BRACE
		  { $$ = new plot_range ($2, $4); }
		| OPEN_BRACE COLON expression CLOSE_BRACE
		  { $$ = new plot_range (0, $3); }
		| OPEN_BRACE expression COLON CLOSE_BRACE
		  { $$ = new plot_range ($2, 0); }
		| OPEN_BRACE COLON CLOSE_BRACE
		  { $$ = new plot_range (); }
		| OPEN_BRACE CLOSE_BRACE
		  { $$ = new plot_range (); }
		;

plot_command1	: // empty
		  { $$ = 0; }
		| plot_command2
		  { $$ = new subplot_list ($1); }
		| plot_command1 ',' plot_command2
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
		;

plot_command2	: expression
		  { $$ = new subplot ($1); }
		| expression plot_options
		  { $$ = $2->set_data ($1); }
		;

plot_options	: using
		  { $$ = new subplot ($1, 0, 0); }
		| title
		  { $$ = new subplot (0, $1, 0); }
		| style
		  { $$ = new subplot (0, 0, $1); }
		| using title
		  { $$ = new subplot ($1, $2, 0); }
		| title using		 
		  { $$ = new subplot ($2, $1, 0); }
		| using style		 
		  { $$ = new subplot ($1, 0, $2); }
		| style using		 
		  { $$ = new subplot ($2, 0, $1); }
		| title style		 
		  { $$ = new subplot (0, $1, $2); }
		| style title		 
		  { $$ = new subplot (0, $2, $1); }
		| using title style	 
		  { $$ = new subplot ($1, $2, $3); }
		| using style title	 
		  { $$ = new subplot ($1, $3, $2); }
		| title using style	 
		  { $$ = new subplot ($2, $1, $3); }
		| title style using	 
		  { $$ = new subplot ($3, $1, $2); }
		| style using title	 
		  { $$ = new subplot ($2, $3, $1); }
		| style title using	 
		  { $$ = new subplot ($3, $2, $1); }
		;

using		: using1
		  {
		    lexer_flags.in_plot_using = false;
		    $$ = $1;
		  }
		| using1 expression
		  {
		    lexer_flags.in_plot_using = false;
		    $$ = $1->set_format ($2);
		  }
		;

using1		: USING expression
		  {
		    subplot_using *tmp = new subplot_using ();
		    $$ = tmp->add_qualifier ($2);
		  }
		| using1 COLON expression
		  { $$ = $1->add_qualifier ($3); }
		;

title		: TITLE expression
		  { $$ = $2; }
		;

style		: WITH STYLE
		  { $$ = new subplot_style ($2->text ()); }
		| WITH STYLE expression
		  { $$ = new subplot_style ($2->text (), $3); }
		| WITH STYLE expression expression
		  { $$ = new subplot_style ($2->text (), $3, $4); }
		;

// =============
// Miscellaneous
// =============

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

  ostrstream output_buf;

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

  output_buf << "\n" << ends;

  char *msg = output_buf.str ();

  parse_error ("%s", msg);

  delete [] msg;
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

    case token::while_end:
      error (fmt, type, "endwhile", l, c); 
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
  if (Vwarn_assign_as_truth_value
      && expr->is_assignment_expression ()
      && expr->paren_count () < 2)
    {
      warning ("suggest parenthesis around assignment used as truth value");
    }
}

// Maybe print a warning about switch labels that aren't constants.

static void
maybe_warn_variable_switch_label (tree_expression *expr)
{
  if (Vwarn_variable_switch_label && ! expr->is_constant ())
    {
      warning ("variable switch label");
    }
}

// Create a plot command.

static tree_plot_command *
make_plot_command (token *tok, plot_limits *range, subplot_list *list)
{
  if (range)
    {
      if (tok->pttype () == token::replot)
	{
	  yyerror ("cannot specify new ranges with replot");
	  return 0;
	}
    }
  else if (! list && tok->pttype () != token::replot)
    {
      yyerror ("must have something to plot");
      return 0;
    }

  lexer_flags.plotting = false;
  lexer_flags.past_plot_range = false;
  lexer_flags.in_plot_range = false;
  lexer_flags.in_plot_using = false;
  lexer_flags.in_plot_style = false;
  
  return new tree_plot_command (list, range, tok->pttype ());
}

static tree_expression *
fold (tree_binary_expression *e)
{
  tree_expression *retval = 0;

  tree_expression *op1 = e->lhs ();
  tree_expression *op2 = e->rhs ();

  if (op1->is_constant () && op2->is_constant ())
    {
      octave_value tmp = e->rvalue ();

      if (! error_state)
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  ostrstream buf;

	  tree_print_code tpc (buf);

	  e->accept (tpc);

	  buf << ends;

	  char *s = buf.str ();

	  tc_retval->stash_original_text (s);

	  delete [] s;

	  delete e;

	  retval = tc_retval;
	}
      else
	delete e;
    }
  else
    retval = e;

  return retval;
}

// Finish building a range.

static tree_expression *
finish_colon_expression (tree_colon_expression *e)
{
  tree_expression *retval = 0;

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

	      if (! error_state)
		{
		  tree_constant *tc_retval = new tree_constant (tmp);

		  ostrstream buf;

		  tree_print_code tpc (buf);

		  e->accept (tpc);

		  buf << ends;

		  char *s = buf.str ();

		  tc_retval->stash_original_text (s);

		  delete [] s;

		  delete e;

		  retval = tc_retval;
		}
	      else
		delete e;
	    }
	  else
	    retval = e;
	}
      else
	{
	  // XXX FIXME XXX -- need to delete this without deleting base too.
	  // delete e;

	  // XXX FIXME XXX -- need to attempt constant folding here
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

  tree_constant *retval;

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

    case TEXT:
      {
	octave_value tmp (tok_val->text ());
	retval = new tree_constant (tmp, l, c);
      }
      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
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
      t = octave_value::pow;
      break;

    case EPOW:
      t = octave_value::el_pow;
      break;

    case '+':
      t = octave_value::add;
      break;

    case '-':
      t = octave_value::sub;
      break;

    case '*':
      t = octave_value::mul;
      break;

    case '/':
      t = octave_value::div;
      break;

    case EMUL:
      t = octave_value::el_mul;
      break;

    case EDIV:
      t = octave_value::el_div;
      break;

    case LEFTDIV:
      t = octave_value::ldiv;
      break;

    case ELEFTDIV:
      t = octave_value::el_ldiv;
      break;

    case LSHIFT:
      t = octave_value::lshift;
      break;

    case RSHIFT:
      t = octave_value::rshift;
      break;

    case EXPR_LT:
      t = octave_value::lt;
      break;

    case EXPR_LE:
      t = octave_value::le;
      break;

    case EXPR_EQ:
      t = octave_value::eq;
      break;

    case EXPR_GE:
      t = octave_value::ge;
      break;

    case EXPR_GT:
      t = octave_value::gt;
      break;

    case EXPR_NE:
      t = octave_value::ne;
      break;

    case EXPR_AND:
      t = octave_value::el_and;
      break;

    case EXPR_OR:
      t = octave_value::el_or;
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
  tree_prefix_expression::type t;

  switch (op)
    {
    case EXPR_NOT:
      t = tree_prefix_expression::unot;
      break;

    case '-':
      t = tree_prefix_expression::uminus;
      break;

    case PLUS_PLUS:
      t = tree_prefix_expression::increment;
      break;

    case MINUS_MINUS:
      t = tree_prefix_expression::decrement;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_prefix_expression (t, op1, l, c);
}

// Build a postfix expression.

static tree_expression *
make_postfix_op (int op, tree_expression *op1, token *tok_val)
{
  tree_postfix_expression::type t;

  switch (op)
    {
    case QUOTE:
      t = tree_postfix_expression::hermitian;
      break;

    case TRANSPOSE:
      t = tree_postfix_expression::transpose;
      break;

    case PLUS_PLUS:
      t = tree_postfix_expression::increment;
      break;

    case MINUS_MINUS:
      t = tree_postfix_expression::decrement;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_postfix_expression (t, op1, l, c);
}

// Build an unwind-protect command.

static tree_command *
make_unwind_command (token *unwind_tok, tree_statement_list *body,
		     tree_statement_list *cleanup, token *end_tok)
{
  tree_command *retval = 0;

  if (end_token_ok (end_tok, token::unwind_protect_end))
    {
      int l = unwind_tok->line ();
      int c = unwind_tok->column ();

      retval = new tree_unwind_protect_command (body, cleanup, l, c);
    }

  return retval;
}

// Build a try-catch command.

static tree_command *
make_try_command (token *try_tok, tree_statement_list *body,
		  tree_statement_list *cleanup, token *end_tok)
{
  tree_command *retval = 0;

  if (end_token_ok (end_tok, token::try_catch_end))
    {
      int l = try_tok->line ();
      int c = try_tok->column ();

      retval = new tree_try_catch_command (body, cleanup, l, c);
    }

  return retval;
}

// Build a while command.

static tree_command *
make_while_command (token *while_tok, tree_expression *expr,
		    tree_statement_list *body, token *end_tok)
{
  tree_command *retval = 0;

  maybe_warn_assign_as_truth_value (expr);

  if (end_token_ok (end_tok, token::while_end))
    {
      lexer_flags.looping--;

      int l = while_tok->line ();
      int c = while_tok->column ();

      retval = new tree_while_command (expr, body, l, c);
    }

  return retval;
}

// Build a for command.

static tree_command *
make_for_command (token *for_tok, tree_argument_list *lhs,
		  tree_expression *expr, tree_statement_list *body,
		  token *end_tok)
{
  tree_command *retval = 0;

  if (end_token_ok (end_tok, token::for_end))
    {
      lexer_flags.looping--;

      int l = for_tok->line ();
      int c = for_tok->column ();

      if (lhs->length () == 1)
	{
	  tree_expression *tmp = lhs->remove_front ();

	  retval = new tree_simple_for_command (tmp, expr, body, l, c);

	  delete lhs;
	}
      else
	retval = new tree_complex_for_command (lhs, expr, body, l, c);
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

  if (lexer_flags.looping || lexer_flags.defining_func || reading_script_file)
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

  if (lexer_flags.looping)
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

  if (lexer_flags.defining_func || reading_script_file)
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
		   token *end_tok)
{
  tree_if_command *retval = 0;

  if (end_token_ok (end_tok, token::if_end))
    {
      int l = if_tok->line ();
      int c = if_tok->column ();

      retval = new tree_if_command (list, l, c);
    }

  return retval;
}

// Build an elseif clause.

static tree_if_clause *
make_elseif_clause (tree_expression *expr, tree_statement_list *list)
{
  maybe_warn_assign_as_truth_value (expr);

  return new tree_if_clause (expr, list);
}

// Finish a switch command.

static tree_switch_command *
finish_switch_command (token *switch_tok, tree_expression *expr,
		       tree_switch_case_list *list, token *end_tok)
{
  tree_switch_command *retval = 0;

  if (end_token_ok (end_tok, token::switch_end))
    {
      int l = switch_tok->line ();
      int c = switch_tok->column ();

      retval = new tree_switch_command (expr, list, l, c);
    }

  return retval;
}

// Build a switch case.

static tree_switch_case *
make_switch_case (tree_expression *expr, tree_statement_list *list)
{
  maybe_warn_variable_switch_label (expr);

  return new tree_switch_case (expr, list);
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
      t = octave_value::asn_eq;
      break;

    case ADD_EQ:
      t = octave_value::add_eq;
      break;

    case SUB_EQ:
      t = octave_value::sub_eq;
      break;

    case MUL_EQ:
      t = octave_value::mul_eq;
      break;

    case DIV_EQ:
      t = octave_value::div_eq;
      break;

    case LSHIFT_EQ:
      t = octave_value::lshift_eq;
      break;

    case RSHIFT_EQ:
      t = octave_value::rshift_eq;
      break;

    case EMUL_EQ:
      t = octave_value::el_mul_eq;
      break;

    case EDIV_EQ:
      t = octave_value::el_div_eq;
      break;

    case AND_EQ:
      t = octave_value::el_and_eq;
      break;

    case OR_EQ:
      t = octave_value::el_or_eq;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = eq_tok->line ();
  int c = eq_tok->column ();

  if (lhs->length () == 1)
    {
      tree_expression *tmp = lhs->remove_front ();

      retval = new tree_simple_assignment (tmp, rhs, false, l, c, t);

      delete lhs;
    }
  else
    return new tree_multi_assignment (lhs, rhs, 0, l, c);

  return retval;
}

// Begin defining a function.

static octave_user_function *
start_function (tree_parameter_list *param_list, tree_statement_list *body)
{
  body->mark_as_function_body ();

  // We'll fill in the return list later.

  octave_user_function *fcn
    = new octave_user_function (param_list, 0, body, curr_sym_tab);

  return fcn;
}

// Do most of the work for defining a function.

static octave_user_function *
frob_function (tree_identifier *id, octave_user_function *fcn)
{
  string id_name = id->name ();

  // If input is coming from a file, issue a warning if the name of
  // the file does not match the name of the function stated in the
  // file.  Matlab doesn't provide a diagnostic (it ignores the stated
  // name).

  fcn->stash_function_name (id_name);

  if (reading_fcn_file)
    {
      if (curr_fcn_file_name != id_name)
	{
	  if (Vwarn_function_name_clash)
	    warning ("function name `%s' does not agree with function\
 file name `%s'", id_name.c_str (), curr_fcn_file_full_name.c_str ());

	  global_sym_tab->rename (id_name, curr_fcn_file_name);

	  if (error_state)
	    return 0;

	  id_name = id->name ();
	}

      fcn->stash_function_name (id_name);
      fcn->stash_fcn_file_name ();
      fcn->stash_fcn_file_time (time (0));
      fcn->mark_as_system_fcn_file ();
    }
  else if (! (input_from_tmp_history_file || input_from_startup_file)
	   && reading_script_file
	   && curr_fcn_file_name == id_name)
    {
      warning ("function `%s' defined within script file `%s'",
	       id_name.c_str (), curr_fcn_file_full_name.c_str ());
    }

  top_level_sym_tab->clear (id_name);

  symbol_record *sr = global_sym_tab->lookup (id_name);

  if (sr)
    fcn->stash_symtab_ptr (sr);
  else
    panic_impossible ();

  id->define (fcn, symbol_def::USER_FUNCTION);

  id->document (help_buf);

  return fcn;
}

// Finish defining a function.

static octave_user_function *
finish_function (tree_identifier *id, octave_user_function *fcn)
{
  tree_parameter_list *tpl = new tree_parameter_list (id);

  tpl->mark_as_formal_parameters ();

  return fcn->define_ret_list (tpl);
}

// Finish defining a function a different way.

static octave_user_function *
finish_function (tree_parameter_list *ret_list, octave_user_function *fcn)
{
  ret_list->mark_as_formal_parameters ();

  return fcn->define_ret_list (ret_list);
}

static void
recover_from_parsing_function (void)
{
  curr_sym_tab = top_level_sym_tab;

  lexer_flags.defining_func = false;
  lexer_flags.beginning_of_function = false;
  lexer_flags.parsed_function_name = false;
  lexer_flags.looking_at_return_list = false;
  lexer_flags.looking_at_parameter_list = false;
}

// Make an index expression.

static tree_index_expression *
make_index_expression (tree_expression *expr, tree_argument_list *args)
{
  tree_index_expression *retval = 0;

  int l = expr->line ();
  int c = expr->column ();

  expr->mark_postfix_indexed ();

  retval =  new tree_index_expression (expr, args, l, c);

  return retval;
}

// Make an indirect reference expression.

static tree_indirect_ref *
make_indirect_ref (tree_expression *expr, const string& elt)
{
  tree_indirect_ref *retval = 0;

  int l = expr->line ();
  int c = expr->column ();

  retval = new tree_indirect_ref (expr, elt, l, c);

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
	    warning ("ignoring static declaration near line %d of file `%s'",
		     l, curr_fcn_file_full_name.c_str ());
	  else
	    warning ("ignoring static declaration near line %d", l);
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
  tree_expression *retval = 0;

  if (m->all_elements_are_constant ())
    {
      octave_value tmp = m->rvalue ();

      if (! error_state)
	{
	  tree_constant *tc_retval = new tree_constant (tmp);

	  ostrstream buf;

	  tree_print_code tpc (buf);

	  m->accept (tpc);

	  buf << ends;

	  char *s = buf.str ();

	  tc_retval->stash_original_text (s);

	  delete [] s;

	  delete m;

	  retval = tc_retval;
	}
      else
	delete m;
    }
  else
    retval = m;

  return retval;
}

static void
maybe_warn_missing_semi (tree_statement_list *t)
{
  if (lexer_flags.defining_func && Vwarn_missing_semicolon)
    {
      tree_statement *tmp = t->rear();

      if (tmp->is_expression ())
	warning ("missing semicolon near line %d, column %d in file `%s'",
		 tmp->line (), tmp->column (),
		 curr_fcn_file_full_name.c_str ());
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
	tree_statement *tmp = list->rear ();
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

static int
warn_assign_as_truth_value (void)
{
  Vwarn_assign_as_truth_value
    = check_preference ("warn_assign_as_truth_value");

  return 0;
}

static int
warn_function_name_clash (void)
{
  Vwarn_function_name_clash = check_preference ("warn_function_name_clash");

  return 0;
}

static int
warn_missing_semicolon (void)
{
  Vwarn_missing_semicolon = check_preference ("warn_missing_semicolon");

  return 0;
}

static int
warn_variable_switch_label (void)
{
  Vwarn_variable_switch_label
    = check_preference ("warn_variable_switch_label");

  return 0;
}

void
symbols_of_parse (void)
{
  DEFVAR (warn_assign_as_truth_value, 1.0, 0, warn_assign_as_truth_value,
    "produce warning for assignments used as truth values");

  DEFVAR (warn_function_name_clash, 1.0, 0, warn_function_name_clash,
    "produce warning if function name conflicts with file name");

  DEFVAR (warn_missing_semicolon, 0.0, 0, warn_missing_semicolon,
    "produce a warning if a statement in a function file is not\n\
terminated with a semicolon");

  DEFVAR (warn_variable_switch_label, 0.0, 0, warn_variable_switch_label,
    "produce warning for variables used as switch labels");
}

/*
;;; Local Variables: ***
;;; mode: text ***
;;; End: ***
*/
