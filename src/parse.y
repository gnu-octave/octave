/*

Copyright (C) 1996 John W. Eaton

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
#include "toplev.h"
#include "pager.h"
#include "parse.h"
#include "pt-cmd.h"
#include "pt-const.h"
#include "pt-fcn.h"
#include "pt-fvc.h"
#include "pt-mat.h"
#include "pt-mvr.h"
#include "pt-exp.h"
#include "pt-misc.h"
#include "pt-plot.h"
#include "pt-pr-code.h"
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

// If TRUE, generate a warning for the comma in things like
//
//   octave> global a, b = 2
//
static bool Vwarn_comma_in_declaration;

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
static void yyerror (const char *s);

// Error mesages for mismatched end tokens.
static void end_error
	(const char *type, token::end_tok_type ettype, int l, int c);

// Check to see that end tokens are properly matched.
static int check_end (token *tok, token::end_tok_type expected);

// Try to figure out early if an expression should become an
// assignment to the built-in variable ans.
static tree_expression *maybe_convert_to_ans_assign (tree_expression *expr);

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.
static void maybe_warn_assign_as_truth_value (tree_expression *expr);

// Maybe print a warning about switch labels that aren't constants.
static void maybe_warn_variable_switch_label (tree_expression *expr);

// Create a plot command.
static tree_plot_command *make_plot_command
	 (token *tok, plot_limits *range, subplot_list *list);

// Finish building a range.
static tree_expression *finish_colon_expression (tree_colon_expression *e);

// Build a constant.
static tree_constant *make_constant (int op, token *tok_val);

// Build a binary expression.
static tree_expression *make_binary_op
	 (int op, tree_expression *op1,	token *tok_val, tree_expression *op2);

// Build a boolean expression.
static tree_expression *make_boolean_op
	 (int op, tree_expression *op1,	token *tok_val, tree_expression *op2);

// Build a prefix expression.
static tree_expression *make_prefix_op
	 (int op, tree_identifier *op1, token *tok_val);

// Build a postfix expression.
static tree_expression *make_postfix_op
	 (int op, tree_identifier *op1, token *tok_val);

// Build a binary expression.
static tree_expression *make_unary_op
	 (int op, tree_expression *op1, token *tok_val);

// Build an unwind-protect command.
static tree_command *make_unwind_command
	 (token *unwind_tok, tree_statement_list *body,
	  tree_statement_list *cleanup, token *end_tok);

// Build a try-catch command.
static tree_command *make_try_command
	 (token *try_tok, tree_statement_list *body,
	  tree_statement_list *cleanup, token *end_tok);

// Build a while command.
static tree_command *make_while_command
	 (token *while_tok, tree_expression *expr,
	  tree_statement_list *body, token *end_tok);

// Build a for command.
static tree_command *make_for_command
	 (token *for_tok, tree_index_expression *var,
	  tree_expression *expr, tree_statement_list *body,
	  token *end_tok);

// Build a for command a different way.
static tree_command *make_for_command
	 (token *for_tok, tree_matrix_row *mr, tree_expression *expr,
	  tree_statement_list *body, token *end_tok);

// Build a break command.
static tree_command *make_break_command (token *break_tok);

// Build a continue command.
static tree_command *make_continue_command (token *continue_tok);

// Build a return command.
static tree_command *make_return_command (token *return_tok);

// Start an if command.
static tree_if_command_list *start_if_command
	 (tree_expression *expr, tree_statement_list *list);

// Finish an if command.
static tree_if_command *finish_if_command
	 (token *if_tok, tree_if_command_list *list, token *end_tok);

// Build an elseif clause.
static tree_if_clause *make_elseif_clause
	 (tree_expression *expr, tree_statement_list *list);

// Finish a switch command.
static tree_switch_command *finish_switch_command
	 (token *switch_tok, tree_expression *expr,
	  tree_switch_case_list *list, token *end_tok);

// Build a switch case.
static tree_switch_case *make_switch_case
	 (tree_expression *expr, tree_statement_list *list);

// Build an assignment to a variable.
static tree_expression *make_simple_assignment
	 (tree_index_expression *var, token *eq_tok, tree_expression *expr);

// Make an expression that handles assignment of multiple values.
static tree_expression *make_multi_val_ret
	 (tree_matrix_row *mr, tree_expression *rhs, token *eq_tok);

// Begin defining a function.
static tree_function *start_function_def
	 (tree_parameter_list *param_list, tree_statement_list *body);

// Do most of the work for defining a function.
static tree_function *frob_function_def
	 (tree_identifier *id, tree_function *fcn);

// Finish defining a function.
static tree_function *finish_function_def (token *var, tree_function *fcn);

// Finish defining a function a different way.
static tree_function *finish_function_def
	 (tree_parameter_list *ret_list, tree_function *fcn);

// Make an index expression.
static tree_index_expression *make_index_expression
	 (tree_indirect_ref *indir, tree_argument_list *args);

// Make a declaration command.
static tree_decl_command *make_decl_command
	(int tok, token *tok_val, tree_decl_init_list *lst);

// Finish building a matrix list.
static tree_expression *finish_matrix (tree_matrix *m);

// Maybe print a warning.  Duh.
static void maybe_warn_missing_semi (tree_statement_list *);

// Maybe print a warning.  Duh.
static void maybe_warn_comma_in_decl (void);

// Set the print flag for a statement based on the separator type.
static void set_stmt_print_flag (tree_statement_list *, char, bool);

#define ABORT_PARSE \
  do \
    { \
      global_command = 0; \
      yyerrok; \
      if (interactive) \
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
  tree_matrix_row *tree_matrix_row_type;
  tree_expression *tree_expression_type;
  tree_constant *tree_constant_type;
  tree_identifier *tree_identifier_type;
  tree_indirect_ref *tree_indirect_ref_type;
  tree_function *tree_function_type;
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
}

// Tokens with line and column information.
%token <tok_val> '=' ':' '-' '+' '*' '/'
%token <tok_val> EXPR_AND_AND EXPR_OR_OR
%token <tok_val> EXPR_AND EXPR_OR EXPR_NOT
%token <tok_val> EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%token <tok_val> LEFTDIV EMUL EDIV ELEFTDIV EPLUS EMINUS
%token <tok_val> QUOTE TRANSPOSE
%token <tok_val> PLUS_PLUS MINUS_MINUS POW EPOW
%token <tok_val> NUM IMAG_NUM
%token <tok_val> NAME SCREW
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
%token <tok_val> TEXT_ID

// Other tokens.
%token LEXICAL_ERROR
%token FCN SCREW_TWO
%token ELLIPSIS
%token ALL_VA_ARGS
%token END_OF_INPUT
%token USING TITLE WITH COLON OPEN_BRACE CLOSE_BRACE CLEAR

// Nonterminals we construct.
%type <sep_type> sep_no_nl opt_sep_no_nl sep opt_sep
%type <tree_type> input
%type <tree_matrix_type> rows rows1
%type <tree_matrix_row_type> matrix_row matrix_row1
%type <tree_expression_type> expression simple_expr simple_expr1
%type <tree_expression_type> ans_expression title matrix
%type <tree_identifier_type> identifier
%type <tree_indirect_ref_type> indirect_ref indirect_ref1
%type <tree_function_type> func_def1 func_def2 func_def3
%type <tree_index_expression_type> variable word_list_cmd
%type <tree_colon_expression_type> colon_expr
%type <tree_argument_list_type> arg_list word_list
%type <tree_parameter_list_type> param_list param_list1
%type <tree_parameter_list_type> return_list return_list1
%type <tree_command_type> command func_def
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
%type <tree_plot_command_type> plot_command 
%type <subplot_type> plot_command2 plot_options
%type <subplot_list_type> plot_command1
%type <plot_limits_type> ranges
%type <plot_range_type> ranges1 
%type <subplot_using_type> using using1 
%type <subplot_style_type> style

// Precedence and associativity.
%left ';' ',' '\n'
%right '='
%left EXPR_AND_AND EXPR_OR_OR
%left EXPR_AND EXPR_OR
%left EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%left ':'
%left '-' '+' EPLUS EMINUS
%left '*' '/' LEFTDIV EMUL EDIV ELEFTDIV
%left QUOTE TRANSPOSE
%left UNARY PLUS_PLUS MINUS_MINUS EXPR_NOT
%right POW EPOW

// There are 18 shift/reduce conflicts, ok?  But this only works with
// bison...
// %expect 18

// Where to start.
%start input

// Grammar rules.

%%

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

parse_error	: LEXICAL_ERROR
		  { yyerror ("parse error"); }
		| error
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
		    lexer_flags.beginning_of_function = 0;
		    $$ = new tree_statement_list ($1);
		  }
		| list1 sep statement
		  {
		    set_stmt_print_flag ($1, $2, true);
		    $1->append ($3);
		    $$ = $1;
		  }
		;

statement	: command
		  { $$ = new tree_statement ($1); }
		| ans_expression
		  { $$ = new tree_statement ($1); }
		| PLOT CLEAR
		  {
		    symbol_record *sr = lookup_by_name ("clearplot", 0);
		    tree_identifier *id = new tree_identifier (sr);
		    $$ = new tree_statement (id);
		  }
		;

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
		    lexer_flags.in_plot_using = 0;
		    $$ = $1;
		  }
		| using1 expression
		  {
		    lexer_flags.in_plot_using = 0;
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

ans_expression	: expression
		  { $$ = maybe_convert_to_ans_assign ($1); }
		;

decl1		: decl2
		  { $$ = new tree_decl_init_list ($1); }
		| decl1 decl2
		  {
		    $1->append ($2);
		    $$ = $1;
		  }
		| decl1 ',' decl2
		  {
		    maybe_warn_comma_in_decl ();
		    $1->append ($3);
		    $$ = $1;
		  }
		;

decl2		: identifier
		  { $$ = new tree_decl_elt ($1); }
		| identifier '=' expression
		  {
		    tree_simple_assignment_expression *tmp_ass;
		    tmp_ass = new tree_simple_assignment_expression
		      ($1, $3, 0, 0, $2->line (), $2->column ());
		    $$ = new tree_decl_elt (tmp_ass);
		  }
		;

declaration	: GLOBAL decl1
		  { $$ = make_decl_command (GLOBAL, $1, $2); }
		| STATIC decl1
		  { $$ = make_decl_command (STATIC, $1, $2); }
		;

command		: plot_command
		  { $$ = $1; }
		| func_def
		  { $$ = $1; }
		| declaration
		  { $$ = $1; }
		| switch_command
		  { $$ = $1; }
		| if_command
		  { $$ = $1; }
		| UNWIND opt_sep opt_list CLEANUP opt_sep opt_list END
		  {
		    if (! ($$ = make_unwind_command ($1, $3, $6, $7)))
		      ABORT_PARSE;
		  }
		| TRY opt_sep opt_list CATCH opt_sep opt_list END
		  {
		    if (! ($$ = make_try_command ($1, $3, $6, $7)))
		      ABORT_PARSE;
		  }
		| WHILE expression opt_sep opt_list END
		  {
		    if (! ($$ = make_while_command ($1, $2, $4, $5)))
		      ABORT_PARSE;
		  }
		| FOR variable '=' expression opt_sep opt_list END
		  {
		    if (! ($$ = make_for_command ($1, $2, $4, $6, $7)))
		      ABORT_PARSE;
		  }
		| FOR '[' screwed_again matrix_row SCREW_TWO '='
		    expression opt_sep opt_list END
		  {
		    if (! ($$ = make_for_command ($1, $4, $7, $9, $10)))
		      ABORT_PARSE;
		  }
		| BREAK
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

screwed_again	: // empty
		  { lexer_flags.maybe_screwed_again++; }
		;

expression	: simple_expr
		  { $$ = $1; }
		| NUM '=' expression
		  {
		    yyerror ("invalid assignment to a number");
		    $$ = 0;
		    ABORT_PARSE;
		  }
		;

// Now that we do some simple constant folding, we have to make sure
// that we get something valid back make_binary_op and make_unary_op.

simple_expr	: simple_expr1
		  {
		    if (! ($$ = $1))
		      ABORT_PARSE;
		  }
		;

simple_expr1	: NUM
		  { $$ = make_constant (NUM, $1); }
		| IMAG_NUM
		  { $$ = make_constant (IMAG_NUM, $1); }
		| TEXT
		  { $$ = make_constant (TEXT, $1); }
		| '(' simple_expr ')'
		  {
		    $2->mark_in_parens ();
		    $$ = $2;
		  }
		| word_list_cmd
		  { $$ = $1; }
		| variable
		  { $$ = $1; }
		| colon_expr
		  { $$ = finish_colon_expression ($1); }
		| matrix
		  { $$ = $1; }
		| '[' ']'
		  { $$ = new tree_constant (Matrix ()); }
		| '[' ';' ']'
		  { $$ = new tree_constant (Matrix ()); }
		| PLUS_PLUS identifier %prec UNARY
		  { $$ = make_prefix_op (PLUS_PLUS, $2, $1); }
		| MINUS_MINUS identifier %prec UNARY
		  { $$ = make_prefix_op (MINUS_MINUS, $2, $1); }
		| EXPR_NOT simple_expr
		  { $$ = make_unary_op (EXPR_NOT, $2, $1); }
		| '+' simple_expr %prec UNARY
		  { $$ = $2; }
		| '-' simple_expr %prec UNARY
		  { $$ = make_unary_op ('-', $2, $1); }
		| variable '=' simple_expr
		  { $$ = make_simple_assignment ($1, $2, $3); }
		| '[' screwed_again matrix_row SCREW_TWO '=' simple_expr
		  {
		    if (! ($$ = make_multi_val_ret ($3, $6, $5)))
		      ABORT_PARSE;
		  }
		| identifier PLUS_PLUS
		  { $$ = make_postfix_op (PLUS_PLUS, $1, $2); }
		| identifier MINUS_MINUS
		  { $$ = make_postfix_op (MINUS_MINUS, $1, $2); }
		| simple_expr QUOTE
		  { $$ = make_unary_op (QUOTE, $1, $2); }
		| simple_expr TRANSPOSE
		  { $$ = make_unary_op (TRANSPOSE, $1, $2); }
		| simple_expr POW simple_expr
		  { $$ = make_binary_op (POW, $1, $2, $3); }
		| simple_expr EPOW simple_expr
		  { $$ = make_binary_op (EPOW, $1, $2, $3); }
		| simple_expr '+' simple_expr
		  { $$ = make_binary_op ('+', $1, $2, $3); }
		| simple_expr '-' simple_expr
		  { $$ = make_binary_op ('-', $1, $2, $3); }
		| simple_expr '*' simple_expr
		  { $$ = make_binary_op ('*', $1, $2, $3); }
		| simple_expr '/' simple_expr
		  { $$ = make_binary_op ('/', $1, $2, $3); }
		| simple_expr EPLUS simple_expr
		  { $$ = make_binary_op ('+', $1, $2, $3); }
		| simple_expr EMINUS simple_expr
		  { $$ = make_binary_op ('-', $1, $2, $3); }
		| simple_expr EMUL simple_expr
		  { $$ = make_binary_op (EMUL, $1, $2, $3); }
		| simple_expr EDIV simple_expr
		  { $$ = make_binary_op (EDIV, $1, $2, $3); }
		| simple_expr LEFTDIV simple_expr
		  { $$ = make_binary_op (LEFTDIV, $1, $2, $3); }
		| simple_expr ELEFTDIV simple_expr
		  { $$ = make_binary_op (ELEFTDIV, $1, $2, $3); }
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

colon_expr	: simple_expr ':' simple_expr
		  {
		    $$ = new tree_colon_expression
		      ($1, $3, $2->line (), $2->column ());
		  }
		| colon_expr ':' simple_expr
		  {
		    if (! ($$ = $1->chain ($3)))
		      ABORT_PARSE;
		  }
		;

word_list_cmd	: identifier word_list
		  {
		    $$ = new tree_index_expression
		      ($1, $2, $1->line (), $1->column ());
		  }
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

// This is truly disgusting.

g_symtab	: // empty
		  { curr_sym_tab = global_sym_tab; }
		;

in_return_list	: // empty
		  { lexer_flags.looking_at_return_list = 1; }
		;

local_symtab	: // empty
		  { curr_sym_tab = tmp_local_sym_tab; }
		;

safe		: // empty
		  { lexer_flags.maybe_screwed = 0; }
		;

are_we_screwed	: // empty
		  { lexer_flags.maybe_screwed = 1; }
		;

func_def	: FCN g_symtab are_we_screwed func_def1
		  {
		    curr_sym_tab = top_level_sym_tab;
		    lexer_flags.defining_func = 0;
		    $$ = 0;
		  }
		| FCN g_symtab are_we_screwed func_def2
		  {
		    curr_sym_tab = top_level_sym_tab;
		    lexer_flags.defining_func = 0;
		    $$ = 0;
		  }
		;

func_def1	: SCREW safe g_symtab '=' func_def2
		  { $$ = finish_function_def ($1, $5); }
		| return_list g_symtab '=' func_def2
		  { $$ = finish_function_def ($1, $4); }
		;

return_list_x	: '[' safe local_symtab in_return_list
		;

return_list	: return_list_x ']'
		  {
		    lexer_flags.looking_at_return_list = 0;
		    $$ = new tree_parameter_list ();
		  }
		| return_list_x ELLIPSIS ']'
		  {
		    lexer_flags.looking_at_return_list = 0;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| return_list1 ']'
		  {
		    lexer_flags.looking_at_return_list = 0;
		    $$ = $1;
		  }
		| return_list1 ',' ELLIPSIS ']'
		  {
		    lexer_flags.looking_at_return_list = 0;
		    $1->mark_varargs ();
		    $$ = $1;
		  }
		;

return_list1	: return_list_x identifier
		  { $$ = new tree_parameter_list ($2); }
		| return_list_x error
		  {
		    yyerror ("invalid function return list");
		    $$ = 0;
		    ABORT_PARSE;
		  }
		| return_list1 ',' identifier
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
		;

func_def2	: identifier safe local_symtab func_def3
		  {
		    if (! ($$ = frob_function_def ($1, $4)))
		      ABORT_PARSE;
		  }
		;

func_def3	: param_list opt_sep opt_list fcn_end_or_eof
		  { $$ = start_function_def ($1, $3); }
		| opt_sep opt_list fcn_end_or_eof
		  { $$ = start_function_def (0, $2); }
		;

fcn_end_or_eof	: END
		  {
		    if (check_end ($1, token::function_end))
		      ABORT_PARSE;

		    if (reading_fcn_file)
		      check_for_garbage_after_fcn_def ();
		  }
		| END_OF_INPUT
		  {
		    if (! (reading_fcn_file || reading_script_file))
		      YYABORT;
		  }
		;

indirect_ref	: indirect_ref1
		  {
		    lexer_flags.looking_at_indirect_ref = 0;
		    $$ = $1;
		  }

indirect_ref1	: identifier
		  {
		    $$ = new tree_indirect_ref ($1, $1->line (),
						$1->column ());
		  }
		| indirect_ref1 '.'
		    { lexer_flags.looking_at_indirect_ref = 1; } TEXT_ID
		  { $$ = new tree_indirect_ref ($1, $4->text ()); }
		;

variable	: indirect_ref
		  { $$ = make_index_expression ($1, 0); }
		| indirect_ref '(' ')'
		  { $$ = make_index_expression ($1, 0); }
		| indirect_ref '(' arg_list ')'
		  { $$ = make_index_expression ($1, $3); }
		| indirect_ref '['
		  {
		    yyerror ("use `(\' and `)\' as index operators, not\
 `[\' and `]\'"); 
		    $$ = 0;
		    ABORT_PARSE;
		  }
		;

param_list_beg	: '('
		  { lexer_flags.looking_at_parameter_list = 1; }
		;

param_list_end	: ')'
		  { lexer_flags.looking_at_parameter_list = 0; }
		;

param_list	: param_list_beg param_list_end
		  {
		    lexer_flags.quote_is_transpose = 0;
		    $$ = 0;
		  }
		| param_list_beg ELLIPSIS param_list_end
		  {
		    lexer_flags.quote_is_transpose = 0;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| param_list1 param_list_end
		  {
		    lexer_flags.quote_is_transpose = 0;
		    $1->mark_as_formal_parameters ();
		    $$ = $1;
		  }
		| param_list1 ',' ELLIPSIS param_list_end
		  {
		    lexer_flags.quote_is_transpose = 0;
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

identifier	: NAME
		  {
		    $$ = new tree_identifier
		      ($1->sym_rec (), $1->line (), $1->column ());
		  }
		;

arg_list	: ':'
		  {
		    tree_constant *colon =
		      new tree_constant (tree_constant::magic_colon_t);
		    $$ = new tree_argument_list (colon);
		  }
		| expression
		  { $$ = new tree_argument_list ($1); }
		| ALL_VA_ARGS
		  {
		    tree_constant *all_va_args =
		      new tree_constant (tree_constant::all_va_args_t);
		    $$ = new tree_argument_list (all_va_args);
		  }
		| arg_list ',' ':'
		  {
		    tree_constant *colon =
		      new tree_constant (tree_constant::magic_colon_t);
		    $1->append (colon);
		    $$ = $1;
		  }
		| arg_list ',' expression
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
		| arg_list ',' ALL_VA_ARGS
		  {
		    tree_constant *all_va_args =
		      new tree_constant (tree_constant::all_va_args_t);
		    $1->append (all_va_args);
		    $$ = $1;
		  }
		;

matrix		: '[' screwed_again rows ']'
		  { $$ = finish_matrix ($3); }
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

matrix_row	: matrix_row1
		  { $$ = $1; }
		| matrix_row1 ','	// Ignore trailing comma.
		  { $$ = $1; }
		;

matrix_row1	: expression		// First element on row.
		  { $$ = new tree_matrix_row ($1); }
		| matrix_row1 ',' expression
		  {
		    $1->append ($3);
		    $$ = $1;
		  }
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

static int
check_end (token *tok, token::end_tok_type expected)
{
  token::end_tok_type ettype = tok->ettype ();
  if (ettype != expected && ettype != token::simple_end)
    {
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
      return 1;
    }
  else
    return 0;
}

// Try to figure out early if an expression should become an
// assignment to the built-in variable ans.
//
// Need to make sure that the expression is not already an identifier
// that has a name, or an assignment expression.
//
// Note that an expression can not be just an identifier now -- it
// must at least be an index expression (see the definition of the
// non-terminal variable above).

static tree_expression *
maybe_convert_to_ans_assign (tree_expression *expr)
{
  if (expr->is_index_expression ())
    {
      expr->mark_for_possible_ans_assign ();
      return expr;
    }
  else if (expr->is_assignment_expression ()
	   || expr->is_prefix_expression ())
    {
      return expr;
    }
  else
    {
      // XXX FIXME XXX -- making ans_id static, passing its address to
      // tree_simple_assignment_expression along with a flag to not
      // delete it seems to create a memory leak.  Hmm.

      static symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);
      tree_identifier *ans_id = new tree_identifier (sr);

      int l = expr->line ();
      int c = expr->column ();

      return new tree_simple_assignment_expression (ans_id, expr, 0, 1, l, c);
    }
}

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.

static void
maybe_warn_assign_as_truth_value (tree_expression *expr)
{
  if (Vwarn_assign_as_truth_value
      && expr->is_assignment_expression ()
      && expr->is_in_parens () < 2)
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

  lexer_flags.plotting = 0;
  lexer_flags.past_plot_range = 0;
  lexer_flags.in_plot_range = 0;
  lexer_flags.in_plot_using = 0;
  lexer_flags.in_plot_style = 0;
  
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
      octave_value tmp = e->eval (0);

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

static tree_expression *
fold (tree_unary_expression *e)
{
  tree_expression *retval = 0;

  tree_expression *op1 = e->operand ();

  if (op1->is_constant ())
    {
      octave_value tmp = e->eval (0);

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

  if (base->is_constant () && limit->is_constant ()
      && (! incr || (incr && incr->is_constant ())))
    {
      octave_value tmp = e->eval (0);

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
	retval = new tree_constant (tok_val->number (), l, c);
	retval->stash_original_text (tok_val->text_rep ());
      }
      break;

    case IMAG_NUM:
      {
	Complex C (0.0, tok_val->number ());
	retval = new tree_constant (C, l, c);
	retval->stash_original_text (tok_val->text_rep ());
      }
      break;

    case TEXT:
      retval = new tree_constant (tok_val->text (), l, c);
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
  tree_binary_expression::type t;

  switch (op)
    {
    case POW:
      t = tree_binary_expression::power;
      break;

    case EPOW:
      t = tree_binary_expression::elem_pow;
      break;

    case '+':
      t = tree_binary_expression::add;
      break;

    case '-':
      t = tree_binary_expression::subtract;
      break;

    case '*':
      t = tree_binary_expression::multiply;
      break;

    case '/':
      t = tree_binary_expression::divide;
      break;

    case EMUL:
      t = tree_binary_expression::el_mul;
      break;

    case EDIV:
      t = tree_binary_expression::el_div;
      break;

    case LEFTDIV:
      t = tree_binary_expression::leftdiv;
      break;

    case ELEFTDIV:
      t = tree_binary_expression::el_leftdiv;
      break;

    case EXPR_LT:
      t = tree_binary_expression::cmp_lt;
      break;

    case EXPR_LE:
      t = tree_binary_expression::cmp_le;
      break;

    case EXPR_EQ:
      t = tree_binary_expression::cmp_eq;
      break;

    case EXPR_GE:
      t = tree_binary_expression::cmp_ge;
      break;

    case EXPR_GT:
      t = tree_binary_expression::cmp_gt;
      break;

    case EXPR_NE:
      t = tree_binary_expression::cmp_ne;
      break;

    case EXPR_AND:
      t = tree_binary_expression::el_and;
      break;

    case EXPR_OR:
      t = tree_binary_expression::el_or;
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
make_prefix_op (int op, tree_identifier *op1, token *tok_val)
{
  tree_prefix_expression::type t;

  switch (op)
    {
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

  return new tree_prefix_expression (op1, l, c, t);
}

// Build a postfix expression.

static tree_expression *
make_postfix_op (int op, tree_identifier *op1, token *tok_val)
{
  tree_postfix_expression::type t;

  switch (op)
    {
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

  return new tree_postfix_expression (op1, l, c, t);
}

// Build a unary expression.

static tree_expression *
make_unary_op (int op, tree_expression *op1, token *tok_val)
{
  tree_unary_expression::type t;

  switch (op)
    {
    case QUOTE:
      t = tree_unary_expression::hermitian;
      break;

    case TRANSPOSE:
      t = tree_unary_expression::transpose;
      break;

    case EXPR_NOT:
      t = tree_unary_expression::unot;
      break;

    case '-':
      t = tree_unary_expression::uminus;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_unary_expression *e
    = new tree_unary_expression (op1, l, c, t);

  return fold (e);
}

// Build an unwind-protect command.

static tree_command *
make_unwind_command (token *unwind_tok, tree_statement_list *body,
		     tree_statement_list *cleanup, token *end_tok)
{
  tree_command *retval = 0;

  if (! check_end (end_tok, token::unwind_protect_end))
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

  if (! check_end (end_tok, token::try_catch_end))
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

  if (! check_end (end_tok, token::while_end))
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
make_for_command (token *for_tok, tree_index_expression *var,
		  tree_expression *expr, tree_statement_list *body,
		  token *end_tok)
{
  tree_command *retval = 0;

  if (! check_end (end_tok, token::for_end))
    {
      lexer_flags.looping--;

      int l = for_tok->line ();
      int c = for_tok->column ();

      retval = new tree_for_command (var, expr, body, l, c);
    }

  return retval;
}

// Build a for command a different way.

static tree_command *
make_for_command (token *for_tok, tree_matrix_row *mr,
		  tree_expression *expr, tree_statement_list *body,
		  token *end_tok)
{
  tree_command *retval = 0;

  if (! check_end (end_tok, token::for_end))
    {
      lexer_flags.looping--;

      tree_return_list *id_list = mr->to_return_list ();

      int l = for_tok->line ();
      int c = for_tok->column ();

      retval = new tree_for_command (id_list, expr, body, l, c);
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

  if (! check_end (end_tok, token::if_end))
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

  if (! check_end (end_tok, token::switch_end))
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
make_simple_assignment (tree_index_expression *var, token *eq_tok,
			tree_expression *expr)
{
  int l = eq_tok->line ();
  int c = eq_tok->column ();

  return new tree_simple_assignment_expression (var, expr, 0, 0, l, c);
}

// Make an expression that handles assignment of multiple values.

static tree_expression *
make_multi_val_ret (tree_matrix_row *mr, tree_expression *rhs, token *eq_tok)
{
// Convert the matrix list to a list of identifiers.  If that fails,
// we can abort here, without losing anything -- no other possible
// syntax is valid if we've seen the equals sign as the next token
// after the `]'. 

  tree_expression *retval = 0;

  lexer_flags.maybe_screwed_again--;

  tree_return_list *id_list = mr->to_return_list ();

  if (id_list)
    {
      int list_len = id_list->length ();

      if (list_len == 1)
	{
	  tree_index_expression *lhs = id_list->remove_front ();

	  int l = eq_tok->line ();
	  int c = eq_tok->column ();

	  retval = new tree_simple_assignment_expression (lhs, rhs,
							  0, 0, l, c);
	}
      else if (list_len > 1)
	{
	  if (rhs->is_multi_val_ret_expression ())
	    {
	      tree_multi_val_ret *t = (tree_multi_val_ret *) rhs;

	      int l = eq_tok->line ();
	      int c = eq_tok->column ();

	      retval = new tree_multi_assignment_expression (id_list, t,
							     0, l, c);
	    }
	  else
	    yyerror ("RHS must be an expression that returns multiple values");
	}
      else
	panic_impossible ();
    }
  else
    yyerror ("invalid identifier list for assignment");

  return retval;
}

// Begin defining a function.

static tree_function *
start_function_def (tree_parameter_list *param_list,
		    tree_statement_list *body)
{
  body->mark_as_function_body ();

  tree_function *fcn = new tree_function (body, curr_sym_tab);

  fcn->define_param_list (param_list);

  return fcn;
}

// Do most of the work for defining a function.

static tree_function *
frob_function_def (tree_identifier *id, tree_function *fcn)
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

  symbol_record *sr = global_sym_tab->lookup (id_name, 0, 0);

  if (sr)
    fcn->stash_symtab_ptr (sr);
  else
    panic_impossible ();

  id->define (fcn);

  id->document (help_buf);

  return fcn;
}

// Finish defining a function.

static tree_function *
finish_function_def (token *var, tree_function *fcn)
{
  symbol_record *sr = var->sym_rec ();

  int l = var->line ();
  int c = var->column ();

  tree_identifier *tmp = new tree_identifier (sr, l, c);

  tree_parameter_list *tpl = new tree_parameter_list (tmp);

  tpl->mark_as_formal_parameters ();

  return fcn->define_ret_list (tpl);
}

// Finish defining a function a different way.

static tree_function *
finish_function_def (tree_parameter_list *ret_list, tree_function *fcn)
{
  ret_list->mark_as_formal_parameters ();

  return fcn->define_ret_list (ret_list);
}

// Make an index expression.

static tree_index_expression *
make_index_expression (tree_indirect_ref *indir, tree_argument_list *args)
{
  tree_index_expression *retval = 0;

  int l = indir->line ();
  int c = indir->column ();

  if (indir->is_identifier_only ())
    {
      indir->preserve_identifier ();
      retval = new tree_index_expression (indir->ident (), args, l, c);
      delete indir;
    }
  else
    retval =  new tree_index_expression (indir, args, l, c);

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

  lexer_flags.maybe_screwed_again--;

  if (m->all_elements_are_constant ())
    {
      octave_value tmp = m->eval (0);

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

static void
maybe_warn_comma_in_decl (void)
{
  if (Vwarn_comma_in_declaration)\
    {
      warning ("comma in declaration not interpreted as a command separator"); 

      if (reading_fcn_file || reading_script_file)
	warning ("near line %d of file `%s'", input_line_number,
		 curr_fcn_file_full_name.c_str ());
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
warn_comma_in_declaration (void)
{
  Vwarn_comma_in_declaration = check_preference ("warn_comma_in_declaration");

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

  DEFVAR (warn_comma_in_declaration, 1.0, 0, warn_comma_in_declaration,
    "produce warning for commas in declaration statements");

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
