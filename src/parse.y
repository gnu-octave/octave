/* parse.y						-*- text -*-

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Parser for Octave.

// C decarations.

%{
#define YYDEBUG 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <strstream.h>

#include "SLStack.h"

#include "Matrix.h"

#include "octave-hist.h"
#include "user-prefs.h"
#include "tree-base.h"
#include "tree-expr.h"
#include "tree-cmd.h"
#include "tree-const.h"
#include "tree-misc.h"
#include "variables.h"
#include "tree-plot.h"
#include "octave.h"
#include "symtab.h"
#include "parse.h"
#include "token.h"
#include "error.h"
#include "pager.h"
#include "input.h"
#include "utils.h"
#include "lex.h"

// Nonzero means we're in the middle of defining a function.
int defining_func = 0;

// Nonzero means we're in the middle of defining a loop.
int looping = 0;

// Nonzero means we're in the middle of defining a conditional expression.
int iffing = 0;

// Nonzero means we need to do some extra lookahead to avoid being
// screwed by bogus function syntax.
int maybe_screwed = 0;

// Nonzero means we need to do some extra lookahead to avoid being
// screwed by bogus function syntax.
int maybe_screwed_again = 0;

// Temporary symbol table pointer used to cope with bogus function syntax.
symbol_table *tmp_local_sym_tab = 0;

// Stack to hold list of literal matrices.
SLStack <tree_matrix *> ml;

// A nonzero element corresponding to an element of ml means we just
// started reading a new matrix.  This should probably be part of a
// new struct for matrix lists... 
SLStack <int> mlnm;

// The current input line number.
int input_line_number = 0;

// The column of the current token.
int current_input_column = 1;

// Buffer for help text snagged from function files.
// Probably shouldn't be a fixed size...
char help_buf [HELP_BUF_LENGTH];

// Nonzero means we're working on a plot command.
int plotting = 0;

// Nonzero means we've seen something that means we must be past the
// range part of a plot command.
int past_plot_range = 0;

// Nonzero means we're looking at the range part of a plot command.
int in_plot_range = 0;

// Nonzero means we're looking at the using part of a plot command.
int in_plot_using = 0;

// Nonzero means we're looking at the style part of a plot command.
int in_plot_style = 0;

// Nonzero means we're looking at an indirect reference to a structure
// element.
int looking_at_indirect_ref = 0;

// Forward declarations for some functions defined at the bottom of
// the file.

// Generic error messages.
static void yyerror (char *s);

// Error mesages for mismatched end tokens.
static void end_error (char *type, token::end_tok_type ettype, int l, int c);

// Check to see that end tokens are properly matched.
static int check_end (token *tok, token::end_tok_type expected);

// Try to figure out early if an expression should become an
// assignment to the builtin variable ans.
static tree_expression *maybe_convert_to_ans_assign (tree_expression *expr);

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.
static void maybe_warn_assign_as_truth_value (tree_expression *expr);

// Build a binary expression.
static tree_expression *make_binary_op
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

// Make an expression that handles assignment of multiple values.
static tree_expression *make_multi_val_ret
	 (tree_expression *rhs, int l = -1, int c = -1);

// Make an index expression.
static tree_index_expression *make_index_expression
	 (tree_indirect_ref *indir, tree_argument_list *args);

#define ABORT_PARSE \
  do \
    { \
      global_command = 0; \
      reset_parser (); \
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
  tree *tree_type;
  tree_expression *tree_expression_type;
  tree_constant *tree_constant_type;
  tree_matrix *tree_matrix_type;
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
  tree_global *tree_global_type;
  tree_global_init_list *tree_global_init_list_type;
  tree_global_command *tree_global_command_type;
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
%token <tok_val> LEFTDIV EMUL EDIV ELEFTDIV QUOTE TRANSPOSE
%token <tok_val> PLUS_PLUS MINUS_MINUS POW EPOW
%token <tok_val> NUM IMAG_NUM
%token <tok_val> NAME SCREW
%token <tok_val> END
%token <tok_val> PLOT
%token <tok_val> TEXT STYLE
%token <tok_val> FOR WHILE IF ELSEIF ELSE BREAK CONTINUE FUNC_RET
%token <tok_val> GLOBAL
%token <tok_val> TEXT_ID

// Other tokens.
%token LEXICAL_ERROR
%token FCN SCREW_TWO
%token ELLIPSIS
%token END_OF_INPUT
%token USING TITLE WITH COLON OPEN_BRACE CLOSE_BRACE CLEAR

// Nonterminals we construct.
%type <tree_type> input
%type <tree_expression_type> expression simple_expr simple_expr1
%type <tree_expression_type> ans_expression title
%type <tree_matrix_type> matrix
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
%type <tree_global_type> global_decl2
%type <tree_global_init_list_type> global_decl1
%type <tree_global_command_type> global_decl
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
%left '-' '+'
%left '*' '/' LEFTDIV EMUL EDIV ELEFTDIV
%left QUOTE TRANSPOSE
%left UNARY PLUS_PLUS MINUS_MINUS EXPR_NOT
%right POW EPOW

// There are 19 shift/reduce conflicts, ok?
%expect 19

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
		| error
		;

simple_list	: semi_comma
		  { $$ = 0; }
		| comma_semi
		  { $$ = 0; }
		| simple_list1
		  { $$ = $1; }
		| simple_list1 semi_comma
		  {
		    tree_statement *tmp = $1->rear ();
		    tmp->set_print_flag (0);
		  }
		| simple_list1 comma_semi
		  { $$ = $1; }
		;

simple_list1	: statement
		  { $$ = new tree_statement_list ($1); }
		| semi_comma statement
		  { $$ = new tree_statement_list ($2); }
		| comma_semi statement
		  { $$ = new tree_statement_list ($2); }
		| simple_list1 semi_comma statement
		  {
		    tree_statement *tmp = $1->rear ();
		    tmp->set_print_flag (0);
		    $1->append ($3);
		  }
		| simple_list1 comma_semi statement
		  { $1->append ($3); }
		;

semi_comma	: ';'
		| semi_comma ','
		| semi_comma ';'
		;

comma_semi	: ','
		| comma_semi ';'
		| comma_semi ','
		;

comma_nl_sep	: ','
		| '\n'
		| comma_nl_sep sep
		;

semi_sep	: ';'
		| semi_sep sep
		;

opt_list	: // empty
		  { $$ = new tree_statement_list (); }
		| list
		  { $$ = $1; }
		;

list		: list1
		  { $$ = $1; }
		| list1 comma_nl_sep
		  { $$ = $1; }
		| list1 semi_sep
		  {
		    tree_statement *tmp = $1->rear ();
		    tmp->set_print_flag (0);
		  }
		;

list1		: statement
		  {
		    beginning_of_function = 0;
		    $$ = new tree_statement_list ($1);
		  }
		| list1 comma_nl_sep statement
		  { $1->append ($3); }
		| list1 semi_sep statement
		  {
		    tree_statement *tmp = $1->rear ();
		    tmp->set_print_flag (0);
		    $1->append ($3);
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
		    if (! $2 && $1->pttype () != token::replot)
		      {
			yyerror ("must have something to plot");
			ABORT_PARSE;
		      }
		    else
		      {
			$$ = new tree_plot_command ($2, $1->pttype ());
			plotting = 0;
			past_plot_range = 0;
			in_plot_range = 0;
			in_plot_using = 0;
			in_plot_style = 0;
		      }
		  }
		| PLOT ranges plot_command1
		  {
		    if ($1->pttype () == token::replot)
		      {
			yyerror ("cannot specify new ranges with replot");
			ABORT_PARSE;
		      }
		    else
		      {
			$$ = new tree_plot_command ($3, $2, $1->pttype ());
			plotting = 0;
			past_plot_range = 0;
			in_plot_range = 0;
			in_plot_using = 0;
			in_plot_style = 0;
		      }
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
		  { $1->append ($3); }
		;

plot_command2	: expression
		  { $$ = new subplot ($1); }
		| expression plot_options
		  {
		    $2->set_data ($1);
		    $$ = $2;
		  }
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
		    in_plot_using = 0;
		    $$ = $1;
		  }
		| using1 expression
		  {
		    in_plot_using = 0;
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
		  { $$ = new subplot_style ($2->string ()); }
		| WITH STYLE expression
		  { $$ = new subplot_style ($2->string (), $3); }
		| WITH STYLE expression bogus_syntax expression
		  { $$ = new subplot_style ($2->string (), $3, $5); }
		;

bogus_syntax	: // empty
		;

ans_expression	: expression
		  { $$ = maybe_convert_to_ans_assign ($1); }
		;

global_decl	: GLOBAL global_decl1
		  {
		    $$ = new tree_global_command ($2, $1->line (),
						  $1->column ());
		  }
		;

global_decl1	: global_decl2
		  { $$ = new tree_global_init_list ($1); }
		| global_decl1 optcomma global_decl2
		  { $1->append ($3); }

global_decl2	: identifier
		  { $$ = new tree_global ($1); }
		| identifier '=' expression
		  {
		    tree_simple_assignment_expression *tmp_ass;
		    tmp_ass = new tree_simple_assignment_expression
		      ($1, $3, 0, 0, $2->line (), $2->column ());
		    $$ = new tree_global (tmp_ass);
		  }
		;

optcomma	: // empty
		| ','
		  {
		    if (user_pref.warn_comma_in_global_decl)
		      warning ("comma in global declaration not\
 interpreted as a command separator");
		  }
		;

command		: plot_command
		  { $$ = $1; }
		| func_def
		  { $$ = $1; }
		| global_decl
		  { $$ = $1; }
		| if_command
		  {
		    iffing--;
		    $$ = $1;
		  }
		| WHILE expression optsep opt_list END
		  {
		    maybe_warn_assign_as_truth_value ($2);
		    if (check_end ($5, token::while_end))
		      ABORT_PARSE;
		    looping--;
		    $$ = new tree_while_command ($2, $4, $1->line (),
						 $1->column ());
		  }
		| FOR variable '=' expression optsep opt_list END
		  {
		    if (check_end ($7, token::for_end))
		      ABORT_PARSE;
		    looping--;
		    $$ = new tree_for_command ($2, $4, $6,
					       $1->line (), $1->column ());
		  }
		| BREAK
		  {
		    if (!looping)
		      {
			yyerror ("parse error");
			error ("break: only meaningful within a `for'\
 or `while' loop");
			ABORT_PARSE;
		      }
		    $$ = new tree_break_command ($1->line (), $1->column ());
		  }
		| CONTINUE
		  {
		    if (!looping)
		      {
			yyerror ("parse error");
			error ("continue: only meaningful within a\
 `for' or `while' loop");
			ABORT_PARSE;
		      }
		    $$ = new tree_continue_command ($1->line (),
						    $1->column ());
		  }
		| FUNC_RET
		  {
		    if (!defining_func)
		      {
			yyerror ("parse error");
			error ("return: only meaningful within a function");
			ABORT_PARSE;
		      }
		    $$ = new tree_return_command ($1->line (), $1->column ());
		  }
		;

if_command	: IF if_cmd_list END
		  {
		    if (check_end ($3, token::if_end))
		      ABORT_PARSE;
		    $$ = new tree_if_command ($2, $1->line (), $1->column ());
		  }
		;

if_cmd_list	: if_cmd_list1
		  { $$ = $1 }
		| if_cmd_list1 else_clause
		  { $1->append ($2); }
		;

if_cmd_list1	: expression optsep opt_list
		  {
		    maybe_warn_assign_as_truth_value ($1);
		    tree_if_clause *t = new tree_if_clause ($1, $3);
		    $$ = new tree_if_command_list (t);
		  }
		| if_cmd_list1 elseif_clause
		  { $1->append ($2); }
		;

elseif_clause	: ELSEIF optsep expression optsep opt_list
		  {
		    maybe_warn_assign_as_truth_value ($3);
		    $$ = new tree_if_clause ($3, $5);
		  }
		;

else_clause	: ELSE optsep opt_list
		  { $$ = new tree_if_clause ($3); }
		;

optsep		: // empty
		| sep
		;

sep		: ','
		| ';'
		| '\n'
		| sep ','
		| sep ';'
		| sep '\n'
		;

screwed_again	: // empty
		  { maybe_screwed_again++; }
		;

expression	: variable '=' expression
		  { $$ = new tree_simple_assignment_expression
		      ($1, $3, 0, 0, $2->line (), $2->column ()); }
		| NUM '=' expression
		  {
		    yyerror ("parse error");
		    error ("invalid assignment to a number");
		    $$ = 0;
		    ABORT_PARSE;
		  }
		| '[' screwed_again matrix_row SCREW_TWO '=' expression
		  {
		    $$ = make_multi_val_ret ($6, $5->line (), $5->column ());

		    if (! $$)
		      ABORT_PARSE;
		  }
		| simple_expr
		  { $$ = $1; }
		;

simple_expr	: simple_expr1
		  { $$ = $1; }
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
		| simple_expr EXPR_AND_AND simple_expr
		  { $$ = make_binary_op (EXPR_AND_AND, $1, $2, $3); }
		| simple_expr EXPR_OR_OR simple_expr
		  { $$ = make_binary_op (EXPR_OR_OR, $1, $2, $3); }
		| simple_expr EXPR_AND simple_expr
		  { $$ = make_binary_op (EXPR_AND, $1, $2, $3); }
		| simple_expr EXPR_OR simple_expr
		  { $$ = make_binary_op (EXPR_OR, $1, $2, $3); }
		;

simple_expr1	: NUM
		  {
		    tree_constant *tmp = new tree_constant ($1->number ());
		    tmp->stash_original_text ($1->text_rep ());
		    $$ = tmp;
		  }
		| IMAG_NUM
		  {
		    Complex c (0.0, $1->number ());
		    tree_constant *tmp = new tree_constant (c);
		    tmp->stash_original_text ($1->text_rep ());
		    $$ = tmp;
		  }
		| TEXT
		  { $$ = new tree_constant ($1->string ()); }
		| '(' expression ')'
		  {
		    $2->in_parens++;
		    $$ = $2;
		  }
		| word_list_cmd
		  { $$ = $1; }
		| variable
		  { $$ = $1; }
		| matrix
		  { $$ = $1; }
		| '[' ']'
		  {
		    mlnm.pop ();
		    $$ = new tree_constant (Matrix ());
		  }
		| '[' ';' ']'
		  {
		    mlnm.pop ();
		    $$ = new tree_constant (Matrix ());
		  }
		| colon_expr
		  { $$ = $1; }
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
		;

colon_expr	: simple_expr ':' simple_expr
		  { $$ = new tree_colon_expression
		      ($1, $3, $2->line (), $2->column ()); }
		| colon_expr ':' simple_expr
		  {
		    $$ = $1->chain ($3);
		    if (! $$)
		      {
			yyerror ("parse error");
			ABORT_PARSE;
		      }
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
		    tree_constant *tmp = new tree_constant ($1->string ());
		    $$ = new tree_argument_list (tmp);
		  }
		| word_list TEXT
		  {
		    tree_constant *tmp = new tree_constant ($2->string ());
		    $1->append (tmp);
		  }
		;

// This is truly disgusting.

g_symtab	: // empty
		  { curr_sym_tab = global_sym_tab; }
		;

local_symtab	: // empty
		  { curr_sym_tab = tmp_local_sym_tab; }
		;

safe		: // empty
		  { maybe_screwed = 0; }
		;

are_we_screwed	: // empty
		  { maybe_screwed = 1; }
		;

func_def	: FCN g_symtab are_we_screwed func_def1
		  {
		    curr_sym_tab = top_level_sym_tab;
		    defining_func = 0;
		    $$ = 0;
		  }
		| FCN g_symtab are_we_screwed func_def2
		  {
		    curr_sym_tab = top_level_sym_tab;
		    defining_func = 0;
		    $$ = 0;
		  }
		;

func_def1	: SCREW safe g_symtab '=' func_def2
		  {
		    tree_identifier *tmp = new tree_identifier
		      ($1->sym_rec (), $1->line (), $1->column ());
		    tree_parameter_list *tpl = new tree_parameter_list (tmp);
		    tpl->mark_as_formal_parameters ();
		    $$ = $5->define_ret_list (tpl);
		  }
		| return_list g_symtab '=' func_def2
		  {
		    $1->mark_as_formal_parameters ();
		    $$ = $4->define_ret_list ($1);
		  }
		;

return_list_x	: '[' safe local_symtab
		;

return_list	: return_list_x ']'
		  { $$ = new tree_parameter_list (); }
		| return_list_x ELLIPSIS ']'
		  {
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| return_list1 ']'
		  { $$ = $1; }
		| return_list1 ',' ELLIPSIS ']'
		  {
		    $1->mark_varargs ();
		    $$ = $1;
		  }
		;

return_list1	: return_list_x identifier
		  { $$ = new tree_parameter_list ($2); }
		| return_list_x error
		  {
		    yyerror ("parse error");
		    error ("invalid function return list");
		    ABORT_PARSE;
		  }
		| return_list1 ',' identifier
		  { $1->append ($3); }
		;

func_def2	: identifier safe local_symtab func_def3
		  {
		    char *id_name = $1->name ();
//		    if (is_text_function_name (id_name))
//		      {
//			yyerror ("parse error");
//			error ("invalid use of reserved word %s", id_name);
//			ABORT_PARSE;
//		      }

// If input is coming from a file, issue a warning if the name of the
// file does not match the name of the function stated in the file.
// Matlab doesn't provide a diagnostic (it ignores the stated name).

		    $4->stash_function_name (id_name);

		    if (reading_fcn_file)
		      {
			if (strcmp (curr_fcn_file_name, id_name) != 0)
			  {
			    warning ("function name `%s' does not agree\
 with function file name `%s.m'", id_name, curr_fcn_file_name);

			    global_sym_tab->rename (id_name,
						    curr_fcn_file_name);

			    if (error_state)
			      ABORT_PARSE;

			    id_name = $1->name ();
			  }

			$4->stash_function_name (id_name);
			$4->stash_fcn_file_name ();
			$4->stash_fcn_file_time (time (0));
			$4->mark_as_system_fcn_file ();
		      }
		    else if (! (input_from_tmp_history_file
				|| input_from_startup_file)
			     && reading_script_file
			     && strcmp (curr_fcn_file_name, id_name) == 0)
		      {
			warning ("function `%s' defined within\
 script file `%s.m'", id_name, curr_fcn_file_name);
		      }

		    top_level_sym_tab->clear (id_name);

		    $1->define ($4);
		    $1->document (help_buf);

		    $$ = $4;
		  }
		;

func_def3	: param_list optsep opt_list fcn_end_or_eof
		  {
		    tree_function *fcn = new tree_function ($3, curr_sym_tab);
		    $$ = fcn->define_param_list ($1);
		  }
		| optsep opt_list fcn_end_or_eof
		  { $$ = new tree_function ($2, curr_sym_tab); }
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
		    looking_at_indirect_ref = 0;
		    $$ = $1;
		  }

indirect_ref1	: identifier
		  {
		    $$ = new tree_indirect_ref ($1, $1->line (),
						$1->column ());
		  }
		| indirect_ref1 '.' { looking_at_indirect_ref = 1; } TEXT_ID
		  { $$ = $1->chain ($4->string ()); }
		;

variable	: indirect_ref
		  { $$ = make_index_expression ($1, 0); }
		| indirect_ref '(' ')'
		  { $$ = make_index_expression ($1, 0); }
		| indirect_ref '(' arg_list ')'
		  { $$ = make_index_expression ($1, $3); }
		| indirect_ref '['
		  {
		    yyerror ("parse error");
		    error ("use `(\' and `)\' as index operators, not\
 `[\' and `]\'"); 
		    $$ = 0;
		    ABORT_PARSE;
		  }
		;

param_list	: '(' ')'
		  {
		    quote_is_transpose = 0;
		    $$ = 0;
		  }
		| '(' ELLIPSIS ')'
		  {
		    quote_is_transpose = 0;
		    tree_parameter_list *tmp = new tree_parameter_list ();
		    tmp->mark_varargs_only ();
		    $$ = tmp;
		  }
		| param_list1 ')'
		  {
		    quote_is_transpose = 0;
		    $1->mark_as_formal_parameters ();
		  }
		| param_list1 ',' ELLIPSIS ')'
		  {
		    quote_is_transpose = 0;
		    $1->mark_as_formal_parameters ();
		    $1->mark_varargs ();
		  }
		;

param_list1	: '(' identifier
		  { $$ = new tree_parameter_list ($2); }
		| param_list1 ',' identifier
		  { $1->append ($3); }
		| '(' error
		  {
		    yyerror ("parse error");
		    error ("invalid parameter list");
		    ABORT_PARSE;
		  }
		| param_list1 ',' error
		  {
		    yyerror ("parse error");
		    error ("invalid parameter list");
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
		    tree_constant *colon;
		    tree_constant::magic_colon t;
		    colon = new tree_constant (t);
		    $$ = new tree_argument_list (colon);
		  }
		| arg_list ',' ':'
		  {
		    tree_constant *colon;
		    tree_constant::magic_colon t;
		    colon = new tree_constant (t);
		    $1->append (colon);
		  }
		| expression
		  { $$ = new tree_argument_list ($1); }
		| arg_list ',' expression
		  { $1->append ($3); }
		;

matrix		: '[' screwed_again rows ']'
		  {
		    mlnm.pop ();
		    maybe_screwed_again--;
		    tree_matrix *tmp = ml.pop ();
		    $$ = tmp->reverse ();
		  }
		;

rows		: matrix_row
		| rows ';'	// Ignore trailing semicolon.
		| rows ';' matrix_row
		;

matrix_row	: expression		// First element on row.
		  {
		    if (mlnm.top ())
		      {
			mlnm.pop ();
			mlnm.push (0);
			tree_matrix *tmp = new tree_matrix
			  ($1, tree_matrix::md_none);
			ml.push (tmp);
		      }
		    else
		      {
			tree_matrix *tmp = ml.pop ();
			tmp = tmp->chain ($1, tree_matrix::md_down);
			ml.push (tmp);
		      }
		  }
		| matrix_row ','		// Ignore trailing comma.
		| matrix_row ',' expression
		  {
		    tree_matrix *tmp = ml.pop ();
		    tmp = tmp->chain ($3, tree_matrix::md_right);
		    ml.push (tmp);
		  }
		;

%%

// Generic error messages.

static void
yyerror (char *s)
{
  char *line = current_input_line;
  int err_col = current_input_column - 1;
  if (err_col == 0 && line)
    err_col = strlen (line) + 1;

// Print a message like `parse error', maybe printing the line number
// and file name.

  ostrstream output_buf;

  output_buf.form ("\n%s", s);

  if (reading_fcn_file || reading_script_file)
    output_buf.form (" near line %d of file %s.m", input_line_number,
		     curr_fcn_file_name);

  if (line)
    {
      int len = strlen (line);
      if (line[len-1] == '\n')
        {
          len--;
          line[len] = '\0';
        }
// Print the line, maybe with a pointer near the error token.
      if (err_col > len)
        output_buf.form (":\n\n  %s\n\n", line);
      else
        output_buf.form (":\n\n  %s\n  %*s\n\n", line, err_col, "^");
    }
  else
    output_buf << "\n\n";

  maybe_page_output (output_buf);
}

// Error mesages for mismatched end tokens.

static void
end_error (char *type, token::end_tok_type ettype, int l, int c)
{
  static char *fmt = "`%s' command matched by `%s' near line %d column %d";

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
// assignment to the builtin variable ans.
//
// Need to make sure that the expression isn't already an identifier
// that has a name, or an assignment expression.
//
// Note that an expression can't be just an identifier anymore -- it
// must at least be an index expression (see the definition of the
// non-terminal `variable' above).
//
// XXX FIXME XXX.  This isn't quite sufficient.  For example, try the
// command `x = 4, x' for `x' previously undefined.
//
// XXX FIXME XXX -- we should probably delay doing this until eval-time.

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
      symbol_record *sr = global_sym_tab->lookup ("ans", 1, 0);

      assert (sr);
      
      tree_identifier *ans = new tree_identifier (sr);

      return new tree_simple_assignment_expression (ans, expr, 0, 1);
    }
}

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.

static void
maybe_warn_assign_as_truth_value (tree_expression *expr)
{
  if (user_pref.warn_assign_as_truth_value
      && expr->is_assignment_expression ()
      && expr->in_parens < 2)
    {
      warning ("suggest parenthesis around assignment used as truth value");
    }
}

// Build a binary expression.

static tree_expression *
make_binary_op (int op, tree_expression *op1, token *tok_val,
		tree_expression *op2)
{
  tree_expression::type t;
  switch (op)
    {
    case POW:
      t = tree_expression::power;
      break;

    case EPOW:
      t = tree_expression::elem_pow;
      break;

    case '+':
      t = tree_expression::add;
      break;

    case '-':
      t = tree_expression::subtract;
      break;

    case '*':
      t = tree_expression::multiply;
      break;

    case '/':
      t = tree_expression::divide;
      break;

    case EMUL:
      t = tree_expression::el_mul;
      break;

    case EDIV:
      t = tree_expression::el_div;
      break;

    case LEFTDIV:
      t = tree_expression::leftdiv;
      break;

    case ELEFTDIV:
      t = tree_expression::el_leftdiv;
      break;

    case EXPR_LT:
      t = tree_expression::cmp_lt;
      break;

    case EXPR_LE:
      t = tree_expression::cmp_le;
      break;

    case EXPR_EQ:
      t = tree_expression::cmp_eq;
      break;

    case EXPR_GE:
      t = tree_expression::cmp_ge;
      break;

    case EXPR_GT:
      t = tree_expression::cmp_gt;
      break;

    case EXPR_NE:
      t = tree_expression::cmp_ne;
      break;

    case EXPR_AND_AND:
      t = tree_expression::and_and;
      break;

    case EXPR_OR_OR:
      t = tree_expression::or_or;
      break;

    case EXPR_AND:
      t = tree_expression::and;
      break;

    case EXPR_OR:
      t = tree_expression::or;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_binary_expression (op1, op2, t, l, c);
}

// Build a prefix expression.

static tree_expression *
make_prefix_op (int op, tree_identifier *op1, token *tok_val)
{
  tree_expression::type t;
  switch (op)
    {
    case PLUS_PLUS:
      t = tree_expression::increment;
      break;

    case MINUS_MINUS:
      t = tree_expression::decrement;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_prefix_expression (op1, t, l, c);
}

// Build a postfix expression.

static tree_expression *
make_postfix_op (int op, tree_identifier *op1, token *tok_val)
{
  tree_expression::type t;
  switch (op)
    {
    case PLUS_PLUS:
      t = tree_expression::increment;
      break;

    case MINUS_MINUS:
      t = tree_expression::decrement;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_postfix_expression (op1, t, l, c);
}

// Build a unary expression.

static tree_expression *
make_unary_op (int op, tree_expression *op1, token *tok_val)
{
  tree_expression::type t;
  switch (op)
    {
    case QUOTE:
      t = tree_expression::hermitian;
      break;

    case TRANSPOSE:
      t = tree_expression::transpose;
      break;

    case EXPR_NOT:
      t = tree_expression::not;
      break;

    case '-':
      t = tree_expression::uminus;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_unary_expression (op1, t, l, c);
}

// Make an expression that handles assignment of multiple values.

static tree_expression *
make_multi_val_ret (tree_expression *rhs, int l, int c) 
{
// Convert the matrix list to a list of identifiers.  If that fails,
// we can abort here, without losing anything -- no other possible
// syntax is valid if we've seen the equals sign as the next token
// after the `]'. 

  tree_expression *retval = 0;

  maybe_screwed_again--;

  tree_matrix *tmp = ml.pop ();

  tmp = tmp->reverse ();

  tree_return_list *id_list = tmp->to_return_list ();

  if (id_list)
    {
      int list_len = id_list->length ();

      if (list_len == 1)
	{
	  tree_index_expression *lhs = id_list->remove_front ();
	  retval = new tree_simple_assignment_expression (lhs, rhs, l, c);
	  
	}
      else if (list_len > 1)
	{
	  if (rhs->is_multi_val_ret_expression ())
	    {
	      tree_multi_val_ret *t = (tree_multi_val_ret *) rhs;
	      retval = new tree_multi_assignment_expression (id_list, t, l, c);
	    }
	  else
	    {
	      yyerror ("parse error");
	      error ("RHS must be an expression that can return\
 multiple values");
	    }
	}
      else
	panic_impossible ();
    }
  else
    {
      yyerror ("parse error");
      error ("invalid identifier list for assignment");
    }

  return retval;
}

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
